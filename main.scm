(load "utils.scm")
(load "md-parser.scm")

(import (chicken base)
	(chicken io)
	(chicken file posix)
        (chicken file)
	(chicken string)
	(chicken pathname)
	(chicken format)
	(chicken process-context)
	(chicken process)
	(chicken irregex)
	(chicken sort)
	(only srfi-13 string-join)
	srfi-18
	matchable

	utils
	md-parser
	)

;; application

(define src-dir (or (get-environment-variable "SRC_DIR")
		    "pages"))
(define out-dir (or (get-environment-variable "OUT_DIR")
		    "out"))
(define template-path (or (get-environment-variable "TEMPLATE_PATH")
			  "template.html"))
(define archive-subdir (or (get-environment-variable "ARCHIVE_SUBDIR")
			   "archive"))
(define static-dir (or (get-environment-variable "STATIC_DIR")
		       "static"))

(define archive-index-md-path (create-temporary-file ".md"))
(define feed-xml-path (create-temporary-file ".xml"))
(define work-dir (create-temporary-directory))

(assert (> (string-length src-dir) 0))
(assert (> (string-length out-dir) 0))
(assert (> (string-length template-path) 0))

(define (preprocess-md md-path out-path)
  (define md-or-eof (with-input-from-file md-path
		      (λ () (read-string #f))))

  (define md (if (eof-object? md-or-eof)
		 (begin (printf "[warn] \"~A\" is empty, using an empty md file...~%" md-path)
			"")
		 ;; else
		 md-or-eof))

  (define cwd (decompose-pathname md-path))
  (define out-path-dir (decompose-pathname out-path))
  (create-directory out-path-dir #t)

  ;; Matches markdown links
  (define irx "\\[(.+?)\\]\\((.+?)\\)")
  (define (link-replace m)
    (define alt (submatch m 1))
    (define link (submatch m 2))

    ;; if link is absolute, external or in-page, leave as is.
    ;; otherwise, normalize it to the current directory context
    (define ret-link
      (if (or (substring=? "https://" link)
	      (substring=? "/" link)
	      (substring=? "#" link))
	  link
	  ;; else
	  (normalize-pathname (make-pathname cwd link))))

    ;; replace src-dir prefix with webroot prefix
    (define rooted (replace (format "^~A\\/?" src-dir) "/" ret-link))
    (format "[~A](~A)" alt rooted))

  (define result (replace-all irx link-replace md))

  (with-output-to-file out-path
    (λ () (print result))))

(define (preprocess-md-files db)
  (define (is-md? entry)
    (string=? "md" (assocdr "file-type" entry)))

  (define (act entry)
    (if (is-md? entry) (preprocess-md (assocdr "src-path" entry)
				      (assocdr "work-md-path" entry))))

  (for-each act db)

  db)

(define (add-static-to-db db)
  (define paths (find-files static-dir))
  (define new-entries '())
  (define (act src-path)
    (define out-path (replace (format "^~A" static-dir)
			      (make-pathname out-dir "static")
			      src-path))
    (set! new-entries
      (cons (list `("file-type" . "other")
		  `("src-path" . ,src-path)
		  `("out-path" . ,out-path))
	    new-entries)))

  (for-each act paths)
  (append new-entries db))

(define (add-now-page-to-db db)
  (define (inner return)
    (define (find-newest-now-page newest entries)
      (if (null? entries)
	  newest
	  (let* ((head (car entries))
		 (tail (cdr entries))
		 (head-file-type (lookup "file-type" head))
		 (head-date (lookup "date" head))
		 (head-kind (lookup "kind" head))
		 (newest-date (lookup "date" newest)))

	    (if (and head-date
		     (equal? head-file-type "md")
		     (truthy? (lookup "is-archive?" head))
		     (equal? head-kind "now")
		     (or (not newest)
			 (string>? head-date newest-date)))
		(find-newest-now-page head tail)
		(find-newest-now-page newest tail)))
	  ))

    (define newest (find-newest-now-page #f db))
    (if (not newest) (return db))

    (define out-path (format "~A/now.html" out-dir))

    (define now-page (apply list
			    `("out-path" . ,out-path)
			    `("hide-body-title" . "defined")
			    newest))

    (cons now-page db))

  (call/cc inner))

(define (is-md-path? path)
  (truthy? (irregex-match ".*?\\.md$" path)))

(define (construct-db-from path)
  (match path
    [(? is-md-path?) (list (construct-db-md path))]
    [(? directory?) (construct-db-dir path)]
    [_ (list (construct-db-other path))]))

(define (construct-db-md path)
  (define fm (or (read-md-frontmatter path) '()))
  (define-values (_ slug _) (decompose-pathname path))
  (define work-md-file (pipe path (@ replace (format "^~A" src-dir) work-dir)))
  (define work-html-file (replace "\\.md$" ".html" work-md-file))
  (define out-file (pipe path (@ replace (format "^~A" src-dir) out-dir)
			 (@ replace "\\.md$" ".html")))
  (define is-archive? (pipe path
			    (@ irregex-match (format "^~A/~A.*" src-dir archive-subdir))
			    (λ (m) (not (eq? #f m)))))

  (apply list
	 `("file-type" . "md")
	 `("src-path" . ,path)
	 `("work-md-path" . ,work-md-file)
	 `("work-html-path" . ,work-html-file)
	 `("out-path" . ,out-file)
	 `("slug" . ,slug)
	 `("is-archive?" . ,is-archive?)
	 fm))

(define (construct-db-dir path)
  (define paths (pipe path
		      (@ format "~A/*")
		      (@ glob)
		      (@ map (@ replace "^\\.\\/" ""))))

  (flatmap construct-db-from paths))

(define (construct-db-other path)
  (define out-path (replace (format "^~A" src-dir) out-dir path))

  `(("file-type" . "other")
    ("src-path" . ,path)
    ("out-path" . ,out-path)))

(define (apply-pandoc-to-md-files db)
  (define (is-md? entry)
    (equal? "md" (assocdr "file-type" entry)))

  (define posts (filter is-md? db))

  (define (act post)
    (define from (assocdr "work-md-path" post))
    (define to (assocdr "work-html-path" post))
    (pandoc-md-to-html from to))

  (for-each act posts)

  db)

(define (pandoc-md-to-html md-path html-path)
  (define args (list md-path
		     "-o" html-path
		     "--standalone"
		     "--template" template-path
		     ;; any style is ok here, the styles are overriden with CSS
		     ;; to support dark and light themes.
		     ;; --no-highlight would produce a non-formatted output
		     "--highlight-style" "pygments"))
  (define output-port (process "pandoc" args))
  ;; read and discard output in order to wait for completion
  (read-string #f output-port)
  #f)

(define (clean-dir dir)
  (define (delete path)
    (if (directory? path)
	(delete-directory path #t)
	(delete-file path)))
  (for-each (@ delete) (glob (format "~A/*" dir))))

(define (move-all-to-out-dir db out-dir)
  (define (move-entry entry)
    (define file-type (assocdr "file-type" entry))
    (define from
      (match file-type
	["md" (assocdr "work-html-path" entry)]
	[other (assocdr "src-path" entry)]))
    (define to (assocdr "out-path" entry))

    (define to-dir (decompose-pathname to))
    (create-directory to-dir #t)

    (read-string #f (process "cp" (list from to))))

  (for-each move-entry db))

(define (generate-feed db)
  (define (is-archive-post? post) (assocdr "is-archive?" post))
  (define paths (pipe db
		      (@ filter (@ assocdr "is-archive?"))
		      (@ map (@ assocdr "work-md-path"))))

  (define link-format (format "~A/%s" archive-subdir))
  (define output-port
    (process "vendor/bin/pandoc-rss"
	     (apply list "-s"
		    "-t" "jan's garden"
		    "-d" "RSS feed for Jan's personal digital garden"
		    "-l" "https://jan.systems"
		    "-f" link-format
		    "-n" "en-GB"
		    "-c" "CC BY-SA 4.0"
		    "-w" "https://jan.systems"
		    paths)))
  (define output (read-string #f output-port))
  (printf "[info] writing RSS feed...~%")
  (with-output-to-file feed-xml-path
    (λ () (print output)))

  (define entry (list `("file-type" . "other")
		      `("src-path" . ,feed-xml-path)
		      `("out-path" . ,(make-pathname out-dir "feed.xml"))))

  (cons entry db))

(define (generate-archive-index db)
  (define (is-archive-post? post) (assocdr "is-archive?" post))
  (define posts (filter (@ assocdr "is-archive?") db))

  ;; newest first
  (define (less? a b)
    (string>? (assocdr "date" (cdr a)) (assocdr "date" (cdr b))))

  (define posts-sorted (sort posts less?))

  (define (to-li post)
    (printf "[info] indexing post ")
    (define title (assocdr "title" post))
    (printf "\"~A\"~%" title)
    (define date (assocdr "date" post))
    (define kind (assocdr "kind" post))
    (define slug (assocdr "slug" post))
    (define link (format "~A/~A" archive-subdir slug))
    (format "<li class=\"archive-entry\"><a href=\"~A\">~A</a><small><span>[~A]</span> (~A)</small></li>"
	    link title kind date))

  (define index-lis (map to-li posts-sorted))
  (define out-md (string-append "---\n"
				"title: archive – jan's garden\n"
				"hide-body-title: defined\n"
				"---\n"
				"# Archive\n\n"
				"Follow via [RSS](/feed.xml) ([Huh?](https://aboutfeeds.com/))\n\n"
				(string-append "<ul class=\"archive-list\">"
					       (string-join index-lis "\n")
					       "</ul>")))

  (define work-md-path (make-pathname work-dir "_archive-index.md"))
  (define work-html-path (make-pathname work-dir "_archive-index.html"))
  (define out-path (make-pathname (list out-dir archive-subdir) "index.html"))

  (with-output-to-file work-md-path
    (λ () (print out-md)))

  (define archive-entry (list `("file-type" . "md")
			      `("src-path" . ,work-md-path)
			      `("work-md-path" . ,work-md-path)
			      `("work-html-path" . ,work-html-path)
			      `("out-path" . ,out-path)))

  (cons archive-entry db))

(define (install-output db)
  (clean-dir out-dir)
  (move-all-to-out-dir db out-dir)
  (delete-directory work-dir #t)

  db)

;; run

(printf "[info] building...~%")

(pipe src-dir
      (@ construct-db-from)
      (@ add-static-to-db)
      (@ generate-archive-index)
      (@ add-now-page-to-db)
      (@ preprocess-md-files)
      (@ generate-feed)
      (@ apply-pandoc-to-md-files)
      (@ install-output))

(printf "[info] done.~%")
