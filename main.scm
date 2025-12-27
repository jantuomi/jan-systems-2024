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
	(chicken port)
	(only srfi-13 string-join string-prefix?)
	srfi-18
	matchable
	json

	utils
	md-parser
	)

;; Utility: link adjacent archive posts (prev/newer, next/older)
(define (add-prev-next-to-posts db)
  ;; Build a mapping from out-path -> navigation HTML, then patch each post's
  ;; work HTML (or, if already installed, the final out HTML).
  ;;
  ;; IMPORTANT: Only include real archive posts.
  ;;
  ;; The most recent "now" page exists as both:
  ;; - /archive/now-YYYY-season (real post)
  ;; - /archive/now             (synthetic alias page)
  ;;
  ;; The alias must be excluded here, otherwise adjacency can point "previous" to
  ;; itself for the "now-*" post.
  ;;
  ;; Derive a stable "archive slug" from out-path, and de-duplicate based on it.
  ;; Relying on frontmatter `slug` is brittle because some pages don't have it.
  (define (archive-slug entry)
    (define out (lookup "out-path" entry))
    (if (not (string? out))
	#f
	(let* ((out2 (replace (format "^~A\\/" out-dir) "" out))
	       (out3 (replace "^\\/" "" out2))
	       ;; drop leading "archive/" and trailing ".html"
	       (no-archive (replace (format "^~A\\/" archive-subdir) "" out3))
	       (no-ext (replace "\\.html$" "" no-archive)))
	  (if (> (string-length no-ext) 0) no-ext #f))))

  (define (only-archive-md-with-date entry)
    (and (equal? "md" (assocdr "file-type" entry))
	 (truthy? (lookup "is-archive?" entry))
	 (truthy? (lookup "date" entry))
	 (let ((out (lookup "out-path" entry)))
	   (and (string? out)
		(irregex-search (irregex (format "^~A/~A/" out-dir archive-subdir)) out)
		(let ((s (archive-slug entry)))
		  (and (string? s)
		       (not (string=? s "now"))))))))

  (define posts (filter only-archive-md-with-date db))

  (define (dedupe-by-archive-slug entries)
    (define (go xs seen acc)
      (if (null? xs)
	  (reverse acc)
	  (let* ((e (car xs))
		 (s (archive-slug e)))
	    (if (and (string? s) (member s seen))
		(go (cdr xs) seen acc)
		(go (cdr xs)
		    (if (string? s) (cons s seen) seen)
		    (cons e acc))))))
    (go entries '() '()))

  (define posts-unique (dedupe-by-archive-slug posts))

  ;; oldest first for clean adjacency indexing, then compute neighbors
  (define (less-oldest-first a b)
    (string<? (assocdr "date" a) (assocdr "date" b)))

  (define sorted (sort posts-unique less-oldest-first))

  (define (strip-quotes s)
    (if (and (string? s)
	     (>= (string-length s) 2)
	     (string-prefix? "\"" s))
	(substring s 1 (- (string-length s) 1))
	s))

  (define (nav-link class label-before label-after entry)
    (define slug (or (archive-slug entry) ""))
    (define title (strip-quotes (or (lookup "title" entry) "")))
    (define href (format "/~A/~A" archive-subdir slug))
    (string-append
     "<a class=\"post-nav-link " class "\" href=\"" href "\">"
     (if label-before (string-append label-before " ") "")
     title
     (if label-after (string-append " " label-after) "")
     "</a>"))

  (define (nav-html prev next)
    ;; `prev` and `next` here refer to chronological neighbors:
    ;; - prev = older post
    ;; - next = newer post
    ;; Render with chevrons and align prev to the left, next to the right.
    (define prev-html
      (if prev
	  (nav-link "post-nav-prev" "‹" #f prev)
	  ""))
    (define next-html
      (if next
	  (nav-link "post-nav-next" #f "›" next)
	  ""))
    (string-append
     "<nav class=\"post-nav\" aria-label=\"Post navigation\">"
     "<div class=\"post-nav-left\">"
     prev-html
     "</div>"
     "<div class=\"post-nav-right\">"
     next-html
     "</div>"
     "</nav>"))

  (define (patch-file path nav)
    (if (and (string? path) (file-exists? path))
	(let* ((html-or-eof (with-input-from-file path (λ () (read-string #f))))
	       (html (if (eof-object? html-or-eof) "" html-or-eof))
	       (marker "<!--POST_NAV-->")
	       (patched
		;; If marker exists, replace it; otherwise, append nav before </main> if present, else append at end.
		(let ((m (irregex-search (irregex (irregex-quote marker)) html)))
		  (if m
		      (irregex-replace/all (irregex (irregex-quote marker)) html nav)
		      (let ((close-main (irregex-search (irregex "</main>") html)))
			(if close-main
			    (irregex-replace/all (irregex "</main>") html (string-append nav "</main>"))
			    (string-append html "\n" nav)))))))
	  (with-output-to-file path (λ () (print patched))))
	#f))

  ;; Iterate with indices to compute adjacent posts
  (define (loop xs prev)
    (if (null? xs)
	#f
	(let* ((curr (car xs))
	       (rest (cdr xs))
	       (next (if (null? rest) #f (car rest)))
	       ;; Sorted oldest->newest, so:
	       ;; - prev link should go to older => prev
	       ;; - next link should go to newer => next
	       ;;
	       ;; Guard against self-links (can happen if duplicates slip through):
	       ;; drop any neighbor whose derived archive slug equals the current one.
	       (curr-slug (archive-slug curr))
	       (prev* (if (and prev curr-slug (archive-slug prev) (string=? (archive-slug prev) curr-slug))
			  #f
			  prev))
	       (next* (if (and next curr-slug (archive-slug next) (string=? (archive-slug next) curr-slug))
			  #f
			  next))
	       (nav (nav-html prev* next*))
	       (work-html (lookup "work-html-path" curr))
	       (out-html (lookup "out-path" curr)))
	  ;; Prefer patching work file (before install), but if missing, patch final out file.
	  (or (patch-file work-html nav)
	      (patch-file out-html nav))
	  (loop rest curr))))

  (loop sorted #f)
  db)

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
(define linklog-json-path (or (get-environment-variable "LINKLOG_JSON_PATH")
			      "linklog.json"))

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

(define (generate-homepage db)
  ;; Replace the LATEST_POSTS placeholder inside the generated `index.html`
  ;; (created from the normal content pipeline) with HTML for the newest blog
  ;; posts from the in-memory db.
  (define (inner return)
    ;; We want posts that are:
    ;; - markdown-backed
    ;; - archive entries
    ;; - have a date for sorting
    ;; (any kind is allowed)
    (define (is-archive-md-post-with-date? entry)
      (and (equal? "md" (assocdr "file-type" entry))
	   (truthy? (lookup "is-archive?" entry))
	   (truthy? (lookup "date" entry))))

    (define posts (filter is-archive-md-post-with-date? db))

    ;; newest first
    (define (less? a b)
      (string>? (assocdr "date" a) (assocdr "date" b)))

    (define sorted (sort posts less?))

    ;; take first n (non-destructive)
    (define (take n xs)
      (if (or (<= n 0) (null? xs))
	  '()
	  (cons (car xs) (take (- n 1) (cdr xs)))))

    (define latest (take 3 sorted))

    ;; Strip quotes from frontmatter fields like `"Title"`
    (define (strip-quotes s)
      (if (and (string? s)
	       (>= (string-length s) 2)
	       (string-prefix? "\"" s))
	  (substring s 1 (- (string-length s) 1))
	  s))

    (define (post->html-li post)
      (define title (strip-quotes (or (lookup "title" post) "")))
      (define date (or (lookup "date" post) ""))
      (define slug (or (lookup "slug" post) ""))
      (define link (format "/~A/~A" archive-subdir slug))
      (format "<li class=\"archive-entry\"><a href=\"~A\">~A</a><small>(~A)</small></li>"
	      link title date))

    (define latest-lis (map post->html-li latest))
    (define latest-html
      (string-append
       "<section class=\"latest-posts\">"
       "<h2>Latest posts</h2>"
       "<ul class=\"archive-list\">"
       (string-join latest-lis "\n")
       "</ul>"
       "</section>"))

    ;; Patch the final installed homepage HTML (out/index.html). This is more
    ;; robust than patching the work file because install-output copies files
    ;; into `out/` after pandoc has run.
    (define out-index-path (make-pathname (list out-dir) "index.html"))
    (if (not (and (string? out-index-path) (file-exists? out-index-path)))
	(return db)
	#f)

    (define html-or-eof
      (with-input-from-file out-index-path (λ () (read-string #f))))
    (define html (if (eof-object? html-or-eof) "" html-or-eof))

    (define placeholder "LATEST_POSTS")
    (define placeholder-irx (irregex placeholder))
    (define m (irregex-search placeholder-irx html))

    (define replaced
      (if m
	  ;; Use irregex-replace/all directly to avoid any module/import ambiguity
	  ;; around `replace-all` at runtime.
	  (irregex-replace/all placeholder-irx html latest-html)
	  html))

    (with-output-to-file out-index-path
      (λ () (print replaced)))

    db)

  (call/cc inner))

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
    (define title-raw (assocdr "title" post))
    ;; strip quotes
    (define title (if (string-prefix? "\"" title-raw) (substring title-raw 1 (- (string-length title-raw) 1)) title-raw))
    (printf "\"~A\"~%" title)
    (define date (assocdr "date" post))
    (define kind (assocdr "kind" post))
    (define slug (assocdr "slug" post))
    (define link (format "/~A/~A" archive-subdir slug))
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

(define (generate-linklog db)
  (printf "[info] generating linklog page\n")
  (define json-str (with-input-from-file linklog-json-path
		 (λ () (read-string #f))))
  (define json (call-with-input-string json-str json-read))
  (define posts (map vector->list (assocdr "posts" (vector->list json))))

  (define (to-li item)
    (define title (assocdr "description" item))
    (define link (assocdr "href" item))
    (define date (assocdr "time" item))
    (format "<li class=\"archive-entry\"><a href=\"~A\">~A</a><small>(~A)</small></li>"
	    link title date))

  (define link-lis (map to-li posts))
  (define ul (string-append "<ul class=\"archive-list\">"
			    (string-join link-lis "\n")
			    "</ul>"))

  (define out-md (string-append "---\n"
				"title: linklog - jan's garden\n"
				"hide-body-title: defined\n"
				"---\n"
				"# Linklog\n\n"
				"My public bookmarks are stored in [Linkhut](https://ln.ht/~jant). This page contains a summary of the most recent updates. Updated hourly.\n\n"
				"Follow my full Linkhut feed via [RSS](https://ln.ht/_/feed/~jant) ([Huh?](https://aboutfeeds.com/)).\n\n"
				"See the blogs I follow with [dynamic OPML](/files/blogroll.opml) ([explanation](https://opml.org/)). Updated daily.\n\n"
				ul))

  (define work-md-path (make-pathname work-dir "_linklog.md"))
  (define work-html-path (make-pathname work-dir "_linklog.html"))
  (define out-path (make-pathname (list out-dir) "linklog.html"))

  (with-output-to-file work-md-path
    (λ () (print out-md)))

  (define linklog-entry (list `("file-type" . "md")
			      `("src-path" . ,work-md-path)
			      `("work-md-path" . ,work-md-path)
			      `("work-html-path" . ,work-html-path)
			      `("out-path" . ,out-path)))

  (cons linklog-entry db)
  )

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
      (@ generate-linklog)
      (@ apply-pandoc-to-md-files)
      (@ install-output)
      (@ add-prev-next-to-posts)
      (@ generate-homepage))

(printf "[info] done.~%")
