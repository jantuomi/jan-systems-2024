(load "utils.scm")
(load "feed.scm")

(import (chicken base)
	(chicken io)
	(chicken file posix)
        (chicken file)
	(chicken string)
	(chicken pathname)
	(chicken format)
	(chicken process-context)
	(chicken process)
	srfi-18
	matchable

	utils
	feed
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

(define md-work-dir (create-temporary-directory))
(define html-work-dir (create-temporary-directory))

(assert (> (string-length src-dir) 0))
(assert (> (string-length out-dir) 0))
(assert (> (string-length template-path) 0))

(define (process-path path)
  (match path
    [(? is-md?) (process-md path)]
    [(? directory?) (process-dir (format "~A" path))]
    [other (process-other-file other)]))

(define (is-md? path)
  (define parts (string-split path "."))
  (string=? "md" (last parts)))

(define (process-md md-path)
  (printf "[info] converting markdown file \"~A\"" md-path)
  (define out-html-file (pipe md-path
			      (@ replace (format "^~A" md-work-dir) html-work-dir)
			      (@ replace "\\.md$" ".html")))
  (printf " to \"~A\"~%" out-html-file)
  (define preprocessed-md-path (preprocess-md md-path))
  (pandoc-md-to-html preprocessed-md-path out-html-file))

(define (preprocess-md md-path)
  (define md (with-input-from-file md-path
	       (λ () (read-string #f))))

  (define cwd (decompose-pathname md-path))

  ;; Matches markdown links
  (define irx "\\[(.+?)\\]\\((.+?)\\)")
  (define (link-replace m)
    (define alt (submatch m 1))
    (define link (submatch m 2))

    ;; if link is absolute or external, leave as is.
    ;; otherwise, normalize it to the current directory context
    (define ret-link
      (if (or (substring=? "https://" link) (substring=? "/" link))
	  link
	  ;; else
	  (normalize-pathname (make-pathname cwd link))))

    ;; replace src-dir prefix with webroot prefix
    (define rooted (replace (format "^~A\\/?" md-work-dir) "/" ret-link))
    (format "[~A](~A)" alt rooted))

  (define result (replace-all irx link-replace md))

  (define out-file (create-temporary-file ".md"))
  (with-output-to-file out-file
    (λ () (print result)))

  out-file)

(define (pandoc-md-to-html md-path html-path)
  (define args (list md-path
		     "-o" html-path
		     "--standalone"
		     "--template" template-path
		     "--highlight-style" "pygments"))
  (define output-port (process "pandoc" args))
  ;; read and discard output in order to wait for completion
  (read-string #f output-port)
  #f)

(define (process-other-file path)
  (printf "[info] copying other file \"~A\"" path)
  (define out-path (replace (format "^~A" md-work-dir) html-work-dir path))
  (printf " to \"~A\"~%" out-path)
  (copy-file path out-path))

(define (process-dir dir)
  (printf "[info] processing directory \"~A\"~%" dir)
  (define out-dir-path (replace (format "^~A" md-work-dir) html-work-dir dir))
  (create-directory out-dir-path #t)
  (define paths (pipe dir
		      (@ format "~A/*")
		      (@ glob)
		      (@ map (@ replace "^\\.\\/" ""))))

  (for-each process-path paths))

(define (clean-dir dir)
  (define (delete path)
    (if (directory? path)
	(delete-directory path #t)
	(delete-file path)))
  (for-each (@ delete) (glob (format "~A/*" dir))))

(define (copy-directory from to)
  (read-string #f (process "cp" (list "-r" from to))))

(define (copy-all from to)
  (define args (append (list "-r")
		       (glob (format "~A/*" from))
		       (list to)))
  (read-string #f (process "cp" args)))

;; run

(printf "[info] copying pages to Markdown work directory \"~A\"~%" md-work-dir)
(copy-all src-dir md-work-dir)
(printf "[info] generating feed from archive subdirectory \"~A\"~%" archive-subdir)
(generate-feed md-work-dir archive-subdir html-work-dir)
(printf "[info] generating archive index.md~%")
(generate-archive-index md-work-dir archive-subdir)
(printf "[info] generating site with pages from \"~A\"~%" md-work-dir)
(process-dir md-work-dir)
(printf "[info] copying static files from \"~A\"~%" static-dir)
(copy-directory static-dir (make-pathname html-work-dir static-dir))
(printf "[info] cleaning up output directory \"~A\"~%" out-dir)
(clean-dir out-dir)
(printf "[info] copying output files from work dir to out dir \"~A\"~%" out-dir)
(copy-all html-work-dir out-dir)
(printf "[info] removing temporary directories~%")
(delete-directory md-work-dir #t)
(delete-directory html-work-dir #t)
(printf "[info] done.~%")
