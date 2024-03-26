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
(define template-path (or (get-environment-variable "PANDOC_TEMPLATE")
			  "template.html"))
(define feed-dir (or (get-environment-variable "FEED_DIR")
		     (make-pathname src-dir "archive")))

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
			      (@ replace (format "^~A" src-dir) out-dir)
			      (@ replace "\\.md$" ".html")))
  (printf " to \"~A\"~%" out-html-file)
  (define preprocessed-md-path (preprocess-md md-path))
  (pandoc-md-to-html preprocessed-md-path out-html-file))

(define (preprocess-md md-path)
  (define md (with-input-from-file md-path
	       (λ () (read-string #f))))

  (define cwd (decompose-pathname md-path))

  ;;(define wiki-image-irx "\\!\\[\\[(.+?)\\]\\]")
  ;;(define (wiki-image-replace m) (format "![](/files/~A)" (submatch m)))
  ;;(define result (pipe md (@ replace-all wiki-image-irx wiki-image-replace)))

  ;; Matches markdown links
  (define irx "\\[(.+?)\\]\\((.+?)\\)")
  (define (link-replace m)
    (define alt (submatch m 1))
    (define link (submatch m 2))

    ;; if link is absolute or external, leave as is.
    ;; otherwise, normalize it to the current context
    (define ret-link
      (if (or (substring=? "https://" link) (substring=? "/" link))
	  link
	  ;; else
	  (normalize-pathname (make-pathname cwd link))))

    (define rooted (replace (format "^~A\\/?" src-dir) "/" ret-link))
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
  (define out-path (replace (format "^~A" src-dir) out-dir path))
  (printf " to \"~A\"~%" out-path)
  (copy-file path out-path))

(define (process-dir dir-path)
  (printf "[info] processing directory \"~A\"~%" dir-path)
  (define out-dir-path (replace (format "^~A" src-dir) out-dir dir-path))
  (create-directory out-dir-path #t)
  (define paths (pipe dir-path
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

;; run

(printf "[info] generating feed from directory \"~A\"~%" feed-dir)
(generate-feed feed-dir out-dir)
(printf "[info] cleaning up output directory \"~A\"~%" out-dir)
(clean-dir out-dir)
(printf "[info] generating site with pages from \"~A\"~%" src-dir)
(process-dir src-dir)
(printf "[info] done.~%")
