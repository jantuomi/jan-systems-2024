(import (chicken base)
	(chicken io)
	(chicken file posix)
        (chicken file)
	(chicken string)
	(chicken pathname)
	(chicken format)
	(chicken process-context)
	(chicken process)
	matchable

	utils
	feed
	)

; application

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
			      (λ (p) (irregex-replace (format "^~A" src-dir) p out-dir))
			      (λ (p) (irregex-replace "\.md$" p ".html"))))
  (printf " to \"~A\"~%" out-html-file)
  (pandoc-md-to-html md-path out-html-file))

(define (pandoc-md-to-html md-path html-path)
  (define args (list md-path
		     "-o" html-path
		     "--standalone"
		     "--template" template-path
		     "--highlight-style" "pygments"))
  (process "pandoc" args))

(define (process-other-file path)
    (printf "[info] copying other file \"~A\"" path)
    (define out-path (replace (format "^~A" src-dir) out-dir path))
    (printf " to \"~A\"~%" out-path)
    (copy-file path out-path))

(define (process-dir dir-path)
  (printf "[info] processing directory ~A~%" dir-path)
  (define out-dir-path (replace (format "^~A" src-dir) out-dir dir-path))
  (create-directory out-dir-path #t)
  (define paths (pipe dir-path
		      (@ format "~A/*")
		      (@ glob)
		      (@ map (@ replace "^\./" ""))))

  (for-each process-path paths))

(define (clean-dir dir)
  (define (delete path)
    (if (directory? path)
	(delete-directory path #t)
	(delete-file path)))
  (for-each (@ delete) (glob (format "~A/*" dir))))

; run

(printf "[info] cleaning up output directory \"~A\"~%" out-dir)
(clean-dir out-dir)
(printf "[info] generating site with pages from \"~A\"~%" src-dir)
(process-dir src-dir)
(printf "[info] generating feed~%")
(generate-feed feed-dir)
(printf "[info] done.~%")
