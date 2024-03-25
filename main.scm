(import (chicken base)
	(chicken io)
	(chicken file posix)
        (chicken file)
	(chicken string)
	(chicken pathname)
	(chicken format)
	(chicken process-context)
	(chicken irregex)
	(chicken process)
	(matchable))

; utils

(define-syntax @
  (syntax-rules ()
    ((_ fn-body expr ...)
     (lambda (x) (fn-body expr ... x)))))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define (pipe x . fns)
  (match fns
    [() x]
    [(fn . rest) (apply pipe (fn x) rest)]))

(define (flow . fns)
  (lambda (x) (apply pipe x fns)))

; application

(define src-dir (or (get-environment-variable "SRC_DIR")
		    "pages"))
(define out-dir (or (get-environment-variable "OUT_DIR")
		    "out"))
(define template-path (or (get-environment-variable "PANDOC_TEMPLATE")
			  "template.html"))

(define (process-path path)
  (match path
    [(? is-md?) (process-md path)]
    [(? directory?) (process-dir (format "~A" path))]
    [other (printf "TODO other file ~A~%" path)]))

(define (is-md? path)
  (define parts (string-split path "."))
  (string=? "md" (last parts)))

(define (process-md md-path)
  (printf "[info] processing markdown file ~A~%" md-path)
  (define out-html-file (pipe md-path
			      (lambda (p) (irregex-replace (format "^~A" src-dir) p out-dir))
			      (lambda (p) (irregex-replace "\.md$" p ".html"))))

  (define-values (html-dir html-filename html-ext) (decompose-pathname out-html-file))
  (create-directory html-dir #t)
  (pandoc-md-to-html md-path out-html-file))

(define (pandoc-md-to-html md-path html-path)
  (define args (list md-path
		     "-o" html-path
		     "--standalone"
		     "--template" template-path
		     "--highlight-style" "pygments"))
  (process "pandoc" args))

(define (process-dir dir-path)
  (printf "[info] processing directory ~A~%" dir-path)
  (define out-dir-path (irregex-replace (format "^~A" src-dir) out-dir))
  (create-directory out-dir-path #t)
  (define paths (glob (format "~A/*" dir-path)))
  (define paths (map (lambda (p) (irregex-replace "^\./" p "")) paths))
  (for-each process-path paths))

(define (clean-dir dir)
  (define (delete path)
    (if (directory? path)
	(delete-directory path #t)
	(delete-file path)))
  (for-each (@ delete) (glob (format "~A/*" dir))))

; run

(printf "[info] cleaning up output directory (~A)~%" out-dir)
(clean-dir out-dir)
(printf "[info] generating site with pages from (~A)~%" src-dir)
(process-dir src-dir)
(printf "[info] done.~%")

;; TODO generate feed with https://wiki.call-cc.org/eggref/5/atom
