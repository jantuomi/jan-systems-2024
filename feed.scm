(module feed (generate-feed generate-archive-index)
  (import scheme
	  (chicken base)
	  (chicken format)
	  (chicken file)
	  (chicken io)
	  (chicken process)
	  (chicken pathname)
	  (chicken irregex)
	  (chicken sort)
	  srfi-1
	  srfi-13
	  utils)

  (define (generate-feed src-dir archive-subdir out-dir)
    (define archive-dir (make-pathname src-dir archive-subdir))
    (define paths (glob (format "~A/*" archive-dir)))
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
    (define out-path (make-pathname out-dir "feed.xml"))
    (printf "[info] writing RSS feed to \"~A\"~%" out-path)
    (with-output-to-file out-path
      (λ () (print output))))

  (define (collect-fm-lines lines #!optional (acc '()))
    (if (string=? "---" (car lines))
	acc
	(collect-fm-lines (cdr lines) (cons (car lines) acc))))

  (define (extract-field field fm) (call/cc (@ extract-field* field fm)))
  (define (extract-field* field fm return)
    (define (search line)
      (define irx (format "^~A: (.+?)$" field))
      (define m (irregex-search irx line))
      (if m
	  (return (submatch m))))

    (for-each search fm)
    #f)

  (define (read-md-frontmatter md-path)
    (define lines (with-input-from-file md-path
		    (λ () (read-lines))))

    (define (inner return)
      (if (not (string=? "---" (car lines)))
	  (return #f))

      (define fm (collect-fm-lines (cdr lines)))
      `((title . ,(extract-field "title" fm))
	(date  . ,(extract-field "date" fm))
	(kind  . ,(extract-field "kind" fm))))

    (call/cc (@ inner)))

  (define (generate-archive-index src-dir archive-subdir)
    (define archive-dir (make-pathname src-dir archive-subdir))
    (printf "debug archive-dir: ~A~%" archive-dir)
    (printf "debug glob: ~A~%" (glob (format "~A/*" archive-dir)))
    (define paths (pipe (glob (format "~A/*" archive-dir))
			(@ filter (@ irregex-search "\\.md$"))))

    (define fm (map (λ (path) (cons path (read-md-frontmatter path))) paths))
    (for-each (@ printf "~A~%") fm)
    (printf "debug fm: ~A~%" fm)
    (printf "debug list?: ~A~%" (list? fm))
    ;; newest first
    (define (less? a b)
      (string>? (assocdr 'date (cdr a)) (assocdr 'date (cdr b))))

    (define fm-sorted (sort fm less?))

    (define (to-li pair)
      (printf "[info] indexing page ~A~%" pair)
      (define path (car pair))
      (define title (assocdr 'title (cdr pair)))
      (define date (assocdr 'date (cdr pair)))
      (define kind (assocdr 'kind (cdr pair)))
      (define link (pipe path
			 (@ replace (format "^~A\\/?" src-dir) "/")
			 (@ replace "\\.md$" ".html")))
      (format "* [~A (~A) [~A]](~A)" title date kind link))

    (define index-lis (map to-li fm-sorted))
    (define out-md (string-append "---\n"
				  "title: Archive\n"
				  "hide-body-title: defined\n"
				  "---\n"
				  "# Archive\n\n"
				  (string-join index-lis "\n")))

    (with-output-to-file (make-pathname archive-dir "index.md")
      (λ () (print out-md))))
  )
