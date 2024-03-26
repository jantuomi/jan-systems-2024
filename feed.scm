(module feed (generate-feed)
  (import scheme
	  (chicken base)
	  (chicken format)
	  (chicken file)
	  (chicken io)
	  (chicken process)
	  (chicken pathname)
	  utils)

  (define (generate-feed archive-dir out-dir)
    (define paths (glob (format "~A/*" archive-dir)))
    (define output-port
      (process "pandoc-rss"
	       (apply list "-s"
		      "-t" "jan's garden"
		      "-d" "RSS feed for Jan's personal digital garden"
		      "-l" "https://jan.systems"
		      "-f" "%s"
		      "-n" "en-GB"
		      "-c" "CC BY-SA 4.0"
		      "-w" "https://jan.systems"
		      paths)))
    (define output (read-string #f output-port))
    (with-output-to-file (make-pathname out-dir "feed.xml")
      (λ () (print output)))))
