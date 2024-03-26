(module feed (generate-feed)
  (import scheme
	  (chicken base)
	  (chicken format)
	  atom)

  (define (generate-feed archive-dir)
    ;; TODO generate feed with https://wiki.call-cc.org/eggref/5/atom or pandoc-rss
    (printf "TODO generate-feed \"~A\"~%" archive-dir)))
