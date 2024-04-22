(module md-parser (read-md-frontmatter)
   (import scheme
	  (chicken base)
	  (chicken format)
	  (chicken file)
	  (chicken io)
	  (chicken irregex)
	  (chicken sort)
	  srfi-1
	  srfi-13

	  utils
	  )
   
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

   (define (parse-line line)
    (define irx (format "^(.+?): (.+?)$" line))
    (define m (irregex-search irx line))
    (if m
	(cons (submatch m 1) (submatch m 2))
	#f))

  (define (read-md-frontmatter md-path)
    (define lines (with-input-from-file md-path
		    (λ () (read-lines))))

    (define (inner return)
      (if (not (string=? "---" (car lines)))
	  (return #f))

      (define fm (collect-fm-lines (cdr lines)))
      (map parse-line fm))

    (call/cc (@ inner))))
