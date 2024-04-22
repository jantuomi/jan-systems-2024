(module utils *
  (import scheme
	  (chicken base)
	  (chicken irregex)
	  matchable)

  (define-syntax λ
    (syntax-rules ()
      ((_ expr ...)
       (lambda expr ...))))

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

  (define (replace irx replacement in)
    (irregex-replace irx in replacement))

  (define (replace-all irx replacement in)
    (irregex-replace/all irx in replacement))

  (define (submatch m #!optional (index 1))
    (irregex-match-substring m index))

  (define (assocar key alist)
    (match (assoc key alist)
      [#f #f]
      [pair (car pair)]))

  (define (assocdr key alist)
    (match (assoc key alist)
      [#f #f]
      [pair (cdr pair)]))

  (define (lookup key alist)
    (if (not alist)
	#f
	(assocdr key alist)))

  (define (filter pred xs)
    (if (null? xs)
	xs
	(if (pred (car xs))
	    (cons (car xs) (filter pred (cdr xs)))
	    (filter pred (cdr xs)))))

  (define (flatten1 as)
    (foldl append '() as))

  (define (flatmap f xs)
    (flatten1 (map f xs)))

  (define (falsy? x) (not x))
  (define (truthy? x) (not (falsy? x)))
  )
