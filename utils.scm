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
  )
q
