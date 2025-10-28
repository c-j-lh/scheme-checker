(define cyclic-list?
  (lambda (v)
    (letrec ([visit (lambda (sp fp)
                      (cond
                        [(eq? sp fp)
                          #t]
                        [(null? fp)
                          #f]
                        [(pair? fp)
                          (let ([fpp (cdr fp)])
                            (cond
                              [(null? fpp)
                                #f]
                              [(pair? fpp)
                                (visit (cdr sp) (cdr fpp))]
                              [else
                                #f]))]
                        [else
                          #f]))])
      (cond
        [(and (pair? v) (pair? (cdr v)))
          (visit (cdr v) (cdr (cdr v)))]
        [else
          #f]))))

(define atomic-value?
  (lambda (v)
    (or (number? v)
      (boolean? v)
      (char? v)
      (string? v)
      (symbol? v)
      (null? v))))

(define cyclic-value?
  (lambda (v)
    (letrec ([visit (lambda (v cs)
                      (cond
                        [(atomic-value? v)
                          #f]
                        [(memq v cs)
                          #t]
                        [(pair? v)
                          (let ([csp (cons v cs)])
                            (or (visit (car v) csp)
                              (visit (cdr v) csp)))]
                        [else
                          (errorf 'cyclic-value "unaddressed type of value: ~s" v)]))])
      (visit v '()))))

(define rec-cyclic-list?
  (lambda (v)
    (or (cyclic-list? v)
        (andmap cyclic-list? v))))

(define cycle '(0 1))
(set-cdr! cycle cycle)
(printf "~s ~s~n" (cyclic-list? cycle) (cyclic-value? cycle))

(let ([b (list 0 cycle)])
  (printf "~s ~s~n" (cyclic-list? b) (cyclic-value? b)))

(define b '((1 2) 3))
(set-cdr! (car b) b)
(printf "~s ~s ~s~n" (cyclic-list? b) (rec-cyclic-list? b) (cyclic-value? b))