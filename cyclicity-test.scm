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

(define check-silently #f)
(define check-quote
  (lambda (arg)
    (cond
      [(number? arg) #t]
      [(boolean? arg) #t]
      [(char? arg) #t]
      [(string? arg) #t]
      [(symbol? arg) #t]
      [(null? arg) #t]
      [(pair? arg)
        (let* ([first (check-quote (car arg))] [second (check-quote (cdr arg))])
          (and first second))]
      [else ; should not get here
        (begin
          (unless check-silently
            (printf "check-quote -- not recognised (not a number, boolean, char, string, symbol nor pair): ~s~n" arg))
          #f)])))

(newline)
(printf "~s~n" (check-quote (lambda (x) x)))
(printf "~s~n" (number? '(lambda (x) x)))
(printf "~s~n" (boolean? '(lambda (x) x)))
(printf "~s~n" (char? '(lambda (x) x)))
(printf "~s~n" (string? '(lambda (x) x)))
(printf "~s~n" (symbol? '(lambda (x) x)))
(printf "~s~n" (null? '(lambda (x) x)))
(printf "~s~n" (pair? '(lambda (x) x)))


;; doesn't check cyclicity
(define strict-andmap
  (lambda (p args)
    (letrec ([loop (lambda (args acc)
                     (cond
                       [(null? args)
                         acc]
                       [(pair? args)
                         (loop (cdr args) (and (p (car args)) acc))]
                       [else
                         (errorf 'strict-andmap "not a proper list")]))])
      (loop args #t))))


(define strict-andmap2
  (lambda (p args)
    (letrec ([loop (lambda (args)
                     (cond
                       [(null? args)
                        acc]
                       [(pair? args)
                        (loop (cdr args) (and (p (car args))))]
                       [else
                        (errorf 'strict-andmap "not a proper list")]))])
      (loop args #t))))
(printf "~s~n" (strict-andmap2 number? '(1 #t "")))
(printf "~s~n" (strict-andmap2 number? (list 1 2 3)))
;(printf "~s~n" (strict-andmap number? (list (trace 1) (trace 2) (trace 3))))