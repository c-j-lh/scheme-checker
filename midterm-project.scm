;;; midterm-project.scm
;;; Time-stamp: <2025-10-14 02:10:03 olivier>
;;; PLC 2025 - CS2104 2025-2026, Sem1 *)
;;; Olivier Danvy <danvy@nus.edu.sg>

;;;;;;;;;;

(define check-silently
  #f)

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


;;;;;;;;;;

(define check-program
  (lambda (v)
    (if (cyclic-value? v)
        (begin
          (unless check-silently (printf "check-program: expression must not be a cyclic list: ~s~n" v))
          #f)
        (letrec ([loop (lambda (v) (cond
                         [(null? v)
                           #t]
                         [(pair? v)
                           (let* ([head (check-toplevel-form (car v))]
                                   [rest (loop (cdr v))])
                             (and head rest))]
                         [else
                           (begin
                             (unless check-silently
                               (printf "check-program -- unrecognized program input: ~s~n" v))
                             #f)]))])
          (loop v)))))

;;;;;;;;;;

;;; 1st accessor:
(define define-1
  (lambda (v)
    (list-ref v 1)))

;;; 2nd accessor:
(define define-2
  (lambda (v)
    (list-ref v 2)))


;; effectively returns (and (proper-list? v) (length v)) without traversing twice
(define and-proper-list?-length
  (lambda (v)
    (letrec ([loop (lambda (v acc)
                     (cond
                      [(null? v) acc]
                      [(pair? v)
                       (loop (cdr v) (1+ acc))]
                      [else #f]))])
      (loop v 0))))

(define check-toplevel-form
  (lambda (v)
    (let ([analysis-result (and-proper-list?-length v)])
      (if (and (not (equal? analysis-result #f)) (not (null? v)) (equal? (car v) 'define))
          (if (= analysis-result 3)
              (check-definition (define-1 v) (define-2 v))
              (begin
                (unless check-silently
                  (printf "definitions should have 2 arguments: ~s~n" v))
                #f))
          (check-expression v)))))
;;;;;;;;;;

;;;;;;;;;;
;;; basic predicates and accessors for definitions:
;;;;;;;;;;

(define check-variable
  (lambda (name)
    (and (symbol? name)
         (not (keyword? name)))))

(define check-definition
  (lambda (name definiens)
    (and (check-variable name)
         (check-expression definiens))))

;; aleah
(define check-cond
  (lambda (clauses)
    (if (null? clauses)
        (begin
          (unless check-silently
            (printf "'cond must have at least one clause as its arguments~n"))
          #f)
        (letrec ([loop
                  (lambda (cls)
                    (if (null? cls)
                        #t ; finished all clauses successfully
                        (let ([clause (car cls)]
                              [rest (cdr cls)])
                          (if (not (list? clause))
                              (begin
                                (unless check-silently
                                  (printf "`cond` clause must be a list: ~s~n" clause))
                                #f)
                              (let ([len (length clause)])
                                (cond
                                 ;; else clause
                                 [(eq? (car clause) 'else)
                                  (if (not (null? rest))
                                      (begin
                                        (unless check-silently
                                          (printf "`else` clause must be last: ~s~n" clause))
                                        #f)
                                      (if (= len 2)
                                          (begin
                                            (check-expression (cadr clause))
                                            #t)
                                          (begin
                                            (unless check-silently
                                              (printf "`else` clause must have exactly one expression: ~s~n" clause))
                                            #f)))]
                                 ;; single expression
                                 [(= len 1)
                                  (begin
                                    (check-expression (car clause))
                                    (loop rest))]
                                 ;; two expressions
                                 [(= len 2)
                                  (begin
                                    (check-expression (car clause))
                                    (check-expression (cadr clause))
                                    (loop rest))]
                                 ;; => expression
                                 [(and (= len 3) (eq? (cadr clause) '=>))
                                  (begin
                                    (check-expression (car clause))
                                    (check-expression (caddr clause))
                                    (loop rest))]
                                 [else
                                  (begin
                                    (unless check-silently
                                      (printf "`cond` clause has invalid form: ~s~n" clause))
                                    #f)]))))))])
          (loop clauses)))))


;; lucy
(define check-case  ; implement in terms of check-cond
  (lambda (expression clauses)
    (printf "'check-case not implemented yet~n")))

;; howen
(define check-let
  (lambda (bindings expression)
    (letrec ([check-bindings
              (lambda (bs seen-names)
                (cond
                 [(null? bs) #t]
                 [(pair? bs)
                  (let ([b (car bs)])
                    (cond
                     [(and (list? b) (equal? (length b) 2)) ; [name, expression] pair
                      (let ([name (car b)]
                            [expression (cadr b)])
                        (cond
                         [(not (check-variable name)) ; name should be a variable
                          (begin
                            (unless check-silently
                              (printf "`let` binding name should be a variable (non-keyword symbol): ~s~n" name))
                            #f)]
                         [(memq name seen-names) ; names must be distinct
                          (begin
                            (unless check-silently
                              (printf "`let` binding names must be distinct, duplicate: ~s~n" name))
                            #f)]
                         [else
                          (and (check-expression expression) ; expression should be a valid expression
                               (check-bindings (cdr bs) (cons name seen-names)))]))] ; check the rest of the bindings
                     [else
                      (begin
                        (unless check-silently
                          (printf "each `let` binding must be a list of the form (name value): ~s~n" b))
                        #f)]))]
                 [else
                  (begin
                    (unless check-silently
                      (printf "`let` bindings must be a proper list: ~s~n" bindings))
                    #f)]))])
      (and (list? bindings)
           (check-bindings bindings '())
           (check-expression expression)))))


;; jiajie
(define check-letstar
  (lambda (bindings expression)
    (letrec ([check-bindings
               (lambda (binds)
                 (cond
                   ;; Base Case
                   [(null? binds) #t]
                   ;; Check if it is a pair
                   [(pair? binds)
                     (let ([bcur (car binds)])
                       (cond
                         ;; Check if current car is a proper binding with a name and expression
                         [(and (list? bcur)
                            (equal? 2 (length bcur)))
                           (let ([name (car bcur)]
                                  [exp (cadr bcur)])
                             (cond
                               ;; Check that name is a non-keyword symbol
                               [(not (check-variable name))
                                 (begin
                                   (unless check-silently
                                     (printf "`let-star' binding name should be a variable (non-keyword symbol): ~s~n" name))
                                   #f)]
                               ;; Check that the expression part of the binding is a valid expression
                               [(not (check-expression exp))
                                 (begin
                                   (unless check-silently
                                     (printf "`let-star' binding expression is not valid: ~s~n" exp))
                                   #f)]
                               ;; Recurse on the rest of the bindings
                               [else
                                 (check-bindings (cdr binds))]))]
                         [else
                           (begin
                             (unless check-silently
                               (printf "each `let-star` binding must be a list of the form (name value): ~s~n" b))
                             #f)]))]
                   ;; Definitely not a proper binding
                   [else
                     (begin
                       (unless check-silently
                         (printf "`let-star` bindings must be a proper list: ~s~n" bindings))
                       #f)]))])
      (and (list? bindings)
        (check-bindings bindings)
        (check-expression expression)))))

;; clara
(define check-letrec
  (lambda (bindings expression)
    (printf "'check-letrec not implemented yet~n")))

;; kelly
(define check-lambda
  (lambda (lambda-formals expression)
    (letrec (
             (formals-check?
              (lambda (formals seen)
                (cond
                 [(null? formals) #t]
                 [(symbol? formals)
                  (if (member formals seen)
                      (begin
                        (unless check-silently (printf "Error: duplicate variable ~a detected in lambda formals.~n" formals))
                        #f)
                      #t)]
                 ;; checks improper / proper lists
                 [(pair? formals)
                  (if (not (symbol? (car formals)))
                      (begin
                        (unless check-silently (printf "Error: non-symbol variable ~a found in lambda formals list.~n" (car formals)))
                        #f)
                      (if (member (car formals) seen)
                          (begin
                            (unless check-silently (printf "Error: duplicate variable ~a detected in lambda formals list.~n" (car formals)))
                            #f)
                          (formals-check? (cdr formals) (cons (car formals) seen))))]
                 [else
                  (begin
                    (unless check-silently (printf "Error: invalid element ~a found in lambda formals; expected variable symbol or proper/improper list.~n" formals))
                    #f)]))))
      (and (formals-check? lambda-formals '())
           (check-expression expression)))))


;; kelly
(define check-trace-lambda
  (lambda (variable lambda-formals expression)
    ;; check variable
    (if (not (symbol? variable))
        (begin
          (unless check-silently (printf "Error: trace-lambda variable ~a is not a valid symbol.~n" variable))
          #f)
        (check-lambda lambda-formals expression))))

;; yaqi
(define check-application-operands
  (lambda (args)
    (strict-andmap check-expression args)))

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


;;;;;;;;;;
;;; the syntax checker proper for expressions:
;;;;;;;;;;


(define check-expression
  (lambda (v)
    (cond
     [(number? v) #t]
     [(boolean? v) #t]
     [(char? v) #t]
     [(string? v) #t]
     [(symbol? v) #t]
     [else
      (let ([analysis-result (and-proper-list?-length v)])
        (if (or (equal? analysis-result #f) (null? v))
            (begin
              (unless check-silently
                (printf "check-expression -- unrecognised input (pairs and empty lists are not valid expressions): ~s~n" v))
              #f)
            (let* ([operator (car v)]
                   [operands (cdr v)]
                   [len (1- analysis-result)])
              (case operator ;;;;;;;
                [(time)
                 (if (equal? len 1)
                     (check-expression (car operands))
                     (begin
                       (unless check-silently
                         (printf "`time` should have exactly 1 argument~n"))
                       #f))]
                [(if)
                 (if (equal? len 3)
                     (strict-andmap check-expression operands)
                     (begin
                       (unless check-silently
                         (printf "`if` should have exactly 3 arguments: ~s~n" v))
                       #f))]
                [(unless)
                 (if (equal? len 2)
                     (strict-andmap check-expression operands)
                     (begin
                       (unless check-silently
                         (printf "`unless` should have exactly 2 arguments: ~s~n" v))
                       #f))]
                [(and or)
                 (strict-andmap check-expression operands)]
                [(cond)
                 (check-cond operands)]
                [(case)
                 (if (>= len 1)
                     (check-case (car operands) (cdr operands))
                     (begin
                       (unless check-silently
                         (printf "`case` should have at least 1 argument: ~s~n" v))
                       #f))]
                [(let)
                 (if (equal? len 2)
                     (check-let (car operands) (cadr operands))
                     (begin
                       (unless check-silently
                         (printf "`let` should have exactly 2 arguments, bindings and expression: ~s~n" v))
                       #f))]
                [(let*)
                 (if (equal? len 2)
                     (check-letstar (car operands) (cadr operands))
                     (begin
                       (unless check-silently
                         (printf "`let*` should have exactly 2 arguments, bindings and expression: ~s~n" v))
                       #f))]
                [(letrec)
                 (if (equal? len 2)
                     (check-letrec (car operands) (cadr operands))
                     (begin
                       (unless check-silently
                         (printf "`letrec` should have exactly 2 arguments, bindings and expression: ~s~n" v))
                       #f))]
                [(begin)
                 (if (>= len 1)
                     (strict-andmap check-expression operands)
                     (begin
                       (unless check-silently
                         (printf "`begin` should have at least 1 argument: ~s~n" v))
                       #f))]
                [(quote)
                 (if (equal? len 1)
                     (check-quote (car operands))
                     (begin
                       (unless check-silently
                         (printf "`quote` should have exactly 1 argument: ~s~n" v))
                       #f))]
                [(lambda)
                 (if (equal? len 2)
                     (check-lambda (car operands) (cadr operands))
                     (begin
                       (unless check-silently
                         (printf "`lambda` should have exactly 2 arguments: ~s~n" v))
                       #f))]
                [(trace-lambda)
                 (if (equal? len 3)
                     (check-trace-lambda (car operands) (cadr operands) (caddr operands))
                     (begin
                       (unless check-silently
                         (printf "`trace-lambda` should have exactly 3 arguments: ~s~n" v))
                       #f))]
                [else  ; function application
                 ;; let* instead of let to make sure head is computed before rest
                 (let* ([head (check-expression operator)] [rest (check-application-operands operands)])
                   (begin
                     (unless (or head check-silently)
                       (printf "check-expression -- unrecognized input - not a function application: ~s~n" v))
                     (and head rest)))]))))])))




;; author: Prof Olivier Danvy
(define atomic-value?
  (lambda (v)
    (or (number? v)
        (boolean? v)
        (char? v)
        (string? v)
        (symbol? v)
        (null? v))))

;; author: Prof Olivier Danvy
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


;;author: Prof Olivier Danvy
(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (- n 1))))))

;;;;;;;;;;
;;; auxiliaries:
;;;;;;;;;;

(define keyword?
  (lambda (symbol)
    (memq symbol '(define, time, if, cond, else, case, and, or, let, let*,
                    letrec, begin, unless, quote, lambda, and trace-lambda))))

(define list-strictly-longer-than?
  (lambda (v n)
    (letrec ([visit (lambda (v i)
                      (and (pair? v)
                           (or (= i 0)
                               (visit (cdr v)
                                      (- i 1)))) )])
      (if (>= n 0)
          (visit v n)
          (errorf 'list-strictly-longer-than? "negative length: ~s" n)))))

;;; reads an entire file as a list of Scheme data
;;; use: (read-file "filename.scm")
(define read-file
  (lambda (filename)
    (call-with-input-file filename
      (lambda (p)
        (letrec ([visit (lambda ()
                          (let ([in (read p)])
                            (if (eof-object? in)
                                '()
                                (cons in (visit)))))])
          (visit))))))

;;; interface:
(define check-file
  (lambda (filename)
    (if (string? filename)
        (check-program (read-file filename))
        (errorf 'check-file "not a string: ~s" filename))))

;;;;;;;;

(let ([cycle '(0 1)])
  (begin
    (printf "~n~n~n=== check-expression with cycles ===~n")
    (set-cdr! cycle cycle)
    (let ([b (check-program
               (list (list cycle 1)))])
      (printf "-- Test case 5.1: ~s~n" b))
    (let ([b (check-program
               (list (list '+ cycle)))])
      (printf "-- Test case 5.2: ~s~n" b))
    (let ([b (check-program
               (list cycle))])
      (printf "-- Test case 5.3: ~s~n" b))

    (printf "~n~n~n=== indirectly: check-quote tests ===~n")
    (let* ([e '(quote a)] [r (check-expression e)])
      (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'correct 'wrong)))
    (let* ([e '(quote (a . b))] [r (check-expression e)])
      (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'correct 'wrong)))
    (let* ([e '(quote (#t . (3 . (#\a . ""))))] [r (check-expression e)])
      (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'correct 'wrong)))
    (let* ([e '(quote (#t . cycle))] [r (check-expression e)])
      (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'correct 'wrong)))
    (let* ([e '(quote (list cycle))] [r (check-expression e)])
      (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'correct 'wrong)))
    (let* ([e (lambda (x) x)] [r (check-quote e)])
      (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'wrong 'correct)))
    (let* ([e '(quote (#t . (3 . (#\a . cycle))))] [r (check-expression e)])
      (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'correct 'wrong)))))


(printf "~n~n~n=== check-program ===~n")
(let ([b (check-program
          '((trace-lambda 3 2 5)))])
  (printf "-- Test case 6: ~s~n" b))
(let ([b (check-program
          '((trace-lambda 3 2 5 5)))])
  (printf "-- Test case 7: ~s~n" b))

(printf "~n~n~n=== Prof Danvy's general tests ===~n")
(let* ([e '(define foo . foo)] [r (check-expression e)])
  (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'wrong 'correct)))
(let* ([e '(+ . foo)] [r (check-expression e)])
  (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'wrong 'correct)))
(let* ([e '(if (case) (case) 0)] [r (check-expression e)])
  (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'wrong 'correct)))
(let* ([e '(unless (a . b) (c . d))] [r (check-expression e)]) ; should check both incorrect expressions
  (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'wrong 'correct)))
(let* ([e '(case . foo)] [r (check-expression e)])
  (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'wrong 'correct)))
(let* ([e '()] [r (check-expression e)])
  (printf "-- Test case ~s: ~s (~s)~n~n" e r (if r 'wrong 'correct)))


(printf "~n~n~n=== Yaqi's old general tests ===~n")
(let ([b (check-program
          '((f 10)
            (g (f 20))
            (define f (lambda (x) x))
            (define g (lambda (x) (f x)))))])
  (printf "-- Test case -1: ~s~n~n" b))
;; test case to check expression types
(let ([b (check-program
          '(;; numbers
            (define n1 42)
            (define n2 -3.14)
            ;; booleans
            (define b1 #t)
            (define b2 #f)
            ;; characters
            (define c1 #\a)
            (define c2 #\space)
            ;; strings
            (define s1 "hello")
            (define s2 "")))])
  (printf "-- Test case 0: ~s~n~n" b))
;; test case to check If Expression
(let ([b (check-program
          '((if #t 1 2)
            (if (and 1 2) 'yes 'no)))])
  (printf "-- Test case 1: ~s~n~n" b))
;; test case to check And/Or Expressions
(let ([b (check-program
          '((and #t #t)
            (or #f #t)))])
  (printf "-- Test case 2: ~s~n~n" b))
;;  test case to check Unless Expression
(let ([b (check-program
          '((unless #f 'ok)))])
  (printf "-- Test case 3: ~s~n~n" b))
;;  test case to check Quote Expression
(let ([b (check-program
          '((quote hello)
            (quote (1 2 3))))])
  (printf "-- Test case 4: ~s~n~n" b))
;; test case for Valid cond - has else at end
(let ([b (check-program
          '((cond
             [#t 1]
             [#f 2]
             [else 3])))])
  (printf "-- Test case 5: ~s~n" b))




(printf "~n~n~n=== check-cond tests ===~n")
;; Test 1: Empty cond will fail
(let ([b (check-cond '())])
  (printf "Test 1: ~s~n~n" b))
;; Test 2: Valid cond with else at the end
(let ([b (check-cond '([else 3]))])
  (printf "Test 2: ~s~n~n" b))
;; Test 3: Cond with single expression clauses only
(let ([b (check-cond '([#t] [#f]))])
  (printf "Test 3: ~s~n~n" b))
;; Test 4: Cond with double expression clauses
(let ([b (check-cond '([#t 1]))])
  (printf "Test 4: ~s~n~n" b))
;; Test 5: Cond with => expression
(let ([b (check-cond '([#t => (lambda (x) x)]
                       [#f => (lambda (y) y)]
                       [else 42]))])
  (printf "Test 5: ~s~n~n" b))
;; Test 6: Cond with else not last will fail
(let ([b (check-cond '([else 1] [#f 2]))])
  (printf "Test 6: ~s~n~n" b))
;; Test 7: Cond with >1 elses will fail
(let ([b (check-cond '([#t 1] [else 2] [else 3]))])
  (printf "Test 7: ~s~n~n" b))
;; Test 8: Cond with invalid syntax will fail
(let ([b (check-cond '([#t 1 2]))])
  (printf "Test 8: ~s~n~n" b))
;; Test 9: Cond clause without parentheses will fail
(let ([b (check-cond '(#t ([#f 2])))])
  (printf "Test 9: ~s~n~n" b))

(printf "~n~n~n=== check-case tests ===~n")


(printf "~n~n~n=== check-let tests ===~n")
;; valid
(let ([b (check-program
           '((let () 42)))])
  (printf "Test case 8: ~s~n~n" b))
;; valid
(let ([b (check-program
           '((let ([x 1] [y 2]) x)))])
  (printf "Test case 9: ~s~n~n" b))
;; invalid, binding name should be a variable
(let ([b (check-program
           '((let ([1 2]) 1)))])
  (printf "Test case 10: ~s~n~n" b))
;; invalid, binding names must be distinct
(let ([b (check-program
           '((let ([x 1] [x 2]) x)))])
  (printf "Test case 11: ~s~n~n" b))
;; invalid, binding must have a value
(let ([b (check-program
           '((let ([x]) x)))])
  (printf "Test case 12: ~s~n~n" b))
;; invalid, binding must have a name
(let ([b (check-program
           '((let ([1]) x)))])
  (printf "Test case 13: ~s~n~n" b))



(printf "~n~n~n=== check-letstar tests ===~n")
;; Valid
(let ([b (check-program
           '((let* () 10)))])
  (printf "Test let-star Empty Bindings: ~s~n~n" b))
;; Valid
(let ([b (check-program
           '((let* ([x 10] [y 20]) x)))])
  (printf "Test let-star Multiple Bindings: ~s~n~n" b))
;; Valid
(let ([b (check-program
           '((let* ([x 10] [x 20]) x)))])
  (printf "Test let-star Non-Distinct Bindings: ~s~n~n" b))
;; Invalid, Binding Name is not a Variable
(let ([b (check-program
           '((let* ([10 20]) 10)))])
  (printf "Test let-star Invalid Binding Name: ~s~n~n" b))
;; Invalid, Binding must have a Value
(let ([b (check-program
           '((let ([x]) x)))])
  (printf "Test let-star Binding without Value: ~s~n~n" b))
;; Invalid, Binding must have Name
(let ([b (check-program
           '((let ([10]) x)))])
  (printf "Test let-star Binding without Name: ~s~n~n" b))


(printf "~n~n~n=== check-letrec tests ===~n")


(printf "~n~n~n=== check-lambda and check-trace-lambda tests ===~n")
;; Proper list (valid)
(let ([b (check-lambda '(x y z) '())])
  (printf "Proper list (valid): ~s~n~n" b))
;; Proper list (duplicate var)
(let ([b (check-lambda '(x y x) '())])
  (printf "Proper list (duplicate var): ~s~n~n" b))
;; Proper list (non-symbol)
(let ([b (check-lambda '(x 3 z) '())])
  (printf "Proper list (non-symbol): ~s~n~n" b))
;; Improper list (valid dotted tail)
(let ([b (check-lambda '(x y . z) '())])
  (printf "Improper list (valid dotted tail): ~s~n~n" b))
;; Improper list (duplicate in tail)
(let ([b (check-lambda '(x x . y) '())])
  (printf "Improper list (duplicate in tail): ~s~n~n" b))
;; Improper list (non-symbol tail)
(let ([b (check-lambda '(x y . 3) '())])
  (printf "Improper list (non-symbol tail): ~s~n~n" b))
;; Improper list (dotted tail empty list)
(let ([b (check-lambda '(x . ()) '())])
  (printf "Improper list (dotted tail empty list): ~s~n~n" b))
;; Single symbol
(let ([b (check-lambda 'x '())])
  (printf "Single symbol: ~s~n~n" b))
;; Invalid number formals
(let ([b (check-lambda 42 '())])
  (printf "Invalid number formals: ~s~n~n" b))
;; Invalid string formals
(let ([b (check-lambda "x" '())])
  (printf "Invalid string formals: ~s~n~n" b))
;; valid trace-lambda: valid symbol variable and valid formals
(let ([b (check-trace-lambda 'trace1 '(x y) '(+ x y))])
  (printf "Test trace-lambda valid: ~s~n~n" b))
;; invalid trace-lambda: variable is not a symbol (number)
(let ([b (check-trace-lambda 42 '(x y) '(+ x y))])
  (printf "Test trace-lambda invalid variable number: ~s~n~n" b))
;; invalid trace-lambda: variable is not a symbol (string)
(let ([b (check-trace-lambda "trace" '(x) '(+ x 1))])
  (printf "Test trace-lambda invalid variable string: ~s~n~n" b))
;; invalid trace-lambda: duplicate variable in formals (uses your check-lambda to detect)
(let ([b (check-trace-lambda 'trace2 '(x x) '(+ x x))])
  (printf "Test trace-lambda duplicate vars in formals: ~s~n~n" b))
;; valid trace-lambda: improper list in formals (valid dotted tail)
(let ([b (check-trace-lambda 'trace3 '(x . y) '(+ x y))])
  (printf "Test trace-lambda valid improper formals: ~s~n~n" b))
;; invalid trace-lambda: improper list invalid dotted tail
(let ([b (check-trace-lambda 'trace4 '(x . 3) '(+ x 3))])
  (printf "Test trace-lambda invalid improper dotted tail: ~s~n~n" b))


(printf "~n~n~n=== check-application-operands tests ===~n")
(let ([result (check-application-operands '(42 #t "hello" #\a))])
  (printf "-- Test (int boolean string): ~s~n" result))
(let ([result (check-application-operands '())])
  (printf "-- Test (null): ~s~n" result))
(let ([result (check-application-operands '((quote hello) (quote (1 2 3)) (quote symbol)))])
  (printf "-- Test (Quoted expressions): ~s~n" result))
(let ([result (check-application-operands '((time (+ 1 2)) (time (* 3 4))))])
  (printf "-- Test (Time expressions): ~s~n" result))
(let ([result (check-application-operands '((if #t 1 2) (if #f "yes" "no")))])
  (printf "-- Test (If expressions): ~s~n" result))

(define check-t ; correct answer should be #t
  (lambda (result)
    (if result 'correct 'wrong)))
(let* ([e '((1 . 2) (3 . 4))] [result (check-application-operands e)])
    (printf "-- Test case ~s: ~s (~s)~n~n" e result (check-t result)))


(printf "Checking itself: ~s~n" (check-file "midterm-project.scm"))



;;; end of midterm-project.scm
"midterm-project.scm"
