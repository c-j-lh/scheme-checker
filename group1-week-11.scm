;;; group1-week-11.scm
;;; Time-stamp: <2025-11-29 17:21:47 leheng>
;;; PLC 2025 - CS2104 2025-2026, Sem1 *)
;;; Olivier Danvy <danvy@nus.edu.sg>
;;; Group 1: Chuah Jia Jie <e0959959@u.nus.edu>; Yap Ho Wen <howenyap@u.nus.edu>; Yang Yaqi <e1522259@u.nus.edu>; Wang Aleah <e1518514@u.nus.edu>; Goussanou Clara <e1535596@u.nus.edu>; Zhang Kelly <e1518548@u.nus.edu>; Wu Lucy Shuai <e1521751@u.nus.edu>; Chieu Le Heng <e0454394@u.nus.edu>

;;;;;;;;;;

(define proper-list-of-given-length?
  (lambda (v n)
    (or (and (null? v)
             (= n 0))
        (and (pair? v)
             (> n 0)
             (proper-list-of-given-length? (cdr v)
                                           (- n 1))))))

;;;;;;;;;;

;;; predicates and accessors:

(define is-mt?
  (lambda (v)
    (and (proper-list-of-given-length? v 1)
         (equal? (car v) 'mt))))

(define is-atom?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'atom))))

(define atom-1 cadr)

(define is-any?
  (lambda (v)
    (and (proper-list-of-given-length? v 1)
         (equal? (car v) 'any))))

(define is-seq?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'seq))))

(define seq-1 cadr)

(define seq-2 caddr)

(define is-disj?
  (lambda (v)
    (and (proper-list-of-given-length? v 3)
         (equal? (car v) 'disj))))

(define disj-1 cadr)

(define disj-2 caddr)

(define is-star?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'star))))

(define star-1 cadr)

(define is-plus?
  (lambda (v)
    (and (proper-list-of-given-length? v 2)
         (equal? (car v) 'plus))))

(define plus-1 cadr)

;;;;;;;;;;

;;; Version 0: mt, atom, and seq

(define positive-unit-tests-for-match0
  (lambda (candidate)
    (and (candidate '(mt) '())
         (candidate '(atom 1) '(1))
         (candidate '(seq (mt) (mt)) '())
         (candidate '(seq (mt) (atom 2)) '(2))
         (candidate '(seq (atom 1) (mt)) '(1))
         (candidate '(seq (atom 1) (atom 2)) '(1 2))
         (candidate '(seq (atom 1) (seq (atom 2) (atom 3)))
                    '(1 2 3))
         (candidate '(seq (seq (atom 1) (atom 2)) (atom 3))
                    '(1 2 3))
         )))

(define negative-unit-tests-for-match0
  (lambda (candidate)
    (and (not (candidate '(mt) '(1)))
         (not (candidate '(atom 1) '()))
         (not (candidate '(atom 1) '(2)))
         (not (candidate '(atom 1) '(1 2)))
         (not (candidate '(seq (atom 1) (atom 2)) '()))
         (not (candidate '(seq (atom 1) (atom 2)) '(1)))
         (not (candidate '(seq (atom 1) (atom 2)) '(2)))
         (not (candidate '(seq (atom 1) (atom 2)) '(1 2 3)))
         )))

;;;;;

;;; we shall use a local procedure visit:

;;; given ns1 such that re matches ns1,
;;; applying visit to re and (append ns1 ns2)
;;; should return ns2

;;; given ns1 such that re does not matches ns1,
;;; applying visit to re and (append ns1 ns2)
;;; should return #f


(define match0
  (lambda (re_given ns_given)
    (letrec ([visit (lambda (re ns)
                      ;; returns the rest of ns or returns #f
                      (cond
                       [(is-mt? re)
                        ns]
                       [(is-atom? re)
                        (if (pair? ns)
                            (if (= (atom-1 re) (car ns))
                                (cdr ns)
                                #f)
                            #f)]
                       [(is-seq? re)
                        (let ([nsp (visit (seq-1 re) ns)])
                          (if nsp
                              (visit (seq-2 re) nsp)
                              #f))]
                       [else
                        (errorf 'match0 "not implemented yet")]))])
      (null? (visit re_given ns_given)))))

(unless (positive-unit-tests-for-match0 match0)
  (printf "the positive tests failed for match0"))

(unless (negative-unit-tests-for-match0 match0)
  (printf "the negative tests failed for match0"))

;;;;;;;;;;

;;; Exercise:
;;; Extend Version 0 into Version 1 that also processes any

(define positive-unit-tests-for-match1
  (lambda (candidate)
    (and (candidate '(any) '(1))
         (candidate '(any) '(-1))
         (candidate '(seq (any) (mt)) '(1))
         (candidate '(seq (mt) (seq (any) (mt))) '(-1))
         (candidate '(seq (any) (any)) '(1 2))
         (candidate '(seq (any) (any)) '(1 -1))
         (candidate '(seq (any) (seq (any) (any))) '(1 2 3))
         (candidate '(seq (any) (seq (atom 3) (any))) '(1 3 3))
         (candidate '(seq (seq (any) (any)) (any)) '(-1 -4 3)))))

(define negative-unit-tests-for-match1
  (lambda (candidate)
    (and (not (candidate '(any) '()))
         (not (candidate '(any) '(1 2)))
         (not (candidate '(seq (any) (mt)) '()))
         (not (candidate '(seq (mt) (seq (any) (mt))) '(-1 -2)))
         (not (candidate '(seq (any) (any)) '()))
         (not (candidate '(seq (any) (any)) '(1)))
         (not (candidate '(seq (any) (any)) '(1 2 3)))
         (not (candidate '(seq (any) (seq (any) (any))) '()))
         (not (candidate '(seq (any) (seq (any) (any))) '(1 2)))
         (not (candidate '(seq (any) (seq (any) (any))) '(1 (2 3))))
         (not (candidate '(seq (any) (seq (any) (any))) '(1 2 3 4)))
         (not (candidate '(seq (any) (seq (atom 3) (any))) '(1 -100 3)))
         (not (candidate '(seq (seq (any) (any)) (any)) '(-1 -4 3 4))))))

(define match1
  (lambda (re_given ns_given)
    (letrec ([visit (lambda (re ns)
                      ;; returns the rest of ns or returns #f
                      (cond
                       [(is-mt? re)
                        ns]
                       [(is-atom? re)
                        (if (pair? ns)
                            (if (= (atom-1 re) (car ns))
                                (cdr ns)
                                #f)
                            #f)]
                       [(is-any? re)
                        (if (pair? ns)
                            (cdr ns)
                            #f)]
                       [(is-seq? re)
                        (let ([nsp (visit (seq-1 re) ns)])
                          (if nsp
                              (visit (seq-2 re) nsp)
                              #f))]
                       [else
                        (errorf 'match1 "not implemented yet")]))])
      (null? (visit re_given ns_given)))))

(unless (positive-unit-tests-for-match0 match1)
  (printf "the positive tests for match0 failed for match1"))

(unless (negative-unit-tests-for-match0 match1)
  (printf "the negative tests for match0 failed for match1"))

(unless (positive-unit-tests-for-match1 match1)
  (printf "the positive tests failed for match1"))

(unless (negative-unit-tests-for-match1 match1)
  (printf "the negative tests failed for match1"))

;;;;;;;;;;

;;; Version 2: mt, atom, seq, and disj

(define positive-unit-tests-for-match2
  (lambda (candidate)
    (and (candidate '(disj (atom 1) (atom 2))
                    '(1))
         (candidate '(disj (atom 1) (atom 2))
                    '(2))
         (candidate '(seq (atom 0) (disj (atom 1) (atom 2)))
                    '(0 1))
         (candidate '(seq (atom 0) (disj (atom 1) (atom 2)))
                    '(0 2))
         (candidate '(disj (disj (atom 1) (atom 2))
                           (disj (atom 3) (atom 4)))
                    '(1))
         (candidate '(disj (disj (atom 1) (atom 2))
                           (disj (atom 3) (atom 4)))
                    '(2))
         (candidate '(disj (disj (atom 1) (atom 2))
                           (disj (atom 3) (atom 4)))
                    '(3))
         (candidate '(disj (disj (atom 1) (atom 2))
                           (disj (atom 3) (atom 4)))
                    '(4))
         (candidate '(seq (atom 0)
                          (seq (disj (disj (atom 1) (atom 2))
                                     (disj (atom 3) (atom 4)))
                               (atom 5)))
                    '(0 1 5))
         (candidate '(seq (atom 0)
                          (seq (disj (disj (atom 1) (atom 2))
                                     (disj (atom 3) (atom 4)))
                               (atom 5)))
                    '(0 2 5))
         (candidate '(seq (atom 0)
                          (seq (disj (disj (atom 1) (atom 2))
                                     (disj (atom 3) (atom 4)))
                               (atom 5)))
                    '(0 3 5))
         (candidate '(seq (atom 0)
                          (seq (disj (disj (atom 1) (atom 2))
                                     (disj (atom 3) (atom 4)))
                               (atom 5)))
                    '(0 4 5))
         )))

(define negative-unit-tests-for-match2
  (lambda (candidate)
    (and (not (candidate '(disj (atom 1) (atom 2)) '()))
         (not (candidate '(disj (atom 1) (atom 2)) '(0)))
         (not (candidate '(disj (atom 1) (atom 2)) '(1 3)))
         (not (candidate '(disj (atom 1) (atom 2)) '(2 3)))
         )))

;;;;;

;;; we shall use a continuation-based local procedure visit:

;;; invariant:
;;; (visit-for-match2 re ns k) == (k (visit-for-match1 re ns))

(define match2
  (lambda (re_given ns_given)
    (letrec ([visit (lambda (re ns k)
                      ;; applies k to the rest of ns or returns #f
                      (cond
                       [(is-mt? re)
                        (k ns)]
                       [(is-atom? re)
                        (if (pair? ns)
                            (if (= (atom-1 re) (car ns))
                                (k (cdr ns))
                                #f)
                            #f)]
                       [(is-any? re)
                        (if (pair? ns)
                            (k (cdr ns))
                            #f)]
                       [(is-seq? re)
                        (visit (seq-1 re) ns (lambda (nsp)
                                               (visit (seq-2 re) nsp k)))]
                       [(is-disj? re)
                        (or (visit (disj-1 re) ns k)
                            (visit (disj-2 re) ns k))]
                       [else
                        (errorf 'match2 "not implemented yet")]))])
      (visit re_given ns_given null?))))

(unless (positive-unit-tests-for-match0 match2)
  (printf "the positive tests for match0 failed for match2"))

(unless (negative-unit-tests-for-match0 match2)
  (printf "the negative tests for match0 failed for match2"))

(unless (positive-unit-tests-for-match1 match2)
  (printf "the positive tests for match1 failed for match2"))

(unless (negative-unit-tests-for-match1 match2)
  (printf "the negative tests for match1 failed for match2"))

(unless (positive-unit-tests-for-match2 match2)
  (printf "the positive tests failed for match2"))

(unless (negative-unit-tests-for-match2 match2)
  (printf "the negative tests failed for match2"))

;;;;;;;;;;

;;; rather than returning #t or #f, let us return the number of ways re and ns match

(define unit-tests-for-match2-how-many
  (lambda (candidate)
    (and (= (candidate '(seq (atom 1) (seq (atom 2) (atom 3)))
                       '(1 2 5))
            0)
         (= (candidate '(seq (atom 1) (seq (atom 2) (atom 3)))
                       '(1 2 3))
            1)
         (= (candidate '(disj (seq (atom 1) (mt))
                              (seq (mt) (atom 1)))
                       '(1))
            2)
         (= (candidate '(disj (disj (atom 1)
                                    (atom 1))
                              (disj (atom 1)
                                    (atom 1)))
                       '(1))
            4)
         (= (candidate '(disj (disj (seq (atom 1) (mt))
                                    (seq (mt) (atom 1)))
                              (disj (seq (atom 1) (mt))
                                    (seq (mt) (atom 1))))
                       '(1))
            4)
         (= (candidate '(seq (disj (atom 1) (seq (atom 1) (atom 2)))
                             (disj (atom 2) (mt)))
                       '(1 2))
            2)
         )))

(define match2-how-many
  (lambda (re_given ns_given)
    (letrec ([visit (lambda (re ns k)
                      ;; applies k to the rest of ns or returns 0
                      (cond
                       [(is-mt? re)
                        (k ns)]
                       [(is-atom? re)
                        (if (pair? ns)
                            (if (= (atom-1 re) (car ns))
                                (k (cdr ns))
                                0)
                            0)]
                       [(is-seq? re)
                        (visit (seq-1 re) ns (lambda (nsp)
                                               (visit (seq-2 re) nsp k)))]
                       [(is-disj? re)
                        (+ (visit (disj-2 re) ns k)
                           (visit (disj-1 re) ns k))]
                       [else
                        (errorf 'match2-how-many "not implemented yet")]))])
      (visit re_given ns_given (lambda (nsp) (if (null? nsp) 1 0))))))

(unless (unit-tests-for-match2-how-many match2-how-many)
  (printf "the tests failed for match2-how-many"))





;;; Version 3: mt, atom, seq, disj and any

(define positive-unit-tests-for-match3
  (lambda (candidate)
    (and (candidate '(disj (atom 1) (any))
                    '(3))
         (candidate '(disj (any) (atom 2))
                    '(3))
         (candidate '(seq (atom 0) (disj (any) (atom 2)))
                    '(0 3))
         (candidate '(seq (any) (disj (atom 1) (atom 2)))
                    '(3 2))
         (candidate '(disj (disj (any) (atom 2))
                           (disj (atom 3) (atom 4)))
                    '(5))
         (candidate '(disj (disj (atom 1) (any))
                           (disj (atom 3) (atom 4)))
                    '(5))
         (candidate '(disj (disj (atom 1) (atom 2))
                           (disj (any) (atom 4)))
                    '(5))
         (candidate '(disj (disj (atom 1) (atom 2))
                           (disj (atom 3) (any)))
                    '(5))
         (candidate '(seq (any)
                          (seq (disj (disj (atom 1) (atom 2))
                                     (disj (atom 3) (atom 4)))
                               (atom 5)))
                    '(10 1 5))
         (candidate '(seq (atom 0)
                          (seq (disj (disj (any) (atom 2))
                                     (disj (atom 3) (any)))
                               (atom 5)))
                    '(0 10 5))
         (candidate '(seq (atom 0)
                          (seq (disj (disj (atom 1) (atom 2))
                                     (disj (atom 3) (atom 4)))
                               (atom 5)))
                    '(0 3 5)))))

(define negative-unit-tests-for-match3
  (lambda (candidate)
    (and (not (candidate '(disj (atom 1) (any)) '()))
         (not (candidate '(disj (any) (any)) '()))
         (not (candidate '(disj (atom 1) (any)) '(1 3)))
         (not (candidate '(disj (atom 1) (any)) '(2 3)))
         )))

(define match3
  (lambda (re_given ns_given)
    (letrec ([visit (lambda (re ns k)
                      ;; applies k to the rest of ns or returns #f
                      (cond
                       [(is-mt? re)
                        (k ns)]
                       [(is-atom? re)
                        (if (pair? ns)
                            (if (= (atom-1 re) (car ns))
                                (k (cdr ns))
                                #f)
                            #f)]
                       [(is-any? re)
                        (if (pair? ns)
                            (k (cdr ns))
                            #f)]
                       [(is-seq? re)
                        (visit (seq-1 re) ns (lambda (nsp)
                                               (visit (seq-2 re) nsp k)))]
                       [(is-disj? re)
                        (or (visit (disj-1 re) ns k)
                            (visit (disj-2 re) ns k))]
                       [else
                        (errorf 'match3 "not implemented yet")]))])
      (visit re_given ns_given null?))))


(unless (positive-unit-tests-for-match0 match3)
  (printf "the positive tests for match0 failed for match3"))

(unless (negative-unit-tests-for-match0 match3)
  (printf "the negative tests for match0 failed for match3"))

(unless (positive-unit-tests-for-match1 match3)
  (printf "the positive tests for match1 failed for match3"))

(unless (negative-unit-tests-for-match1 match3)
  (printf "the negative tests for match1 failed for match3"))

(unless (positive-unit-tests-for-match2 match3)
  (printf "the positive tests for match2 failed for match3"))

(unless (negative-unit-tests-for-match2 match3)
  (printf "the negative tests for match2 failed for match3"))

(unless (positive-unit-tests-for-match3 match3)
  (printf "the positive tests failed for match3"))

(unless (negative-unit-tests-for-match3 match3)
  (printf "the negative tests failed for match3"))

;;;;;;;;;;

;;; rather than returning #t or #f, let us return the number of ways re and ns match

;; @howen
(define unit-tests-for-match3-how-many
  (lambda (candidate)
    (and (= (candidate '(any) '(1)) 1)
         (= (candidate '(any) '()) 0)
         (= (candidate '(any) '(1 2)) 0)
         (= (candidate '(disj (atom 1) (any)) '(1)) 2)
         (= (candidate '(disj (atom 1) (any)) '(2)) 1)
         (= (candidate '(disj (any) (any)) '(3)) 2)
         (= (candidate '(disj (seq (atom 1) (mt))
                              (seq (any) (mt)))
                       '(1))
            2)
         (= (candidate '(disj (disj (any) (any))
                              (disj (any) (any)))
                       '(42))
            4))))

(define match3-how-many
  (lambda (re_given ns_given)
    (letrec ([visit (lambda (re ns k)
                      ;; applies k to the rest of ns or returns 0
                      (cond
                       [(is-mt? re)
                        (k ns)]
                       [(is-atom? re)
                        (if (pair? ns)
                            (if (= (atom-1 re) (car ns))
                                (k (cdr ns))
                                0)
                            0)]
                       [(is-any? re)
                        (if (pair? ns)
                            (k (cdr ns))
                            0)]
                       [(is-seq? re)
                        (visit (seq-1 re) ns (lambda (nsp)
                                               (visit (seq-2 re) nsp k)))]
                       [(is-disj? re)
                        (+ (visit (disj-2 re) ns k)
                           (visit (disj-1 re) ns k))]
                       [else
                        (errorf 'match3-how-many "not implemented yet")]))])
      (visit re_given ns_given (lambda (nsp) (if (null? nsp) 1 0))))))

(unless (unit-tests-for-match2-how-many match3-how-many)
  (printf "the tests for match2-how-many failed for match3-how-many"))
(unless (unit-tests-for-match3-how-many match3-how-many)
  (printf "the tests failed for match3-how-many"))




;;; Version 4: mt, atom, seq, disj, any and star

(define positive-unit-tests-for-match4
  (lambda (candidate)
    (and
     (candidate '(star (mt))
                '())
     (candidate '(star (atom 3))
                '())
     (candidate '(star (atom 3))
                '(3))
     (candidate '(star (atom 3))
                '(3 3 3))
     (candidate '(seq (atom 1) (seq (star (disj (atom 2) (seq (atom 3) (atom 4)))) (atom 5)))
                '(1 5))
     (candidate '(seq (atom 1) (seq (star (disj (atom 2) (seq (atom 3) (atom 4)))) (atom 5)))
                '(1 2 5))
     (candidate '(seq (atom 1) (seq (star (disj (atom 2) (seq (atom 3) (atom 4)))) (atom 5)))
                '(1 3 4 5))
     (candidate '(seq (atom 1) (seq (star (disj (atom 2) (seq (atom 3) (atom 4)))) (atom 5)))
                '(1 2 2 2 3 4 2 5))
     )))

(define negative-unit-tests-for-match4
  (lambda (candidate)
    (and
     (not (candidate '(star (mt))
                     '(1)))
     (not (candidate '(star (atom 3))
                     '(4)))
     (not (candidate '(star (atom 3))
                     '(3 3 4)))
     (not (candidate '(seq (atom 1) (seq (star (disj (atom 2) (seq (atom 3) (atom 4)))) (atom 5)))
                     '(1 5 6)))
     (not (candidate '(seq (atom 1) (seq (star (disj (atom 2) (seq (atom 3) (atom 4)))) (atom 5)))
                     '(1 1 2 5)))
     (not (candidate '(seq (atom 1) (seq (star (disj (atom 2) (seq (atom 3) (atom 4)))) (atom 5)))
                     '(1 3 2 4 5)))
     (not (candidate '(seq (atom 1) (seq (star (disj (atom 2) (seq (atom 3) (atom 4)))) (atom 5)))
                     '(1 2 2 2 3 4 2 5 6)))
     )))

(define match4
  (lambda (re_given ns_given)
    (letrec ([visit (lambda (re ns k)
                      ;; applies k to the rest of ns or returns #f
                      (cond
                       [(is-mt? re)
                        (k ns)]
                       [(is-atom? re)
                        (if (pair? ns)
                            (if (= (atom-1 re) (car ns))
                                (k (cdr ns))
                                #f)
                            #f)]
                       [(is-any? re)
                        (if (pair? ns)
                            (k (cdr ns))
                            #f)]
                       [(is-seq? re)
                        (visit (seq-1 re) ns (lambda (nsp)
                                               (visit (seq-2 re) nsp k)))]
                       [(is-disj? re)
                        (or (visit (disj-1 re) ns k)
                            (visit (disj-2 re) ns k))]
                       [(is-star? re)
                        (or (k ns)
                            (visit (star-1 re) ns
                                   (lambda (nsp)
                                     (if (eq? ns nsp)
                                         #f
                                         (visit re nsp k)))))]
                       [else
                        (errorf 'match4 "not implemented yet")]))])
      (visit re_given ns_given null?))))


(unless (positive-unit-tests-for-match0 match4)
  (printf "the positive tests for match0 failed for match4"))
(unless (negative-unit-tests-for-match0 match4)
  (printf "the negative tests for match0 failed for match4"))

(unless (positive-unit-tests-for-match1 match4)
  (printf "the positive tests for match1 failed for match4"))
(unless (negative-unit-tests-for-match1 match4)
  (printf "the negative tests for match1 failed for match4"))

(unless (positive-unit-tests-for-match2 match4)
  (printf "the positive tests for match2 failed for match4"))
(unless (negative-unit-tests-for-match2 match4)
  (printf "the negative tests for match2 failed for match4"))

(unless (positive-unit-tests-for-match3 match4)
  (printf "the positive tests for match3 failed for match4"))
(unless (negative-unit-tests-for-match3 match4)
  (printf "the negative tests for match3 failed for match4"))

(unless (positive-unit-tests-for-match4 match4)
  (printf "the positive tests failed for match4"))
(unless (negative-unit-tests-for-match4 match4)
  (printf "the negative tests failed for match4"))

;;;;;;;;;;

;;; rather than returning #t or #f, let us return the number of ways re and ns match

(define unit-tests-for-match4-how-many
  (lambda (candidate)
    (and (= (candidate '(star (mt)) '())
            1)
         (= (candidate '(star (atom 3)) '(3 3))
            1)
         (= (candidate '(seq (star (atom 1)) (star (atom 1))) '(1 1))
            3)
         (= (candidate '(seq (star (disj (atom 1) (atom 1))) (mt))
                       '(1))
            2)
         (= (candidate '(star (star (atom 1)))
                       '(1))
            1)
         (= (candidate '(star (star (atom 1)))
                       '(1 1))
            2)
         (= (candidate '(star (star (atom 1)))
                       '(1 1 1))
            4)
         (= (candidate '(star (star (atom 1)))
                       '(1 1 1 1))
            8)
         )))

(define match4-how-many
  (lambda (re_given ns_given)
    (letrec ([visit (lambda (re ns k)
                      ;; applies k to the rest of ns or returns 0
                      (cond
                       [(is-mt? re)
                        (k ns)]
                       [(is-atom? re)
                        (if (pair? ns)
                            (if (= (atom-1 re) (car ns))
                                (k (cdr ns))
                                0)
                            0)]
                       [(is-any? re)
                        (if (pair? ns)
                            (k (cdr ns))
                            0)]
                       [(is-seq? re)
                        (visit (seq-1 re) ns (lambda (nsp)
                                               (visit (seq-2 re) nsp k)))]
                       [(is-disj? re)
                        (+ (visit (disj-2 re) ns k)
                           (visit (disj-1 re) ns k))]
                       [(is-star? re)
                        (+ (k ns)
                           (visit (star-1 re) ns
                                  (lambda (nsp)
                                    (if (eq? ns nsp)
                                        0
                                        (visit re nsp k)))))]
                       [else
                        (errorf 'match4-how-many "not implemented yet")]))])
      (visit re_given ns_given (lambda (nsp) (if (null? nsp) 1 0))))))

(unless (unit-tests-for-match2-how-many match4-how-many)
  (printf "the tests for match2-how-many failed for match4-how-many"))
(unless (unit-tests-for-match3-how-many match4-how-many)
  (printf "the tests for match3-how-many failed for match4-how-many"))
(unless (unit-tests-for-match4-how-many match4-how-many)
  (printf "the tests failed for match4-how-many"))

;;;;;;;;;;

;;; week-11_matching-regular-expressions.scm

"week-11_matching-regular-expressions.scm"
