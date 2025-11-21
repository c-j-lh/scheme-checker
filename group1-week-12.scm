;;; week-12_compiled-regular-expressions.scm
;;; Time-stamp: <2025-11-14 17:21:47 leheng>
;;; PLC 2025 - CS2104 2025-2026, Sem1 *)
;;; Olivier Danvy <danvy@nus.edu.sg>
;;; Group 1: Chuah Jia Jie <e0959959@u.nus.edu>; Yap Ho Wen <howenyap@u.nus.edu>; Yang Yaqi <e1522259@u.nus.edu>; Wang Aleah <e1518514@u.nus.edu>; Goussanou Clara <e1535596@u.nus.edu>; Zhang Kelly <e1518548@u.nus.edu>; Wu Lucy Shuai <e1521751@u.nus.edu>; Chieu Le Heng <e0454394@u.nus.edu>

;;;;;;;;;;

;;; Exercise 03

;;;;;
(define re03_1
  '(mt))

(define test_c_re03_1
  (lambda (candidate)
    (and (candidate '())
         (not (candidate '(())))
         (not (candidate '(1))))))

(define c_re03_1
  (lambda (ns)
    (null? ns)))

(unless (test_c_re03_1 c_re03_1)
  (printf "test_c_re03_03_1 failed"))

;;;;;
(define re03_2
  '(atom 10))

(define test_c_re03_2
  (lambda (candidate)
    (and (candidate '(10))
         (not (candidate '()))
         (not (candidate '(10 10)))
         (not (candidate '(1 0)))
         (not (candidate '(11 12))))))

(define c_re03_2
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 10)
         (null? (cdr ns)))))

(unless (test_c_re03_2 c_re03_2)
  (printf "test_c_re03_2 failed"))

;;;;;

(define re03_3
  '(seq (atom 10) (atom 20)))

(define test_c_re03_3
  (lambda (candidate)
    (and (candidate '(10 20))
         (not (candidate '()))
         (not (candidate '(10 20 30)))
         (not (candidate '(10)))
         (not (candidate '(20)))
         (not (candidate '(20 10))))))


(define c_re03_3
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 10)
         (let ([ns (cdr ns)])
           (and (pair? ns)
                (= (car ns) 20)
                (null? (cdr ns)))))))

(unless (test_c_re03_3 c_re03_3)
  (printf "test_c_re03_3 failed"))

;;;;;

(define re03_4
  '(seq (atom 10) (seq (atom 20) (atom 30))))

(define test_c_re03_4
  (lambda (candidate)
    (and (candidate '(10 20 30))
         (not (candidate '()))
         (not (candidate '(10 20)))
         (not (candidate '(10)))
         (not (candidate '(20 30)))
         (not (candidate '(10 20 30 40))))))

(define c_re03_4
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 10)
         (let ([ns (cdr ns)])
           (and (pair? ns)
                (= (car ns) 20)
                (let ([ns (cdr ns)])
                  (and (pair? ns)
                       (= (car ns) 30)
                       (null? (cdr ns)))))))))


(unless (test_c_re03_4 c_re03_4)
  (printf "test_c_re03_4 failed"))

;;;;;

(define re03_5
  '(seq (atom 10) (seq (atom 20) (seq (atom 30) (atom 40)))))

(define test_c_re03_5
  (lambda (candidate)
    (and (candidate '(10 20 30 40))
         (not (candidate '()))
         (not (candidate '(10 20 30)))
         (not (candidate '(10)))
         (not (candidate '(20 30 40)))
         (not (candidate '(10 20 30 40 50))))))

(define c_re03_5
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 10)
         (let ([ns (cdr ns)])
           (and (pair? ns)
                (= (car ns) 20)
                (let ([ns (cdr ns)])
                  (and (pair? ns)
                       (= (car ns) 30)
                       (let ([ns (cdr ns)])
                         (and (pair? ns)
                              (= (car ns) 40)
                              (null? (cdr ns)))))))))))


(unless (test_c_re03_5 c_re03_5)
  (printf "test_c_re03_5 failed"))

;;;;;

(define re03_6
  '(disj (atom 10) (atom 11)))
(define test_c_re03_6
  (lambda (candidate)
    (and (candidate '(10))
         (candidate '(11))
         (not (candidate '()))
         (not (candidate '(12)))
         (not (candidate '(10 11)))
         (not (candidate '(11 10))))))

(define c_re03_6
  (lambda (ns)
    (and (pair? ns)
         (let ([n (car ns)])
           (or (= n 10)
               (= n 11)))
         (null? (cdr ns)))))

(unless (test_c_re03_6 c_re03_6)
  (printf "test_c_re03_6 failed"))


;;;;;

(define re03_7
  '(seq (atom 10) (seq (disj (atom 20) (atom 21)) (seq (atom 30) (atom 40)))))

(define test_c_re03_7
  (lambda (candidate)
    (and (candidate '(10 20 30 40))
         (candidate '(10 21 30 40))
         (not (candidate '()))
         (not (candidate '(10 30 40)))
         (not (candidate '(10 20 21 30 40)))
         (not (candidate '(10 21 30 40 50)))
         (not (candidate '(10 20 30))))))

(define c_re03_7
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 10)
         (let ([ns (cdr ns)])
           (and (pair? ns)
                (let ([n (car ns)]
                      [ns (cdr ns)])
                  (and (or (= n 20)
                           (= n 21))
                       (pair? ns)
                       (= (car ns) 30)
                       (let ([ns (cdr ns)])
                         (and (pair? ns)
                              (= (car ns) 40)
                              (null? (cdr ns)))))))))))

(unless (test_c_re03_7 c_re03_7)
  (printf "test_c_re03_7 failed"))

;;;;;

(define re03_8
  '(seq (atom 10) (seq (disj (atom 20) (seq (atom 21) (atom 22))) (seq (atom 30) (atom 40)))))

(define test_c_re03_8
  (lambda (candidate)
    (and (candidate '(10 20 30 40))
         (candidate '(10 21 22 30 40))
         (not (candidate '()))
         (not (candidate '(20 30 40)))
         (not (candidate '(10 30 40)))
         (not (candidate '(10 20 21 30 40)))
         (not (candidate '(10 20 21 22 30 40)))
         (not (candidate '(10 20 30 40 50)))
         (not (candidate '(10 21 22 30))))))

(define c_re03_8
  (lambda (ns)
    (let ([k (lambda (ns)
               (and (pair? ns)
                    (= (car ns) 30)
                    (let ([ns (cdr ns)])
                      (and (pair? ns)
                           (= (car ns) 40)
                           (null? (cdr ns))))))])
      (and (pair? ns)
           (= (car ns) 10)
           (let ([ns (cdr ns)])
             (and (pair? ns)
                  (let ([n (car ns)]
                        [ns (cdr ns)])
                    (or (and (= n 20)
                             (k ns))
                        (and (= n 21)
                             (pair? ns)
                             (= (car ns) 22)
                             (k (cdr ns)))))))))))

(unless (test_c_re03_8 c_re03_8)
  (printf "test_c_re03_8 failed"))

;;;;;;;;;;

;;; Exercise 04

;;;;;

(define test_c_re04_1
  (lambda (candidate)
    (and (candidate '())
         (not (candidate '(1)))
         (not (candidate '(1 1))))))

(define re04_1
  '(star (mt)))

(define c_re04_1
  (lambda (ns)
    (null? ns)))

(unless (test_c_re04_1 c_re04_1)
  (printf "test_c_re04_1 failed"))

;;;;;

(define test_c_re04_2
  (lambda (candidate)
    (and (candidate '())
         (candidate '(20))
         (candidate '(20 20))
         (candidate '(20 20 20))
         (not (candidate '(21)))
         (not (candidate '(20 21))))))

(define re04_2
  '(star (atom 20)))

(define c_re04_2
  (lambda (ns)
    (or (null? ns)
        (and (pair? ns)
             (= (car ns) 20)
             (let ([ns (cdr ns)])
               (c_re04_2 ns))))))

(unless (test_c_re04_2 c_re04_2)
  (printf "test_c_re04_2 failed"))

;;;;;

(define test_c_re04_3
  (lambda (candidate)
    (and (candidate '(10 30 40 50))
         (candidate '(10 20 30 40 50))
         (candidate '(10 20 20 20 30 40 50))
         (not (candidate '(10 21 30 40 50)))
         (not (candidate '(10 20 21 30 40 50)))
         )))

(define re04_3
  '(seq (atom 10) (seq (star (atom 20)) (seq (atom 30) (seq (atom 40) (atom 50))))))

(define c_re04_3
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 10)
         (let ([ns (cdr ns)])
           (letrec ([loop (lambda (ns)
                            (or (k ns)
                                (and (pair? ns)
                                     (= (car ns) 20)
                                     (let ([ns (cdr ns)])
                                       (loop ns)))))]
                    [k (lambda (ns)
                         (and (pair? ns)
                              (= (car ns) 30)
                              (let ([ns (cdr ns)])
                                (and (pair? ns)
                                     (= (car ns) 40)
                                     (let ([ns (cdr ns)])
                                       (and (pair? ns)
                                            (= (car ns) 50)
                                            (null? (cdr ns))))))))])
             (loop ns))))))

(unless (test_c_re04_3 c_re04_3)
  (printf "test_c_re04_3 failed"))

;;;;;

(define test_c_re04_4
  (lambda (candidate)
    (and (candidate '(10 30))
         (candidate '(10 20 30))
         (candidate '(10 21 30))
         (candidate '(10 20 21 21 30))
         (not (candidate '(10 22 30)))
         (not (candidate '(10 20 21 20 22 30)))
         )))

(define re04_4
  '(seq (atom 10) (seq (star (disj (atom 20) (atom 21))) (atom 30))))

(define c_re04_4
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 10)
         (let ([ns (cdr ns)])
           (letrec ([loop (lambda (ns)
                            (or (k ns)
                                (and (pair? ns)
                                     (let ([n (car ns)]
                                           [ns (cdr ns)])
                                       (and (or (= n 20)
                                                (= n 21))
                                            (loop ns))))))]
                    [k (lambda (ns)
                         (and (pair? ns)
                              (= (car ns) 30)
                              (null? (cdr ns))))])
             (loop ns))))))

(unless (test_c_re04_4 c_re04_4)
  (printf "test_c_re04_4 failed"))

;;;;;

(define test_c_re04_5
  (lambda (candidate)
    (and (candidate '(10 30))
         (candidate '(10 20 30))
         (candidate '(10 20 21 22 20 20 30))
         (not (candidate '(10 21 30)))
         (not (candidate '(10 20 21 20 22 30)))
         )))

(define re04_5
  '(seq (atom 10) (seq (star (disj (atom 20) (seq (atom 21) (atom 22)))) (atom 30))))

(define c_re04_5
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 10)
         (let ([ns (cdr ns)])
           (letrec ([loop (lambda (ns)
                            (or (k ns)
                                (and (pair? ns)
                                     (let ([n (car ns)]
                                           [ns (cdr ns)])
                                       (or (and (= n 20)
                                                (loop ns))
                                           (and (= n 21)
                                                (pair? ns)
                                                (= (car ns) 22)
                                                (loop (cdr ns))))))))]
                    [k (lambda (ns)
                         (and (pair? ns)
                              (= (car ns) 30)
                              (null? (cdr ns))))])
             (loop ns))))))


(unless (test_c_re04_5 c_re04_5)
  (printf "test_c_re04_5 failed"))

;;;;;

(define test_c_re04_6
  (lambda (candidate)
    (and (candidate '())
         (candidate '(10))
         (candidate '(10 10 10))
         (candidate '(20))
         (candidate '(10 20))
         (candidate '(10 10 10 20))
         (candidate '(20 20 20))
         (candidate '(10 20 20 20))
         (candidate '(10 10 10 20 20 20))
         (not (candidate '(10 20 10)))
         (not (candidate '(20 10)))
         (not (candidate '(11))) )))

(define re04_6
  '(seq (star (atom 10)) (star (atom 20))))

(define c_re04_6
  (lambda (ns)
    (letrec ([loop (lambda (ns)
                     (or (k ns)
                         (and (pair? ns)
                              (= (car ns) 10)
                              (let ([ns (cdr ns)])
                                (loop ns)))))]
             [k (lambda (ns)
                  (or (null? ns)
                      (and (pair? ns)
                           (= (car ns) 20)
                           (k (cdr ns)))))])
      (loop ns))))

(unless (test_c_re04_6 c_re04_6)
  (printf "test_c_re04_6 failed"))

;;;;;

(define test_c_re04_7
  (lambda (candidate)
    (and (candidate '())
         (candidate '(10))
         (candidate '(10 10 10))
         (candidate '(20 10 10 20 20))
         (not (candidate '(10 20 11)))
         (not (candidate '(11))))))

(define re04_7
  '(star (disj (atom 10) (atom 20))))

(define c_re04_7
  (lambda (ns)
    (or (null? ns)
        (and (pair? ns)
             (let ([n (car ns)]
                   [ns (cdr ns)])
               (and (or (= n 10)
                        (= n 20))
                    (c_re04_7 ns)))))))

(unless (test_c_re04_7 c_re04_7)
  (printf "test_c_re04_7 failed"))

;;;;;;;;;;

;;; Exercise 05

;;;;;

(define test_c_re05_1
  (lambda (candidate)
    (and (candidate '())
         (not (candidate '(1)))
         (not (candidate '(1 1))))))

(define re05_1
  '(plus (mt)))

(define c_re05_1
  (lambda (ns)
    (null? ns)))

(unless (test_c_re05_1 c_re05_1)
  (printf "test_c_re05_1 failed"))

;;;;;

(define test_c_re05_2
  (lambda (candidate)
    (and (candidate '(20))
         (candidate '(20 20))
         (candidate '(20 20 20))
         (not (candidate '()))
         (not (candidate '(21)))
         (not (candidate '(20 21))))))

(define re05_2
  '(plus (atom 20)))

(define c_re05_2
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 20)
         (let ([ns (cdr ns)])
           (or (c_re04_2 ns)
               (null? ns))))))

(unless (test_c_re05_2 c_re05_2)
  (printf "test_c_re05_2 failed"))

;;;;;

(define test_c_re05_3
  (lambda (candidate)
    (and (candidate '(10 20 30 40 50))
         (candidate '(10 20 20 20 30 40 50))
         (not (candidate '(10 30 40 50)))
         (not (candidate '(10 21 30 40 50)))
         (not (candidate '(10 20 21 30 40 50)))
         )))

(define re05_3
  '(seq (atom 10) (seq (plus (atom 20)) (seq (atom 30) (seq (atom 40) (atom 50))))))

(define c_re05_3
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 10)
         (let ([ns (cdr ns)])
           (letrec ([loop (lambda (ns)
                            (and (pair? ns)
                                 (= (car ns) 20)
                                 (let ([ns (cdr ns)])
                                   (or (loop ns)
                                       (k ns)))))]
                    [k (lambda (ns)
                         (and (pair? ns)
                              (= (car ns) 30)
                              (let ([ns (cdr ns)])
                                (and (pair? ns)
                                     (= (car ns) 40)
                                     (let ([ns (cdr ns)])
                                       (and (pair? ns)
                                            (= (car ns) 50)
                                            (null? (cdr ns))))))))])
             (loop ns))))))

(unless (test_c_re05_3 c_re05_3)
  (printf "test_c_re05_3 failed"))

;;;;;

(define test_c_re05_4
  (lambda (candidate)
    (and (candidate '(10 20 30))
         (candidate '(10 21 30))
         (candidate '(10 20 20 20 20 30))
         (candidate '(10 21 21 21 21 21 30))
         (candidate '(10 20 21 21 30))
         (not (candidate '(10 30)))
         (not (candidate '(10 22 30)))
         (not (candidate '(10 20 21 20 22 30)))
         )))

(define re05_4
  '(seq (atom 10) (seq (plus (disj (atom 20) (atom 21))) (atom 30))))

(define c_re05_4
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 10)
         (let ([ns (cdr ns)])
           (letrec ([loop (lambda (ns)
                            (and (pair? ns)
                                 (let ([n (car ns)]
                                       [ns (cdr ns)])
                                   (and (or (= n 20)
                                            (= n 21))
                                        (or (loop ns)
                                            (k ns))))))]
                    [k (lambda (ns)
                         (and (pair? ns)
                              (= (car ns) 30)
                              (null? (cdr ns))))])
             (loop ns))))))

(unless (test_c_re05_4 c_re05_4)
  (printf "test_c_re05_4 failed"))

;;;;;

(define test_c_re05_5
  (lambda (candidate)
    (and (candidate '(10 20 30))
         (candidate '(10 21 22 30))
         (candidate '(10 20 21 22 20 20 30))
         (not (candidate '(10 30)))
         (not (candidate '(10 21 30)))
         (not (candidate '(10 22 30)))
         (not (candidate '(10 20 21 20 22 30)))
         )))


(define re05_5
  '(seq (atom 10) (seq (plus (disj (atom 20) (seq (atom 21) (atom 22)))) (atom 30))))


(define c_re05_5
  (lambda (ns)
    (and (pair? ns)
         (= (car ns) 10)
         (let ([ns (cdr ns)])
           (letrec ([loop (lambda (ns)
                            (and (pair? ns)
                                 (let ([n (car ns)]
                                       [ns (cdr ns)])
                                   (or (and (= n 20)
                                            (or (loop ns)
                                                (k ns)))
                                       (and (= n 21)
                                            (pair? ns)
                                            (= (car ns) 22)
                                            (let ([ns (cdr ns)])
                                              (or (loop ns)
                                                  (k ns))))))))]
                    [k (lambda (ns)
                         (and (pair? ns)
                              (= (car ns) 30)
                              (null? (cdr ns))))])
             (loop ns))))))


(unless (test_c_re05_5 c_re05_5)
  (printf "test_c_re05_5 failed"))

;;;;;

(define test_c_re05_6
  (lambda (candidate)
    (and (candidate '(10 20))
         (candidate '(10 10 10 20))
         (candidate '(10 20 20 20))
         (candidate '(10 10 10 20 20 20))
         (not (candidate '()))
         (not (candidate '(10)))
         (not (candidate '(20)))
         (not (candidate '(10 10 10)))
         (not (candidate '(20 20 20)))
         (not (candidate '(10 20 10)))
         (not (candidate '(20 10)))
         (not (candidate '(11))))))

(define re05_6
  '(seq (plus (atom 10)) (plus (atom 20))))

(define c_re05_6
  (lambda (ns)
    (letrec ([loop (lambda (ns)
                     (and (pair? ns)
                          (= (car ns) 10)
                          (let ([ns (cdr ns)])
                            (or (loop ns)
                                (k ns)))))]
             [k (lambda (ns)
                  (and (pair? ns)
                       (= (car ns) 20)
                       (let ([ns (cdr ns)])
                         (or (k ns)
                             (null? ns)))))])
      (loop ns))))

(unless (test_c_re05_6 c_re05_6)
  (printf "test_c_re05_6 failed"))

;;;;;

(define test_c_re05_7
  (lambda (candidate)
    (and (candidate '(10))
         (candidate '(20))
         (candidate '(10 10 10))
         (candidate '(20 20 20))
         (candidate '(20 10 10 20 20))
         (not (candidate '()))
         (not (candidate '(10 20 11)))
         (not (candidate '(11))))))

(define re05_7
  '(plus (disj (atom 10) (atom 20))))

(define c_re05_7
  (lambda (ns)
    (and (pair? ns)
         (let ([n (car ns)]
               [ns (cdr ns)])
           (and (or (= n 10)
                    (= n 20))
                (or (c_re05_7 ns)
                    (null? ns)))))))

(unless (test_c_re05_7 c_re05_7)
  (printf "test_c_re05_7 failed"))

;;;;;;;;;;

;;; end of week-12_compiled-regular-expressions.scm

"week-12_compiled-regular-expressions.scm"
