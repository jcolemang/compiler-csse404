
#lang racket

(require "compiler.rkt")


;; racket compiler.rkt > test.s
;; gcc -g -std=c99 runtime.o test.s
;; ./a.out

;; (let ((test-prog '(program (+ 1 2))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program
;;                    (+ 1(+ 1(+ 1(+ 1(+ 1(+ 1(+ 1(+ 1 (+ 1
;;                       (+ 1
;;                          (+ 1
;;                             (+ 1
;;                                (+ 1
;;                                   (+ 1
;;                                      (+ 1 1))))))))))))))))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program (read))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program (let ((x 1)) (+ x 41)))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program (let ((x 41)) (+ 1 x)))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program (+ 47 (- 5)))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program (if (not (eq? 1 2)) 1 2))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program (if (eq? (let ([x 42]) (if (eq? x 42) x 20)) 42) 42 777))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program (if (eq? 1 2) 1 2))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program (if (and #t #t) 1 2))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program (if (> 1 2) 1 2))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program (+ 1 42))))
;;   (display (run-all test-prog)))

;; (let ((test-prog '(program (vector-ref (vector 1) 0))))
;;   (display (run-all test-prog)))
;; (let ((test-prog '(program (vector-ref vector-ref (vector (vector 42 32)) 0) 1))))
;;   (display (run-all test-prog)))
(let ((test-prog '(program
                   (let ([v0 (vector 0 1 2 3 4 5 6 7 8 9
                                     10 11 12 13 14 15 16 17 18 19
                                     20 21 22 23 24 25 26 27 28 29
                                     30 31 32 33 34 35 36 37 38 39
                                     40 41 42 43 44 45 46 47 48 49)])
                     (let ([v1 (vector 0 1 2 3 4 5 6 7 8 9
                                       10 11 12 13 14 15 16 17 18 19
                                       20 21 22 23 24 25 26 27 28 29
                                       30 31 32 33 34 35 36 37 38 39
                                       40 41 42 43 44 45 46 47 48 49)])
                       (let ([v2 (vector 0 1 2 3 4 5 6 7 8 9
                                         10 11 12 13 14 15 16 17 18 19
                                         20 21 22 23 24 25 26 27 28 29
                                         30 31 32 33 34 35 36 37 38 39
                                         40 41 42 43 44 45 46 47 48 49)])
                         (vector-ref v0 42))))

                   )))
  (display (run-all test-prog)))

;; (let ((test-prog '(program (let ([a 1])
;;                              (let ([b 2])
;;                                (let ([x (if (eq? (read) 0)
;;                                             (- a)
;;                                             b)])
;;                                  (+ x 10)))))))
;;   (display (run-all test-prog)))


;; `(1 2 3 ,@(map add1 '(4 5 6))) -> (1 2 3 4 5 6)
