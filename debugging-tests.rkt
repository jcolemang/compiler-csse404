
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
(let ((test-prog '(program (vector-ref (vector-ref (vector (vector 42)) 0) 0))))
  (display (run-all test-prog)))

;; (let ((test-prog '(program (let ([a 1])
;;                              (let ([b 2])
;;                                (let ([x (if (eq? (read) 0)
;;                                             (- a)
;;                                             b)])
;;                                  (+ x 10)))))))
;;   (display (run-all test-prog)))


;; `(1 2 3 ,@(map add1 '(4 5 6))) -> (1 2 3 4 5 6)
