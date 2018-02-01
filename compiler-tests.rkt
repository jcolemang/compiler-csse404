#lang racket

(require rackunit "compiler.rkt")
(require rackunit/text-ui)
(require "utilities.rkt")

(define graph-tests
  (let* ((g1 (add-edge 'a 'b
                       (add-node 'a
                                 (add-node 'b
                                           (make-graph)))))
         (g2 (add-node 'c g1))
         (g3 (add-edges '((a b))
                        (make-graph))))

    (test-suite
     "Tests to make sure my graph implementation isn't broken"

     (check-equal? (adjacent 'a g1) '(b))
     (check-equal? (adjacent 'b g1) '(a))
     (check-equal? (adjacent 'b g2) '(a))

     (check-equal? (adjacent 'a g3) '(b))
     (check-equal? (adjacent 'b g3) '(a)))))

(define sort-sym-ls
  (lambda (lss)
    (map (lambda (ls)
           (sort ls symbol<?))
         lss)))

(define live-after-tests
  (let ((book-prog '(program
                     (v w x y z t.1 t.2)
                     ((movq (int 1) (var v))
                      (movq (int 46) (var w))
                      (movq (var v) (var x))
                      (addq (int 7) (var x))
                      (movq (var x) (var y))
                      (addq (int 4) (var y))
                      (movq (var x) (var z))
                      (addq (var w) (var z))
                      (movq (var y) (var t.1))
                      (neg (var t.1))
                      (movq (var z) (var t.2))
                      (addq (var t.1) (var t.2))
                      (movq (var t.2) (reg rax)))))
        (book-live-after-sets
         (sort-sym-ls '((v)
                        (v w)
                        (w x)
                        (w x)
                        (w x y)
                        (w x y)
                        (w y z)
                        (y z)
                        (t.1 z)
                        (t.1 z)
                        (t.1 t.2)
                        (t.2)
                        ())))
        (smaller-prog
         '(program
           (a b)
           ((movq (int 5) (var a))
            (movq (int 2) (var b))
            (addq (var a) (var b)))))
        (smaller-live-after
         (sort-sym-ls '((a)
                        (a b)
                        ())))
        (smaller-prog-two
         '(program
           (a)
           ((movq (int 5) (var a))
            (neg (var a)))))
        (smaller-live-after-two
         (sort-sym-ls '((a) ())))
        (mov-prog
         '(program
           (a b)
           ((movq (int 5) (var a))
            (movq (int 10) (var b))
            (movq (var a) (var b)))))
        (mov-live-after
         '((a) (a) ())))
    (test-suite
     "Live after function tests"

     (check-equal? (sort-sym-ls
                    (map cadr
                         (live-after-sets (caddr smaller-prog))))
                   smaller-live-after)
     (check-equal? (sort-sym-ls
                    (map cadr
                         (live-after-sets (caddr smaller-prog-two))))
                   smaller-live-after-two)
     (check-equal? (map cadr
                        (live-after-sets (caddr mov-prog)))
                   mov-live-after)
     (check-equal? (sort-sym-ls
                    (map cadr
                         (live-after-sets (caddr book-prog))))
                   book-live-after-sets)
     )))

(define interference-graph-tests
  (let ((book-prog '(program
                     (v w x y z t.1 t.2)
                     ((movq (int 1) (var v))
                      (movq (int 46) (var w))
                      (movq (var v) (var x))
                      (addq (int 7) (var x))
                      (movq (var x) (var y))
                      (addq (int 4) (var y))
                      (movq (var x) (var z))
                      (addq (var w) (var z))
                      (movq (var y) (var t.1))
                      (neg (var t.1))
                      (movq (var z) (var t.2))
                      (addq (var t.1) (var t.2))
                      (movq (var t.2) (reg rax)))))
        (book-graph (add-edges '((v w)
                                 (w x)
                                 (w y)
                                 (w z)
                                 (y z)
                                 (z t.1)
                                 (t.1 t.2))
                               (make-graph)))
        (small-instrs '((movq (int 1) (var a))    ; ((a)
                        (movq (int 2) (var b))    ;  (a b)
                        (addq (var a) (var b)))) ;  ())
        (small-graph '((a b)
                       (b a))))

    (test-suite
     "Tests for interference graph construction"
     (check-pred (lambda (x) (graph-equal? (car x) (cadr x)))
                 (list (construct-graph (live-after-sets small-instrs))
                       small-graph))

     (check-pred (lambda (x) (graph-equal? (car x) (cadr x)))
                 (list (construct-graph (live-after-sets (caddr book-prog)))
                       book-graph))
     )))


(define my-syntax-tests
  (test-suite
   "Testing the absurd syntax I am adding"

   (check-equal? (--> 1)
                 1)
   (check-equal? (--> x <- 5
                      x)
                 5)
   (check-equal? (--> x <- 5
                      x <- 10
                      x)
                 10)
    ))

(define my-helper-tests
  (test-suite
   "Testing my own absurd functions"

   ;; (check-equal? (get-param-locations 1)
   ;;               '((reg rdi)))
   ;; (check-equal? (get-param-locations 6)
   ;;               '((reg rdi)
   ;;                 (reg rsi)
   ;;                 (reg rdx)
   ;;                 (reg rcx)
   ;;                 (reg r8)
   ;;                 (reg r9)))
   ;; (check-equal? (get-param-locations 7)
   ;;               '((reg rdi)
   ;;                 (reg rsi)
   ;;                 (reg rdx)
   ;;                 (reg rcx)
   ;;                 (reg r8)
   ;;                 (reg r9)
   ;;                 (deref rbp -8)))
   ;; (check-equal? (get-param-locations 10)
   ;;               '((reg rdi)
   ;;                 (reg rsi)
   ;;                 (reg rdx)
   ;;                 (reg rcx)
   ;;                 (reg r8)
   ;;                 (reg r9)
   ;;                 (deref rbp -8)
   ;;                 (deref rbp -16)
   ;;                 (deref rbp -24)
   ;;                 (deref rbp -32)))

   (check-equal? (concat-map identity
                             '((1)
                               (2)
                               (3)))
                 '(1 2 3))))

(define set-tests
  (test-suite
   "Testing my set implementation"

   (check-equal? (set-union (set-singleton 'a)
                            (set-singleton 'a))
                 (set-singleton 'a))

   (check-equal? (set-difference (set-singleton 'a)
                                 (set-singleton 'a))
                 (empty-set))

   (check-equal? (set-remove 'a (set-singleton 'a))
                 (empty-set))

   (check-equal? (set-remove 'b (set-singleton 'a))
                 (set-singleton 'a))
   ))

(display "~~~~~~~~~~~~~~~~~~~~~~~~~")
(newline)
(display "Running my personal tests")
(newline)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~")
(newline)


(run-tests graph-tests)
(run-tests live-after-tests)
(run-tests my-syntax-tests)
(run-tests my-helper-tests)
(run-tests set-tests)
(run-tests interference-graph-tests)

(newline)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~")
(newline)
(display "Running the given test cases")
(newline)
(display "~~~~~~~~~~~~~~~~~~~~~~~~~")
(newline)

(let ((compiler (reverse `((print-instructions  ,print-instructions  nothing)
                           (patch-instructions  ,patch-instructions  nothing)
                           (add-bookkeeping     ,add-bookkeeping     nothing)
                           (lower-conditionals  ,lower-conditionals  nothing)
                           (add-register-saves  ,add-register-saves  nothing)
                           (allocate-registers  ,allocate-registers  nothing)
                           (manage-root-stack   ,manage-root-stack   nothing)
                           (build-interference  ,build-interference  nothing)
                           (uncover-live        ,uncover-live        nothing)
                           (select-instructions ,select-instructions nothing)
                           (flatten             ,flatten             nothing)
                           (expose-allocation   ,expose-allocation   nothing)
                           (convert-to-closures ,convert-to-closures nothing)
                           (reveal-functions    ,reveal-functions    nothing)
                           (uniquify            ,uniquify            nothing))))
      (type-checker typecheck-R4))
  ;; (compiler-tests "r1-compiler"
  ;;                 type-checker
  ;;                 compiler
  ;;                 "r1"
  ;;                 (range 1 49))
  ;; (compiler-tests "r2-compiler"
  ;;                 type-checker
  ;;                 compiler
  ;;                 "r2"
  ;;                 (range 1 60))

  ;; (compiler-tests "r3-compiler"
  ;;                 type-checker
  ;;                 compiler
  ;;                 "r3"
  ;;                 (range 1 36))

  ;; (compiler-tests "r4-compiler"
  ;;                 type-checker
  ;;                 compiler
  ;;                 "r4"
  ;;                 (range 1 39))

  (compiler-tests "r5-compiler"
                  type-checker
                  compiler
                  "r5"
                  (range 1 13)))
