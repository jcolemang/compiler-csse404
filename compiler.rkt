#lang racket

;; NOTE Look into using basic blocks in the compiler

;; NOTE Look into the differences between generational garbage collecting and
;; copy collecting. Check out some of the references on page 81

;; NOTE Look into using type information in garbage collecting. See page 82


(require "utilities.rkt")
(require racket/trace)

;; My helpers

(define-syntax >>=
  (syntax-rules (<-)
    [(_ x <- exp rest ...)
     (let ((x exp))
       (if x
           (>>= rest ...)
           x))]
    [(_ x rest1 rest2 ...)
     (if x
         (>>= rest1 rest2 ...)
         x)]
    [(_ x) x]))

(define-syntax -->
  (syntax-rules (<-)
    [(_ x <- exp rest ...)
     (let ((x exp))
       (--> rest ...))]
    [(_ x next rest ...)
     (begin x
            (--> next rest ...))]
    [(_ x) x]))

(define-syntax expand-fs
  (syntax-rules ()
    [(_ x) x]
    [(_ x func rest ...)
     (func (expand-fs x rest ...))]))

;; absolutely does not need to be a macro but I am trying to get into the
;; holiday spirit.
(define-syntax compose
  (syntax-rules ()
    [(_ f ...)
     (lambda (x)
       (expand-fs x f ...))]))

(define join
  (lambda (ls)
    (apply append ls)))

(define concat-map
  (lambda (f . ls)
    (join (apply map f ls))))

(define intercalate
  (lambda (item ls)
    (match ls
      [`() ls]
      [`(,_) ls]
      [`(,elem ,rest ...)
       `(,elem ,item ,@(intercalate item rest))])))

(define set-singleton
  (lambda (element)
    (set-insert element (empty-set))))

(define set-insert
  (lambda (element set)
    (match `(,element ,set)
      [`(,x ,(list x rest ...)) set]
      [`(,x ()) `(,x)]
      [`(,_ (,x ,rest ...))
       (cons x (set-insert element rest))])))

(define set-remove
  (lambda (element set)
    (cond
     [(null? set) (empty-set)]
     [(equal? element (car set)) (cdr set)]
     [else (cons (car set) (set-remove element (cdr set)))])))

(define set-equal?
  (lambda (set1 set2)
    (or (and (null? set1)
             (null? set2))
        (and (= (length set1) (length set2))
             (set-equal? (set-remove (car set1) set1)
                         (set-remove (car set1) set2))))))

(define set-difference
  (lambda (set elems)
    (foldl set-remove set elems)))

(define empty-set
  (lambda ()
    '()))

(define make-graph
  (lambda ()
    '()))

(define get-nodes
  (lambda (graph)
    (map car graph)))

(define graph-equal?
  (lambda (g1 g2)
    (and (= (length g1) (length g2))
         (set-equal? (map car g1)
                     (map car g2))
         (let ((edges (map car g1)))
           (andmap (lambda (x)
                     (set-equal? (adjacent x g1)
                                 (adjacent x g2)))
                   edges)))))

(define add-edge
  (lambda (from to graph)
    (add-edge-help from
                   to
                   (add-edge-help to
                                  from
                                  graph))))


(define add-edge-help
  (lambda (from to graph)
    (map (lambda (node)
           (if (equal? from (car node))
               (cons (car node) (set-insert to (cdr node)))
               (if (equal? to (car node))
                   (cons (car node) (set-insert from (cdr node)))
                   node)))
         graph)))

(define add-edges
  (case-lambda
    [(adjs graph)
     (foldl (lambda (adj g)
              (add-edge (car adj)
                        (cadr adj)
                        g))
            (add-nodes (append (map car adjs)
                               (map cadr adjs))
                       graph)
            adjs)]
    [(froms to graph)
     (foldl (lambda (node g)
              (add-edge node to g))
            (add-nodes (cons to froms) graph)
            froms)]))

(define add-node
  (lambda (name graph)
    (if (assv name graph)
        graph
        (cons (cons name '()) graph))))

(define add-nodes
  (lambda (nodes graph)
    (foldl add-node graph nodes)))

(define adjacent
  (lambda (name graph)
    (cdr (assv name graph))))

(define color-graph
  (letrec ((get-id car)
           (get-color cadr)
           (get-impossible-colors caddr)
           (update-color
            (lambda (node new-color)
              (match node
                [(list _ (var bad-colors))
                 `(,new-color ,bad-colors)])))
           (update-exclusion
            (lambda (node bad-color)
              (match node
                [(list (var color)
                       (var bad-colors))
                 `(,color
                   ,(sort (set-insert bad-color bad-colors) <))])))
           (get-saturation
            (lambda (node graph node-colors)
              (length (get-impossible-colors node))))

              ;; (foldl + 0 (map (lambda (x)
              ;;                   (let ((color-node (hash-ref node-colors x)))
              ;;                     (if (get-color color-node) 1 0)))
              ;;                 (adjacent (get-id node) graph)))))
           (get-max-saturated-node
            (lambda (graph uncolored node-colors)
              (let ((max-sat
                     (argmax (lambda (n)
                               (get-saturation (cons n
                                                     (hash-ref node-colors n))
                                               graph
                                               node-colors))
                             uncolored)))
                (cons max-sat
                      (hash-ref
                       node-colors
                       max-sat)))))
           (get-lowest-helper
            (lambda (bad-colors curr)
              (cond
               [(null? bad-colors)
                curr]
               [(= curr (car bad-colors))
                (get-lowest-helper (cdr bad-colors) (add1 curr))]
               [else curr])))
           (get-lowest-color
            (lambda (node)
              (get-lowest-helper (get-impossible-colors node) 0)))
           (color-helper
            (lambda (graph uncolored nodes)
              (if (null? uncolored)
                  nodes
                  (let* ((max-saturated (get-max-saturated-node graph
                                                                uncolored
                                                                nodes))
                         (lowest-color (get-lowest-color max-saturated)))
                    (color-helper
                     graph
                     (remove (get-id max-saturated)
                             uncolored)
                     (let ((with-exclusions
                             (foldl (lambda (n colored-nodes)
                                      (hash-update
                                       colored-nodes
                                       n
                                       (lambda (node)
                                         (update-exclusion node
                                                           lowest-color))))
                                    nodes
                                    (adjacent (get-id max-saturated)
                                              graph))))
                       (hash-update with-exclusions
                                    (get-id max-saturated)
                                    (lambda (node)
                                      (update-color node
                                                    lowest-color))))))))))
    (lambda (graph)
      (let ((result
             (color-helper graph
                           (get-nodes graph)
                           (make-immutable-hasheqv (map (lambda (n)
                                                          `(,n #f ()))
                                                        (get-nodes graph))))))
        (hash-map result
                  (lambda (iden node)
                    (cons iden (car node))))))))




;; Actual compiler

(define variable-lookup
  (lambda (var assoc-list)
    (>>= (not (null? assoc-list))
         assoc-var <- (assq var assoc-list)
         (cdr assoc-var))))

(struct u-state
  (assoc-list curr-num))

(define (uniquify state)
  (lambda (exp)
    (let ((assoc-list (u-state-assoc-list state))
          (same-state-uniquify (uniquify state)))
      (match exp
        [`(program ,exps ...)
         `(program ,(map same-state-uniquify exps))]
        [(? integer?) exp]
        [(? boolean?) exp]
        [(? symbol?)
         (or (variable-lookup exp assoc-list)
             (error 'uniquify "Unbound variable: ~a" exp))]
        [`(if ,test ,true ,false)
         `(if ,(same-state-uniquify test)
              ,(same-state-uniquify true)
              ,(same-state-uniquify false))]
        [`(let ([,var ,val])
            ,body)
         (let ([new-sym (gensym)])
           `(let ([,new-sym ,(same-state-uniquify val)])
              ,((uniquify (struct-copy u-state
                                       state
                                       (assoc-list (cons (cons var new-sym)
                                                         assoc-list))))
                body)))]
        [`(,operator ,operands ...)
         (let ((f (uniquify state)))
           `(,(f operator)
             ,@(map f operands)))]))))

(define flatten
  (lambda (exp)
    (match exp
      [`(program ,exps)
       (let ((flat (map (lambda (exp)
                          (let-values ([(assignments value vars)
                                        (flatten exp)])
                            `(,assignments ,value ,vars)))
                        exps)))
         `(program ,(foldl set-insert
                           '()
                           (concat-map caddr flat))
                   ,(concat-map (lambda (assgns val)
                                  `(,@assgns
                                    (return ,val)))
                                (map car flat)
                                (map cadr flat))))]
      [(? integer?)
       (values '() exp '())]
      [(? boolean?)
       (values '() exp '())]
      [(? symbol?)
       (values '() exp `(,exp))]
      [`(let ((,x ,assgn)) ,body)
       (let-values ([(assgn-assignments assgn-value assgn-vars)
                     (flatten assgn)]
                    [(body-assignments body-value body-vars)
                     (flatten body)])
         (values `(,@assgn-assignments
                   (assign ,x ,assgn-value)
                   ,@body-assignments)
                 body-value
                 `(,@assgn-vars
                   ,@body-vars
                   ,x)))]
      [`(if ,test ,true ,false)
       (let-values ([(test-assgns test-value test-vars)
                     (flatten test)]
                    [(true-assgns true-value true-vars)
                     (flatten true)]
                    [(false-assgns false-value false-vars)
                     (flatten false)])
         (let ((if-var (gensym)))
           (values `(,@test-assgns
                     (if (eq? #t test-value)
                         (,@true-assgns
                          (assign ,if-var ,true-value))
                         (,@false-assgns
                          (assign ,if-var ,false-value))))
                   if-var
                   (append test-vars true-vars false-vars))))]
      [`(,rator ,(? integer? x) ...)
       (let ((new-sym (gensym)))
         (values `((assign ,new-sym ,exp))
                 new-sym
                 `(,new-sym)))]
      [`(,rator ,rands ...)
       (let ((results (map (lambda (exp)
                             (let-values ([(assignments value vars)
                                           (flatten exp)])
                               `(,assignments
                                 ,value
                                 ,vars)))
                           rands))
             (new-sym (gensym)))
         (values `(,@(concat-map car results)
                   (assign ,new-sym (,rator ,@(map cadr results))))
                 new-sym
                 `(,new-sym
                   ,@(concat-map caddr results))))])))

(define select-instructions
  (lambda (prog)
    (letrec ((expand
              (lambda (exp)
                (match exp
                  [(? integer?) `(int ,exp)]
                  [(? symbol?) `(var ,exp)]
                  [(? boolean?) `(bool ,exp)]
                  [`(assign ,var ,val)
                   (expand-into-var val var)]
                  [`(return ,x)
                   `((movq ,(expand x) (reg rax)))])))
             (expand-into-var
              (lambda (exp var)
                (match exp
                  [`(+ ,val1 ,val2)
                   `((movq ,(expand val1) (var ,var))
                     (addq ,(expand val2) (var ,var)))]
                  [`(read)
                   `((callq read_int)
                     (movq (reg rax) (var ,var)))]
                  [`(- ,val)
                   `((movq ,(expand val) (var ,var))
                     (neg (var ,var)))]
                  [val
                   `((movq ,(expand val) (var ,var)))]))))
      (match prog
        [`(program ,vars ,code)
         `(program ,vars
                   ,(concat-map expand code))]))))

(define get-offset
  (lambda (var vars)
    (* -16 (index-of vars var))))

;; this will need modification if the saves and restores are to be offset by
;; more than a single instruction.
(define add-register-saves
  (let ((helper
         (lambda (live-regss instrs)
           (let ((to-backup (map (lambda (live-regs instr)
                                   (match instr
                                     [`(callq ,func-name)
                                      (filter caller-save? live-regs)]
                                     [_ '()]))
                                 live-regss
                                 instrs)))
             (concat-map (lambda (backups instr)
                           `(,@(map (lambda (reg)
                                      `(pushq (reg ,reg)))
                                    backups)
                             ,instr
                             ,@(map (lambda (reg)
                                      `(popq (reg ,reg)))
                                    (reverse backups))))
                         to-backup
                         instrs)))))
    (lambda (prog)
      (match prog
        [`(program ,num ,live-varss ,instrs)
         `(program ,num
                   ,(helper live-varss instrs))]))))

;; I should add register saves and such here. callq should save all caller save
;; registers, and this should add pushs and pops at the top of all functions. In
;; this case, the only function is main.
(define add-bookkeeping
  (lambda (prog)
    (match prog
      [`(program ,stack-num ,instrs)
       `(program ,stack-num
                 ((pushq (reg rbp))
                  (pushq (reg rax))
                  (movq (reg rsp) (reg rbp))
                  (subq (int ,stack-num) (reg rsp))
                  ,@instrs
                  (movq (reg rax) (reg rdi))
                  (callq print_int)
                  (addq (int ,stack-num) (reg rsp))
                  (popq (reg rax))
                  (and (int 0) (reg rax))
                  (popq (reg rbp))
                  (retq)))])))


(define patch-instruction
  (lambda (inst)
    (match inst
      [`(,inst (deref ,var1 ,off1) (deref ,var2 ,off2))
       `((movq (deref ,var1 ,off1) (reg rax))
         (,inst (reg rax) (deref ,var2 ,off2)))]
      ;; [`(addq (deref ,var1 ,off1) (deref ,var2 ,off2))
      ;;  `((movq (deref ,var1 ,off1) (reg rax))
      ;;    (addq (reg rax) (deref ,var2 ,off2)))]
      [`(neg (deref ,var ,off))
       `((movq (deref ,var ,off) (reg rax))
         (neg (reg rax))
         (movq (reg rax) (deref ,var ,off)))]
      [`(movq (reg ,reg) (reg ,reg))
       '()]
      [x `(,x)])))

(define patch-instructions
  (lambda (prog)
    (match prog
      [`(program ,x ,insts)
       `(program ,x
                 ,(concat-map patch-instruction insts))])))

(define (print-instructions prog)
  (match prog
    [`(program ,stack-num ,insts)
     (apply string-append
            (intercalate "\n"
                         `("	.globl main"
                           "main:"
                           ,@(map print-instructions insts)
                           "\n")))]
    [`(reg ,reg) (format "%~a" reg)]
    [`(int ,num) (format "$~a" num)]
    [`(deref ,reg ,offset)
     (format "~a(%~a)" offset reg)]
    [`(,rator ,inst1 ,inst2)
     (format "	~a	~a,	~a"
             rator
             (print-instructions inst1)
             (print-instructions inst2))]
    [`(,rator ,rand)
     (format "	~a	~a"
             rator
             (print-instructions rand))]
    [`(,x) (format "	~a" x)]
    [x (format "~a" x)]))

(define get-read-vars
  (lambda (instr)
    (match instr
      [`(movq (var ,var) ,_) `(,var)]
      [`(addq (var ,var1) (var ,var2)) `(,var1 ,var2)]
      [`(addq ,_ (var ,var)) `(,var)]
      [`(neg (var ,var)) `(,var)]
      [`(,instr (var ,var) ,_) `(,var)]
      [_ '()])))

(define get-written-vars
  (lambda (instr)
    (match instr
      [`(,inst ,_ (var ,var)) `(,var)]
      [`(neg (var ,var)) `(,var)]
      [_ '()])))

(define live-after
  (letrec ((helper
            (lambda (instrs)
              (cond
               [(null? instrs) '()]
               [(null? (cdr instrs)) `(,(get-read-vars (car instrs)) ())]
               [else
                (let* ((curr (car instrs))
                       (rest (helper (cdr instrs)))
                       (next (car rest))
                       (reads (get-read-vars curr))
                       (writes (get-written-vars curr)))
                  (cons (set-union reads
                                   (set-difference next
                                                   writes))
                        rest))]))))
    (lambda (instrs)
      (cdr (helper instrs)))))

(define uncover-live
  (lambda (prog)
    (match prog
      [`(program ,x ,instrs)
       `(program ,(live-after instrs) ,x ,instrs)])))

(define add-interference-edges
  (lambda (instr live-vars graph)
    (match instr
      [`(movq (var ,s) (var ,d))
       (add-edges (filter (lambda (v)
                            (not (or (eqv? v s)
                                     (eqv? v d))))
                          live-vars)
                  d
                  (add-nodes `(,s ,d) graph))]
      [`(,_ (var ,s) (var ,d))
       (add-edges (filter (lambda (v)
                            (not (eqv? d v)))
                          live-vars)
                  d
                  (add-nodes `(,s ,d) graph))]
      [(or `(movq ,_ (var ,d))
           `(,_ (var ,d)))
       (add-edges (filter (lambda (v)
                            (not (eqv? v d)))
                          live-vars)
                  d
                  (add-nodes `(,d) graph))]
      [_ graph])))

(define construct-graph
  (lambda (instrs liveness)
    (foldl (lambda (p graph)
             (let ((instr (car p))
                   (live-vars (cdr p)))
               (add-interference-edges instr live-vars graph)))
           (make-graph)
           (map cons instrs liveness))))

(define build-interference
  (lambda (prog)
    (match prog
      [`(program ,live-after-list ,vars ,instrs)
       `(program ,(construct-graph instrs
                                   live-after-list)
                 ,live-after-list
                 ,instrs)])))

(define caller-save?
  (lambda (reg)
    (match reg
      ['rdx #t]
      ['rcx #t]
      ['rsi #t]
      ['rdi #t]
      ['r8  #t]
      ['r9  #t]
      ['r10 #t]
      ['r11 #t]
      [_   #f])))

(define caller-save-regs
  '(rdx
    rcx
    rsi
    rdi
    r8
    r9
    r10
    r11))

(define callee-save-regs
  '(rbx
    r12
    r13
    r14
    r15))

(define word-size 8)

;; need to add mappings to actual registers as well as stack locations
(define allocate-registers
  (letrec ((assign-reg
            (lambda (mapping)
              (lambda (datum)
                (match datum
                  [`(var ,var)
                   (hash-ref mapping var)]
                  [`(,instr ,args ...)
                   `(,instr ,@(map (assign-reg mapping) args))]
                  [x x])))))
    ;; (lambda (caller-save callee-save)
      (lambda (prog)
        (match prog
          [`(program ,graph ,live-vars ,instrs)
           (let* ((get-color-var car)
                  (get-color-num cdr)
                  (colors (color-graph graph))
                  (num-colors (if (null? colors)
                                  0
                                  (apply max (map get-color-num colors))))
                  (all-regs (append caller-save-regs
                                    callee-save-regs))
                  (num-regs (length all-regs))
                  (reg-mapping
                   (map (lambda (color)
                          (let ((color-num (cdr color)))
                            (cons (get-color-var color)
                                  (if (>= (get-color-num color) num-regs)
                                      `(deref rsp ,(* word-size (- color-num num-regs)))
                                      `(reg ,(list-ref all-regs color-num))))))
                        colors))
                  (reg-map (make-immutable-hasheqv reg-mapping)))
             `(program ,(let ((stack-num (- (add1 num-colors)
                                            (length caller-save-regs))))
                          (max (* stack-num word-size) 0))
                       ,(map (lambda (vars)
                               (concat-map (lambda (var)
                                             (match (hash-ref reg-map var)
                                               [`(reg ,reg)
                                                `(,reg)]
                                               [_ '()]))
                                           vars))
                             live-vars)
                       ;; ,live-vars
                       ,(map (assign-reg reg-map)
                             instrs)))]))))

(define lookup-variable
  (lambda (var env)
    (cdr (assv var env))))

(define extend-env
  (lambda (var val env)
    (cons `(,var val) env)))

(define type-error
  (lambda (func actual expected)
    (error func
           "Type error in ~a: Expected type was ~a, but got ~a"
           func
           expected
           actual)))

(define is-type-boolean?
  (lambda (type)
    (eqv? type 'Boolean)))

(define typecheck-R2-curry
  (lambda (env)
    (lambda (exp)
      (let ((recur (typecheck-R2-curry env)))
        (match exp
          [`(program ,body)
           `(program (type ,(recur body)) ,body)]
          [(? fixnum?) 'Integer]
          [(? boolean?) 'Boolean]
          [(? symbol?) (lookup-variable exp env)]
          [`(not ,exp)
           (match (recur exp)
             ['Boolean 'Boolean]
             [type (type-error 'not type 'Boolean)])]
          [`(if ,test ,true ,false)
           (let ((test-type (recur test))
                 (true-type (recur true))
                 (false-type (recur false)))
             (and (is-type-boolean? test-type)
                  (eqv? true-type false-type)
                  true-type))]
          [`(let ((,vars ,vals) ...) ,body)
           (let ((typed-pairs (map (lambda (var val)
                                     (cons var (recur val)))
                                   vars
                                   vals)))
             ((typecheck-R2-curry (foldl (lambda (pair ext-env)
                                           (extend-env (car pair)
                                                       (recur (cadr pair))
                                                       ext-env))
                                         env
                                         typed-pairs)) body))])))))

(define typecheck-R2
  (typecheck-R2-curry '()))

(define built-ins
  (map (lambda (x)
         (cons x x))
       '(+ - read < > <= >= eq?)))

(define run-all
  (compose print-instructions
           patch-instructions
           add-bookkeeping
           add-register-saves
           allocate-registers
           build-interference
           uncover-live
           select-instructions
           flatten
           (uniquify (u-state built-ins 0))))

;; racket compiler.rkt > test.s
;; gcc -g -std=c99 runtime.o test.s
;; ./a.out

;; (if #t
;;     ;; (let ((test-prog '(program (let ((a 5)) a))))
;;     (let ((test-prog '(program (if #t 1 2))))
;;     ;; (let ((test-prog '(program (let ((x (read)))
;;     ;;                              (let ((y (read)))
;;     ;;                                (+ x (- y)))))))
;;       ;; (display test-prog)
;;       ;; (newline)
;;       (display (run-all test-prog)))

;;     (compiler-tests "r1-compiler"
;;                     #f
;;                     (reverse `((print-instructions  ,print-instructions               nothing)
;;                                (patch-instructions  ,patch-instructions               nothing)
;;                                (add-bookkeeping     ,add-bookkeeping                  nothing)
;;                                (add-register-saves  ,add-register-saves               nothing)
;;                                (allocate-registers  ,allocate-registers               nothing)
;;                                (build-interference  ,build-interference               nothing)
;;                                (uncover-live        ,uncover-live                     nothing)
;;                                (select-instructions ,select-instructions              nothing)
;;                                (flatten             ,flatten                          nothing)
;;                                (uniquify            ,(uniquify (u-state built-ins 0)) nothing)))
;;                     "r1"
;;                     (range 1 49)))

(provide make-graph
         graph-equal?
         adjacent
         add-node
         add-edge
         add-edges
         construct-graph

         set-union
         set-difference
         set-singleton
         set-remove
         empty-set
         set-equal?

         live-after

         -->
         >>=
         concat-map

         print-instructions
         patch-instructions
         add-bookkeeping
         add-register-saves
         allocate-registers
         build-interference
         uncover-live
         select-instructions
         flatten
         uniquify
         u-state
         built-ins
         )
