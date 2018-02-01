#lang racket

;; TODO fix register allocation maybe? Theres just a bug somewhere......

;; NOTE Look into using basic blocks in the compiler

;; NOTE Look into the differences between generational garbage collecting and
;; copy collecting. Check out some of the references on page 81

;; NOTE Look into using type information in garbage collecting. See page 82


;; Need to get uncover-live to spit out data which indicates whether it was a
;; simple instruction or an if statement. For the if statements, recursive
;; processing is also necessary. The next step, build-interference, needs to
;; interpret these and act recursively on the if statements as well.




(require "utilities.rkt")
(require racket/trace)

;; My helpers

(define-for-syntax DEBUGGING #f)

(define-syntax debug-define
  (lambda (exp)
    (syntax-case exp ()
      [(_ name body)
       (if DEBUGGING
           #'(trace-define name body)
           #'(define name body))])))

;; basically just bind for something similar to a maybe monad
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

;; a sort of let* shorthand, inspired by do notation
(define-syntax -->
  (syntax-rules (<- rec)
    [(_ x <- exp rest ...)
     (let ((x exp))
       (--> rest ...))]
    [(_ recur x <- exp rest ...)
     (letrec ((x exp))
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

(debug-define uniquify
  (lambda (prog)
    (match prog
      [`(program ,type ,defines ,instrs)
       (--> init-env <- (u-state (extend-env-vars
                               (map (lambda (x) (cons (caadr x) (gensym 'userfunc))) defines)
                               built-ins)
                              0)
            uniq-defs <- (map (uniquify-def init-env) defines)
            uniq-bodies <- (map (uniquify-exp init-env) instrs)
            `(program ,type
                      ,uniq-defs
                      ,uniq-bodies))])))

(define type-and-exp
  (lambda (exp)
    (match exp
      [`(has-type ,exp ,type)
       (values exp type)])))

(define get-type
  (lambda (exp)
    (match exp
      [`(has-type ,exp ,type)
       type])))

(define uniquify-def
  (lambda (state)
    (lambda (def)
      (let ((assoc-list (u-state-assoc-list state))
            (same-state-uniquify (uniquify-exp state)))
        (match def
          [`(define (,name ,vars ...) : ,type ,body)
           (--> renamed-vars <- (map (lambda (formal) (cons formal (gensym))) (map car vars))
                new-env <- (extend-env-vars renamed-vars assoc-list)
                new-vars <- (map (lambda (var)
                                   (match var [`(,var-name : ,type)
                                               `(,(variable-lookup var-name new-env) : ,type)]))
                                 vars)
                `(define (,(variable-lookup name new-env) ,@new-vars) : ,type
                   ,((uniquify-exp
                      (struct-copy u-state
                                   state
                                   (assoc-list new-env)))
                     body)))])))))

(define uniquify-exp
  (lambda (state)
    (lambda (typed-exp)
      (let-values ([(exp type)
                    (type-and-exp typed-exp)])
        (let ((assoc-list (u-state-assoc-list state))
              (same-state-uniquify (uniquify-exp state)))
          `(has-type ,(match exp
                        [(? integer?) exp]
                        [(? boolean?) exp]
                        [`(has-type ,var built-in)
                         `(has-type ,var built-in)]
                        [(? symbol?)
                         (or (variable-lookup exp assoc-list)
                             (error 'uniquify-exp "Unbound variable: ~a" exp))]
                        [`(lambda ([,param-names : ,param-types] ...) : ,type ,body)
                         (let* ((new-param-names (map (lambda (_) (gensym))
                                                      param-names))
                                (new-env (extend-env-vars (map cons param-names new-param-names)
                                                          assoc-list)))
                           `(lambda ,(map (lambda (name type) `(,name : ,type))
                                     new-param-names
                                     param-types) : ,type
                              ,((uniquify-exp
                                 (struct-copy u-state
                                              state
                                              (assoc-list new-env)))
                                body)))]
                        [`(if ,test ,true ,false)
                         `(if ,(same-state-uniquify test)
                              ,(same-state-uniquify true)
                              ,(same-state-uniquify false))]
                        [`(let ([,var ,val])
                            ,body)
                         (let ([new-sym (gensym)])
                           `(let ([,new-sym ,(same-state-uniquify val)])
                              ,((uniquify-exp
                                 (struct-copy u-state
                                              state
                                              (assoc-list (cons (cons var
                                                                      new-sym)
                                                                assoc-list))))
                                body)))]
                        [`(,operator ,operands ...)
                         (let ((f (uniquify-exp state)))
                           `(,(f operator)
                             ,@(map f operands)))])
                     ,type))))))

(debug-define reveal-functions
  (lambda (prog)
    (match prog
      [`(program ,type ,defs ,bodies)
       (--> func-names <- (map caadr defs)
            revealed-defs <- (map (reveal-def func-names) defs)
            revealed-bodies <- (map (reveal-exp func-names) bodies)
            `(program ,type ,revealed-defs ,revealed-bodies))])))

(define reveal-def
  (lambda (func-names)
    (lambda (def)
      (match def
        [`(define (,name ,vars ...) : ,type ,body)
         `(define (,name ,@vars) : ,type ,((reveal-exp func-names) body))]))))

(define reveal-exp
  (lambda (func-names)
    (lambda (typed-exp)
      (let-values ([(exp type)
                    (type-and-exp typed-exp)])
        (let ([recur (reveal-exp func-names)])
          `(has-type ,(match exp
                        [(? symbol?)
                         (if (member exp func-names)
                             `(function-ref ,exp)
                             exp)]
                        [`(if ,test ,true ,false)
                         `(if ,(recur test)
                              ,(recur true)
                              ,(recur false))]
                        [`(lambda ,params : ,type ,body)
                         `(lambda ,params : ,type ,(recur body))]
                        [`(let ([,var ,val]) ,body)
                         `(let ([,var ,(recur val)]) ,(recur body))]
                        [`(,operator ,operands ...)
                         (match (get-type operator)
                           [`(Function ,arg-types ,ret-type)
                            `(app ,(recur operator) ,@(map recur operands))]
                           [_ `(,operator ,@(map recur operands))])]
                        [x x]) ,type))))))

(define free-variables
  (lambda (typed-exp)
                (let-values ([(exp type)
                              (type-and-exp typed-exp)])
                  (match exp
                    [(? symbol?) `((,exp . ,type))]
                    [(? integer?) '()]
                    [(? boolean?) '()]
                    [`(function-ref ,name) '()]
                    [`(if ,test ,true ,false)
                     (append (free-variables test)
                             (free-variables true)
                             (free-variables false))]
                    [`(lambda ,params : ,type ,body)
                     (filter-not (lambda (x) (member (car x) (map car params)))
                                 (free-variables body))]
                    [`(app ,rator ,rands ...)
                     (concat-map free-variables `(,rator ,@rands))]
                    [`(,rator ,rands ...)
                     (concat-map free-variables rands)]
                    [`(let ((,name ,val)) ,body)
                     (filter-not (lambda (sym) (eqv? name (car sym)))
                                 (free-variables body))])
                  )))

(define make-def-from-lambda
  (lambda (lam free-vars vec-type)
    (match lam
      [`(lambda ,params : ,type ,body)
       (let ((name (gensym 'lambda))
             (closure-name (gensym 'closure)))
         (letrec ((assign-free-vars
                   (lambda (free-vars i)
                     (if (null? free-vars)
                         body
                         `(has-type (let ([,(caar free-vars) (has-type
                                                              ((has-type vector-ref built-in)
                                                               (has-type ,closure-name ,vec-type)
                                                               (has-type ,i Integer))
                                                              ,(cdar free-vars))])
                            ,(assign-free-vars (cdr free-vars) (add1 i))) ,type)))))
           `(define (,name (,closure-name : ,vec-type) ,@params) : ,type
              ,(assign-free-vars free-vars 1))))])))


;; two values returned from each function. One is every new define created by
;; the form, and the other is the resultant form. Lambdas will be replaced with
;; vectors, and defines will just be appended onto the list.
(debug-define convert-to-closures
  (letrec ([convert-define-to-closures
            (lambda (def)
              (match def
                [`(define (,name ,params ...) : ,type ,body)
                 (let-values ([(body-defines new-body)
                              (convert-exp-to-closures body)])
                   (let* ((new-formal (gensym 'closure-placeholder)))
                   (values body-defines
                           `(define (,name (,new-formal : (Vector (Function ,(map caddr params)
                                                                            ,type))) ,@params) : ,type
                              ,new-body))))]))]
           [convert-exp-to-closures
            (lambda (typed-exp)
              (let-values ([(exp type)
                            (type-and-exp typed-exp)])
                (let-values
                    ([(defines new-exp)
                      (match exp
                        [(? symbol?) (values '() exp)]
                        [(? integer?) (values '() exp)]
                        [(? boolean?) (values '() exp)]
                        [`(function-ref ,_)
                         (let ((old-type type))
                           (set! type `(Vector ,type))
                           (values '() `((has-type vector built-in)
                                         (has-type ,exp ,old-type))))]
                        [`(lambda ,params : ,ret-type ,body)
                         (let-values ([(body-defines new-body)
                                       (convert-exp-to-closures body)])
                           (let* ((free-vars (set->list (list->set (free-variables typed-exp))))
                                  (vec-type `(Vector ,type ,@(map cdr free-vars)))
                                  (new-def (make-def-from-lambda exp free-vars vec-type))
                                  (old-type type))
                             (match new-def
                               [`(define (,name ,params ...) : ,new-def-type ,new-def-body)

                                (let-values ([(new-def-body-defs new-new-def-body)
                                              (convert-exp-to-closures new-def-body)])
                                  (let ((new-new-def
                                         `(define (,name ,@params) : ,new-def-type ,new-new-def-body)))
                                    (set! type vec-type)
                                    (values `(,new-new-def ,@new-def-body-defs)
                                            `((has-type vector built-in) (has-type (function-ref ,(caadr new-new-def)) ,old-type)
                                              ,@(map (lambda (x) `(has-type ,(car x) ,(cdr x))) free-vars))

                                            )))])))]
                        [`(let ((,var ,val)) ,body)
                         (let-values ([(val-defines new-val)
                                       (convert-exp-to-closures val)]
                                      [(body-defines new-body)
                                       (convert-exp-to-closures body)])
                           (values (append val-defines body-defines)
                                   `(let ((,var ,new-val)) ,new-body)))]
                        [`(if ,test ,true ,false)
                         (let-values ([(test-defines new-test)
                                       (convert-exp-to-closures test)]
                                      [(true-defines new-true)
                                       (convert-exp-to-closures true)]
                                      [(false-defines new-false)
                                       (convert-exp-to-closures false)])
                           (values (append test-defines
                                           true-defines
                                           false-defines)
                                   `(if ,new-test
                                        ,new-true
                                        ,new-false)))]
                        [`(app ,rator ,rands ...)
                         (let-values ([(rator-defines new-rator)
                                       (convert-exp-to-closures rator)]
                                      [(rands-defines new-rands)
                                       (convert-list-to-closures rands convert-exp-to-closures)])
                           (values (append rator-defines rands-defines)
                                   (let ((temp (gensym 'app-temp)))
                                     `(let ([,temp ,new-rator])
                                        (has-type (app (has-type ((has-type vector-ref built-in)
                                                                  (has-type ,temp ,(get-type new-rator))
                                                                  (has-type 0 Integer))
                                                                 ,(get-type rator))
                                                       (has-type ,temp ,(get-type new-rator))
                                                       ,@new-rands)
                                                  ,type)))))]

                        [`(,rator ,rands ...)
                         (let-values ([(rands-defines new-rands)
                                       (convert-list-to-closures rands convert-exp-to-closures)])
                           (values rands-defines
                                   `(,rator ,@new-rands)))]
                        )]
                     )
                  (values defines `(has-type ,new-exp ,type)))))]
           [convert-list-to-closures
            (lambda (exps conversion-func)
              (let ((exps-values (map (lambda (exp)
                                         (let-values ([(exp-defines new-exp)
                                                       (conversion-func exp)])
                                           (cons exp-defines new-exp)))
                                       exps)))
                (values (concat-map car exps-values)
                        (map cdr exps-values))))])
    (lambda (prog)
      (match prog
        [`(program ,type ,defs ,bodies)
         (let-values ([(defs-defines new-defs)
                       (convert-list-to-closures defs convert-define-to-closures)]
                      [(bodies-defines new-bodies)
                       (convert-list-to-closures bodies convert-exp-to-closures)])
                         `(program ,type
                                   ,(append new-defs defs-defines bodies-defines)
                                   ,new-bodies))]))))


(debug-define expose-allocation
  (letrec ((let-nester
            (lambda (ided-typed-exps to-nest vec-addr curr-idx vec-type vec-len)
              (if (null? ided-typed-exps)
                  (let ((num-bytes (+ 8 (* vec-len 8))))
                    `(has-type (let ([,(gensym 'should-collect)
                                      (has-type (if (has-type ((has-type < built-in)
                                                               (has-type ((has-type + built-in)
                                                                          (has-type (global-value free_ptr) Integer)
                                                                          (has-type ,num-bytes Integer))
                                                                         Integer)
                                                               (has-type (global-value fromspace_end) Integer))
                                                              Boolean)
                                                    (has-type ((has-type void built-in)) Void)
                                                    (has-type ((has-type collect built-in)
                                                               (has-type ,num-bytes Integer)) Void))
                                                Void)])
                                 (has-type (let ([,vec-addr (has-type (allocate ,vec-len ,vec-type) ,vec-type)])
                                             ,to-nest)
                                           ,vec-type))
                               ,vec-type))
                  (--> curr <- (car ided-typed-exps)
                       id <- (car curr)
                       typed-exp <- (cdr curr)
                       `(has-type (let ([,id ,typed-exp])
                                    ,(let-nester (cdr ided-typed-exps)
                                                 `(has-type (let ([,(gensym 'vec-index)
                                                                   (has-type
                                                                    ((has-type vector-set! built-in)
                                                                     (has-type ,vec-addr
                                                                               ,vec-type)
                                                                     (has-type ,curr-idx
                                                                               Integer)
                                                                     (has-type ,id
                                                                               ,(caddr typed-exp)))
                                                                    Void)])
                                                              ,to-nest)
                                                            ,vec-type)
                                                 vec-addr
                                                 (add1 curr-idx)
                                                 vec-type
                                                 vec-len))
                                  ,vec-type)))))
           (expose-helper
            (lambda (typed-exp)
              (match typed-exp
                [(or `(has-type ,_ built-in)
                     `(has-type ,(? integer?) Integer)
                     `(has-type ,(? symbol?) ,_)
                     `(has-type ,(? boolean?) Boolean)
                     `(has-type (function-ref ,_) ,_))
                 typed-exp]
                [`(has-type ((has-type vector ,_) ,typed-exps ...) ,type)
                 (--> ided-typed-exps <- (map (lambda (typed-exp)
                                                (cons (gensym)
                                                      (expose-helper typed-exp)))
                                              typed-exps)
                      vector-addr <- (gensym)
                      vector-len <- (length typed-exps)
                      (let-nester ided-typed-exps
                                  `(has-type ,vector-addr ,type)
                                  vector-addr
                                  0
                                  type
                                  vector-len))]
                [`(has-type (let ((,sym ,exp)) ,body) ,type)
                 `(has-type (let ((,sym ,(expose-helper exp)))
                              ,(expose-helper body))
                            ,type)]
                [`(has-type (if ,test ,true ,false) ,type)
                 `(has-type (if ,(expose-helper test)
                                ,(expose-helper true)
                                ,(expose-helper false))
                            ,type)]
                [`(has-type (app ,rator ,rands ...) ,type)
                 `(has-type (app ,(expose-helper rator)
                                 ,@(map expose-helper rands)) ,type)]
                [`(has-type (,exps ...) ,type)
                 `(has-type ,(map expose-helper exps) ,type)]))))
    (lambda (prog)
      (match prog
        [`(program ,output-type
                   ,defines
                   ,exps)
         `(program ,output-type
                   ,(map expose-allocation defines)
                   ,(map expose-helper exps))]
        [`(define (,name ,params ...) : ,return-type ,body)
         `(define (,name ,@params) : ,return-type ,(expose-helper body))]))))


(debug-define flatten
  (letrec ((recur
            (lambda (typed-exp)
              (let-values ([(exp type)
                            (type-and-exp typed-exp)])
                (match exp
                  [(? integer?)
                   (values '() exp '())]
                  [(? boolean?)
                   (values '() exp '())]
                  [(? symbol?)
                   (values '() exp `((,exp . ,type)))]
                  [`(allocate ,bytes ,type)
                   (values '() exp '())]
                  [`(function-ref ,func-name)
                   (let ((ref-sym (gensym 'func-ref)))
                     (values `((assign ,ref-sym ,exp))
                             ref-sym
                             `((,ref-sym . ,type))))]
                  [`(let ((,x ,assgn)) ,body)
                   (let-values ([(assgn-assignments assgn-value assgn-vars)
                                 (recur assgn)]
                                [(body-assignments body-value body-vars)
                                 (recur body)])
                     (values `(,@assgn-assignments
                               (assign ,x ,assgn-value)
                               ,@body-assignments)
                             body-value
                             `(,@assgn-vars
                               ,@body-vars
                               (,x . ,(match assgn
                                        [`(has-type ,_ ,assgn-type)
                                         assgn-type])))))]
                  [`(if ,test ,true ,false)
                   (let-values ([(test-assgns test-value test-vars)
                                 (recur test)]
                                [(true-assgns true-value true-vars)
                                 (recur true)]
                                [(false-assgns false-value false-vars)
                                 (recur false)])
                     (let ((if-var (gensym 'if-var)))
                       (values `(,@test-assgns
                                 (if (eq? #t ,test-value)
                                     (,@true-assgns
                                      (assign ,if-var ,true-value))
                                     (,@false-assgns
                                      (assign ,if-var ,false-value))))
                               if-var
                               (append `((,if-var . ,type))
                                       test-vars
                                       true-vars
                                       false-vars))))]
                  [`(global-value ,name)
                   (let ((global-val-var (gensym 'global-val-var)))
                     (values `((assign ,global-val-var ,exp))
                             global-val-var
                             `((,global-val-var . Integer))))]
                  [`((has-type and built-in) ,rands ...)
                   (match rands
                     [`()
                      (values '() #t '())]
                     [`(,rand ,rest ...)
                      (recur `(has-type (if ,rand
                                            (has-type ((has-type and built-in)
                                                       ,@rest)
                                                      Boolean)
                                            (has-type #f Boolean))
                                        Boolean))])]
                  [`((has-type collect built-in) (has-type ,bytes Integer))
                   (values `((collect (int ,bytes)))
                           `(void)
                           '())]
                  [`((has-type ,rator built-in) ,rands ...)
                   (let ((results (map (lambda (exp)
                                         (let-values ([(assignments value vars)
                                                       (recur exp)])
                                           `(,assignments
                                             ,value
                                             ,vars)))
                                       rands))
                         (exp-result (gensym 'app-return)))
                     (values `(,@(concat-map car results)
                               (assign ,exp-result (,rator ,@(map cadr results))))
                             exp-result
                             `((,exp-result . ,type)
                               ,@(concat-map caddr results))))]
                  ;; [`(,rator ,(? integer? x) ...)
                  ;;  (let ((new-sym (gensym 'app-return-int)))
                  ;;    (values `((assign ,new-sym ,exp))
                  ;;            new-sym
                  ;;            `((,new-sym . ,type))))]
                  [`(app ,rator ,rands ...)
                   (let-values ([(results)
                                 (map (lambda (exp)
                                        (let-values ([(assignments value vars)
                                                      (recur exp)])
                                          `(,assignments
                                            ,value
                                            ,vars)))
                                      rands)]
                                [(rator-assgns rator-value rator-vars)
                                 (recur rator)]
                                [(result-var) (gensym 'result-var)]
                                [(rator-result) (gensym 'rator-result)])
                     (values `(,@(concat-map car results)
                               ,@rator-assgns
                               (assign ,rator-result ,rator-value)
                               (assign ,result-var (app ,rator-result
                                                        ,@(map cadr results))))
                             result-var
                             `((,rator-result . ,(get-type rator))
                               (,result-var . ,type)
                               ,@(concat-map caddr results))))])))))
    (lambda (exp)
      (match exp
        [`(program ,type ,defines ,exps)
         (let ((flat (map (lambda (exp)
                            (let-values ([(assignments value vars)
                                          (recur exp)])
                              `(,assignments ,value ,vars)))
                          exps))
               (defines-flats
                 (map (lambda (def)
                        (match def
                          [`(define (,name ,params ...) : ,ret-type ,body)
                           (let-values ([(assignments value vars)
                                         (recur body)])
                             `(define (,name ,@params) : ,ret-type
                                ,(filter-not (lambda (x)
                                               (member (car x)
                                                       (map car params)))
                                             vars)
                                (,@assignments
                                 (return ,value))))]))
                      defines)))
           `(program ,type
                     ,(foldl set-insert
                             '()
                             (concat-map caddr flat))
                     ,defines-flats
                     ,(concat-map (lambda (assgns val)
                                    `(,@assgns
                                      (return ,val)))
                                  (map car flat)
                                  (map cadr flat))))]))))

(define get-param-locations
  (lambda (num caller-view?)
    (let* ((regs (map (lambda (x)
                        `(reg ,x))
                      '(rdi rsi rdx rcx r8 r9)))
           (stacks (map (lambda (x)
                          (if caller-view?
                              `(deref rsp ,(* 8 x))
                              `(deref rbp ,(* 8 (+ 2 x)))))
                        (range (- num (length regs))))))
      (take (append regs stacks) num))))

(debug-define select-instructions
  (lambda (prog)
    (letrec ((expand-comparison
              (lambda (comp arg1 arg2 var)
                `((cmpq ,(expand arg2)
                        ,(expand arg1))
                  ,(match comp
                     [`(eq? ,_ ,_) '(sete (byte-reg al))]
                     [`(< ,_ ,_) '(setl (byte-reg al))]
                     [`(> ,_ ,_) '(setg (byte-reg al))]
                     [`(<= ,_ ,_) '(setle (byte-reg al))]
                     [`(>= ,_ ,_) '(setge (byte-reg al))])
                  (movzbq (byte-reg al) (var ,var)))))
             (expand
              (lambda (exp)
                (match exp
                  [(? integer?) `(int ,exp)]
                  [(? symbol?) `(var ,exp)]
                  [(? boolean?) `(bool ,exp)]
                  [`(assign ,var ,val)
                   (expand-into-var val var)]
                  [`(eq? ,exp1 ,exp2)
                   `(eq? ,(expand exp1) ,(expand exp2))]
                  [`(collect ,num-bytes)
                   `(
                     (spill-vectors-to-root-stack)
                     (movq (reg r15) (reg rdi))
                     (movq ,num-bytes (reg rsi))
                     (callq collect)
                     (restore-vectors-from-root-stack)
                     )]
                  [`(if ,test ,true ,false)
                   `((if ,(expand test)
                         ,(concat-map expand true)
                         ,(concat-map expand false)))]
                  [`(return ,x)
                   `((movq ,(expand x) (reg rax)))])))
             (expand-into-var
              (lambda (exp var)
                (match exp
                  [`(function-ref ,func)
                   `((leaq (function-ref ,func) (var ,var)))]
                  [`(+ ,val1 ,val2)
                   `((movq ,(expand val1) (var ,var))
                     (addq ,(expand val2) (var ,var)))]
                  [`(app ,rator ,rands ...)
                   (let ((locs (get-param-locations (length rands) #t)))
                     `(,@(map (lambda (loc val)
                                `(movq ,(expand val)
                                       ,loc))
                              locs
                              rands)
                       (indirect-callq (var ,rator))
                       (movq (reg rax) (var ,var))))]
                  [`(read)
                   `((callq read_int)
                     (movq (reg rax) (var ,var)))]
                  [`(- ,val)
                   `((movq ,(expand val) (var ,var))
                     (neg (var ,var)))]
                  [`(not ,val)
                   `((movq (int 1) (var ,var))
                     (xorq ,(expand val) (var ,var)))]
                  [`(vector-set! ,vector ,idx ,value)
                   `((movq (var ,vector) (reg r11))
                     (movq ,(expand value) (deref r11 ,(* 8 (add1 idx))))
                     (movq (int 0) (var ,var)))]
                  [`(vector-ref ,vec ,idx)
                   `((movq (var ,vec) (reg r11))
                     (movq (deref r11 ,(* 8 (add1 idx))) ,(expand var)))]
                  [`(global-value ,global-val)
                   `((movq (global-value ,global-val) ,(expand var)))]
                  [`(allocate ,len (Vector ,vec-types ...))
                   (let ((tag (bitwise-ior (arithmetic-shift
                                            (foldl (lambda (if-vec accum)
                                                     (+ if-vec (* 2 accum)))
                                                   0
                                                   (map (lambda (type)
                                                          (match type
                                                            [`(Vector ,_ ...) 1]
                                                            [_ 0]))
                                                        vec-types))
                                            7)
                                           1
                                           (arithmetic-shift (length vec-types)
                                                             1)))
                         (bytes-allocated (* 8 (add1 (length vec-types)))))
                     `((movq (global-value free_ptr) ,(expand var))
                       (addq (int ,bytes-allocated) (global-value free_ptr))
                       (movq ,(expand var) (reg r11))
                       (movq (int ,tag) (deref r11 0))))]
                  [`(void)
                   `((movq (int 0) ,(expand var)))]
                  [(or `(eq? ,arg1 ,arg2)
                       `(> ,arg1 ,arg2)
                       `(< ,arg1 ,arg2)
                       `(>= ,arg1 ,arg2)
                       `(<= ,arg1 ,arg2))
                   (expand-comparison exp arg1 arg2 var)]
                  [val
                   `((movq ,(expand val) (var ,var)))]))))
      (match prog
        [`(program ,type ,vars ,defines ,code)
         `(program ,type
                   ,vars
                   ,(map select-instructions defines)
                   ,(concat-map expand code))]
        [`(define (,name ,params ...) : ,type ,vars ,body)
         `(define (,name ,@params) : ,type ,(append (map cons
                                                         (map car params)
                                                         (map caddr params))
                                                    vars)
            (,@(map (lambda (loc param-name)
                      `(movq ,loc (var ,param-name)))
                    (get-param-locations (length params) #f)
                    (map car params))
             ,@(concat-map expand body)))]))))

(define get-offset
  (lambda (var vars)
    (* -16 (index-of vars var))))



;; I should add register saves and such here. callq should save all caller save
;; registers, and this should add pushs and pops at the top of all functions. In
;; this case, the only function is main.
(debug-define add-bookkeeping
  (lambda (prog)
    (match prog
      [`(program (type ,type) ,stack-num ,defines ,instrs)
       (let ((max-def-stack-n (* 8 (apply max 0 (map get-stack-num defines)))))
         `(program ,stack-num
                   ,(map (add-bookkeeping-def max-def-stack-n) defines)
                   ((label-pos prelude)
                    (pushq (reg rbp))
                    (pushq (reg rax))
                    (movq (reg rsp) (reg rbp))
                    (subq (int ,(+ stack-num max-def-stack-n)) (reg rsp))
                    (movq (int 2048) (reg rdi))
                    (movq (int 65536) (reg rsi))
                    (callq initialize)
                    (movq (global-value rootstack_begin) (reg r15))
                    (movq (int 0) (deref r15 0))
                    (label-pos endprelude)
                    ,@instrs
                    (label-pos conclusion)
                    (movq (reg rax) (reg rdi))
                    (literal ,(print-by-type type))
                    (addq (int ,(+ stack-num max-def-stack-n)) (reg rsp))
                    (popq (reg rax))
                    (and (int 0) (reg rax))
                    (popq (reg rbp))
                    (retq))))]
      )))

(define add-bookkeeping-def
  (lambda (max-stack-n)
    (lambda (def)
      (match def
        [`(define (,name ,params ...) : ,type ,stack-num ,instrs)
         `(define (,name ,@params) ,stack-num
            ((pushq (reg rbp))
             (movq (reg rsp) (reg rbp))
             ,@(map (lambda (reg)
                      `(pushq (reg ,reg)))
                    callee-save-regs)
             (subq (int ,(+ stack-num max-stack-n)) (reg rsp))
             ,@instrs
             (addq (int ,(+ stack-num max-stack-n)) (reg rsp))
             ,@(map (lambda (reg)
                      `(popq (reg ,reg)))
                    (reverse callee-save-regs))
             (popq (reg rbp))
             (retq)))]))))


(define patch-instruction
  (lambda (inst)
    (match inst
      [`(,inst (deref ,var1 ,off1) (deref ,var2 ,off2))
       `((movq (deref ,var1 ,off1) (reg rax))
         (,inst (reg rax) (deref ,var2 ,off2)))]
      [`(,inst (deref ,var1 ,off1) (global-value ,var2))
       `((movq (deref ,var1 ,off1) (reg rax))
         (,inst (reg rax) (global-value ,var2)))]
      [`(,inst (global-value ,var1) (deref ,var2 ,off2))
       `((movq (global-value ,var1) (reg rax))
         (,inst (reg rax) (deref ,var2 ,off2)))]
      [`(,inst (global-value ,var1) (global-value ,var2))
       `((movq (global-value ,var1) (reg rax))
         (,inst (reg rax) (global-value ,var2)))]
      [`(movzbq ,reg (deref ,var ,off))
       `((movzbq ,reg (reg rax))
         (movq (reg rax) (deref ,var ,off)))]
      [`(neg (deref ,var ,off))
       `((movq (deref ,var ,off) (reg rax))
         (neg (reg rax))
         (movq (reg rax) (deref ,var ,off)))]
      [`(cmpq ,arg (int ,num))
       `((movq (int ,num) (reg rax))
         (cmpq ,arg (reg rax)))]
      [`(cmpq ,arg (bool ,b))
       `((movq (bool ,b) (reg rax))
         (cmpq ,arg (reg rax)))]
      [`(movq (reg ,reg) (reg ,reg))
       '()]
      [x `(,x)])))

(debug-define patch-instructions
  (lambda (prog)
    (match prog
      [`(program ,stack-num ,defines ,insts)
       `(program ,stack-num
                 ,(map patch-instructions defines)
                 ,(concat-map patch-instruction insts))]
      [`(define (,name ,params ...) ,stack-num ,insts)
       `(define (,name ,@params) ,stack-num
          ,(concat-map patch-instruction insts))])))

(define print-instructions
  (lambda (prog)
    (match prog
      [`(program ,stack-num ,defines ,insts)
       (apply string-append
              (intercalate "\n"
                           `(,@(map print-instructions defines)
                             "	.globl main"
                             "main:"
                             ,@(map print-instructions insts)
                             "\n")))]
      [`(define (,name ,params ...) ,stack-num ,insts)
       (apply string-append
              (intercalate "\n"
                           `(,(format "	.globl ~a" name)
                             ,(format "~a:" name)
                             ,@(map print-instructions insts)
                             "\n\n")))]
      [`(literal ,str) str]
      [`(reg ,reg) (format "%~a" reg)]
      [`(byte-reg ,reg) (format "%~a" reg)]
      [`(label ,label) (format "~a" label)]
      [`(label-pos ,label) (format "~a:" label)]
      [`(int ,num) (format "$~a" num)]
      [`(bool #t) (format "$1")]
      [`(bool #f) (format "$0")]
      [`(deref ,reg ,offset)
       (format "~a(%~a)" offset reg)]
      [`(global-value ,global-value)
       (format "~a(%rip)" global-value)]
      [`(jmp-if equal ,location)
       (format "	je	~a" (print-instructions location))]
      [`(sete ,reg)
       (format "	~a	~a" 'sete (print-instructions reg))]
      [`(function-ref ,name)
       (format "~a(%rip)" name)]
      [`(indirect-callq ,arg)
       (format "	callq	*~a" (print-instructions arg))]
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
      [x (format "~a" x)])))

(define get-read-vars
  (letrec ((std-two-arg-reads
            (lambda (instr)
              (match instr
                [`(,_ (var ,var1) (var ,var2))
                 `(,var1 ,var2)]
                [`(,_ (var ,var) ,_)
                 `(,var)]
                [`(,_ ,_ (var ,var))
                 `(,var)]
                [`(,_ ,_ ,_) '()])))
           (std-one-arg-reads
            (lambda (instr)
              (match instr
                [`(,_ (var ,var))
                 `(,var)]))))
    (lambda (instr)
      (match instr
        [`(movq (var ,var) ,_) `(,var)]
        [`(movq ,_ ,_) '()]
        [`(callq ,_) '()]
        [(or `(addq ,_ ,_)
             `(eq? ,_ ,_)
             `(xorq ,_ ,_)
             `(cmpq ,_ ,_)
             `(movzbq ,_ ,_))
         (std-two-arg-reads instr)]
        [`(neg ,_) (std-one-arg-reads instr)]
        [`(not ,_) (std-one-arg-reads instr)]
        [`(indirect-callq ,_) (std-one-arg-reads instr)]
        [(or `(restore-vectors-from-root-stack)
             `(spill-vectors-to-root-stack)
             `(leaq ,_ ,_)
             `(sete ,_)
             `(setle ,_)
             `(setge ,_)
             `(setg ,_)
             `(setl ,_)) '()]
        [_ (error 'get-read-vars "Unrecognized instruction: ~a" instr)]))))

(define get-written-vars
  (letrec ((std-two-arg-writes
            (lambda (instr)
              (match instr
                [`(,_ ,_ (var ,var)) `(,var)]
                [`(,_ ,_ ,_) '()]))))
    (lambda (instr)
      (match instr
        [`(movq ,_ (var ,var))
         `(,var)]
        [`(movq ,_ ,_)
         `()]
        [(or `(addq ,_ ,_)
             `(eq? ,_ ,_)
             `(cmpq ,_ ,_)
             `(xorq ,_ ,_)
             `(leaq ,_ ,_)
             `(movzbq ,_ ,_))
         (std-two-arg-writes instr)]
        [`(callq ,_) '()]
        [`(neg (var ,var))
         `(,var)]
        [`(not (var ,var))
         `(,var)]
        [(or `(restore-vectors-from-root-stack)
             `(spill-vectors-to-root-stack)
             `(indirect-callq ,_)
             `(sete ,_)
             `(setl ,_)
             `(setg ,_)
             `(setge ,_)
             `(setle ,_))
         '()]
        [_ (error 'get-written-vars "Unrecognized instruction: ~a" instr)]))))

;; need to refactor this to actually modify the instructions instead of
;; returning a parallel list
(define live-after-sets
  (letrec ((get-live-vars
            (lambda (live-set)
              (match live-set
                [_
                 live-set])))
           (set-calculation
            (lambda (reads writes live-vars)
              (set-union reads
                         (set-difference live-vars
                                         writes))))
           (live-before
            (lambda (curr-instr live-after)
              (--> reads  <- (get-read-vars curr-instr)
                   writes <- (get-written-vars curr-instr)
                   (set-calculation reads writes live-after))))
           (live-after-prime
            (lambda (instrs live-after)
              (let ((curr (car instrs)))
                (match curr
                  [`(if ,test ,true ,false)
                   (let*-values ([(live-after-instrs processed-instrs)
                                  (live-after-prime (cdr instrs) live-after)]
                                 [(live-before-test test-instrs)
                                  (live-after-prime (list test) live-after-instrs)]
                                 [(live-before-true true-instrs)
                                  (live-after-prime true live-after-instrs)]
                                 [(live-before-false false-instrs)
                                  (live-after-prime false live-after-instrs)]
                                 )
                     (let ((live-before-if (set-union live-before-true
                                                      live-before-false
                                                      live-before-test)))
                       (values live-before-if
                               (cons `((if ,test-instrs
                                           ,true-instrs
                                           ,false-instrs)
                                       ,live-before-if)
                                     processed-instrs))))]
                  [else
                   (cond
                    ;; nothing is live after the last instruction
                    [(null? (cdr instrs))
                     (values (set-calculation (get-read-vars curr)
                                              (get-written-vars curr)
                                              live-after)
                             `((,curr ,live-after)))]
                    [else
                     (let-values ([(live-after-instrs processed-instrs)
                                   (live-after-prime (cdr instrs) live-after)])
                       (values (live-before curr live-after-instrs)
                               (cons `(,curr ,live-after-instrs)
                                     processed-instrs)))])])))))
    (lambda (instrs)
      ;; L_before is only used for the previous instruction's L_after, making
      ;; the first set useless for our analysis
      (let-values ([(_ new-instrs)
                    (live-after-prime instrs '())])
        new-instrs))))

(debug-define uncover-live
  (lambda (prog)
    (match prog
      [`(program ,type ,x ,defines ,instrs)
       `(program ,type ,x
                 ,(map uncover-live defines)
                 ,(live-after-sets instrs))]
      [`(define (,name ,params ...) : ,type ,vars ,body)
       `(define (,name ,@params) : ,type ,vars ,(live-after-sets body))])))

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
           `(neg (var ,d))
           `(movzbq ,_ (var ,d))
           `(leaq ,_ (var ,d)))
       (add-edges (filter (lambda (v)
                            (not (eqv? v d)))
                          live-vars)
                  d
                  (add-nodes `(,d) graph))]
      [(or `(spill-vectors-to-root-stack)
           `(restore-vectors-from-root-stack)
           `(indirect-callq ,_)
           `(callq ,_)
           `(movq ,_ ,_)
           `(cmpq ,_ ,_)
           `(xorq ,_ ,_)
           `(sete ,_)
           `(setl ,_)
           `(setg ,_)
           `(setle ,_)
           `(setge ,_)
           `(addq ,_ ,_)
           `(eq? ,_ ,_))
       graph])))


(define construct-graph
  (letrec ((handle-pair
            (lambda (p graph)
              (let ((instr (car p))
                    (live-vars (cadr p)))
                (match instr
                  [`(if ,test ,true ,false)
                   (--> test-graph <- (handle-instrs test
                                                     graph)
                        true-graph <- (handle-instrs true
                                                     test-graph)
                        false-graph <- (handle-instrs false
                                                      true-graph)
                        false-graph)]
                  [_
                   (add-interference-edges instr live-vars graph)]))))
           (handle-instrs
            (lambda (instrs graph)
              (foldl handle-pair
                     graph
                     instrs))))
    (lambda (instrs)
      (handle-instrs instrs
                     (make-graph)))))

(debug-define build-interference
  (lambda (prog)
    (match prog
      [`(program ,type ,typed-vars ,defines ,instrs)
       `(program ,type
                 ,typed-vars
                 ,(construct-graph instrs)
                 ,(map build-interference defines)
                 ,instrs)]
      [`(define (,name ,params ...) : ,type ,vars ,body)
       `(define (,name ,@params) : ,type
          ,vars
          ,(construct-graph body)
          ,body)])))

(debug-define manage-root-stack
  (lambda (prog)
    (letrec ((helper
              (lambda (typed-vars)
                (lambda (instr)
                  (match instr
                    [`((spill-vectors-to-root-stack) ,live-vars)
                     (concat-map (lambda (live-var)
                                   (let ((var-type (cdr (assv live-var typed-vars))))
                                     (match var-type
                                       [`(Vector ,vec-types ...)
                                        `(((movq (var ,live-var) (deref r15 0)) ,live-vars)
                                          ((addq (int 8) (reg r15)) ,live-vars))]
                                       [x (list)])))
                                 (sort live-vars symbol<?))]
                    [`((restore-vectors-from-root-stack) ,live-vars)
                     (concat-map (lambda (live-var)
                                   (let ((var-type (cdr (assv live-var typed-vars))))
                                     (match var-type
                                       [`(Vector ,vec-types ...)
                                        `(((addq (int -8) (reg r15)) ,live-vars)
                                          ((movq (deref r15 0) (var ,live-var)) ,live-vars))]
                                       [_ (list)])))
                                 (reverse (sort live-vars symbol<?)))]
                    [`((if ,test ,true ,false) ,live-vars)
                     `(((if ,test
                            ,(concat-map (helper typed-vars) true)
                            ,(concat-map (helper typed-vars) false))
                        ,live-vars))]
                    [x (list x)])))))
      (match prog
        [`(program ,type ,typed-vars ,graph ,defines ,instrs)
         `(program ,type ,typed-vars ,graph
                   ,(map manage-root-stack defines)
                   ,(concat-map (helper typed-vars) instrs))]
        [`(define (,name ,params ...) : ,type ,typed-vars ,graph ,body)
         `(define (,name ,@params) : ,type ,typed-vars ,graph
            ,(concat-map (helper typed-vars) body))]))))

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

;; I don't actually know what r11 is for, but see 5.3.3 for info
(define caller-save-regs
  '(
    ;; rdx
    ;; rcx
    ;; rsi
    ;; rdi
    ;; r8
    ;; r9
    ;; r10
    ;; r11
    ))

;; r15 is the top of the root stack
(define callee-save-regs
  '(
    rbx
    r12
    r13
    r14
    ;; r15
    ))

(define word-size 8)

(define get-stack-num
  (lambda (def)
    (match def
      [`(define (,_ ,vars ...) : ,_ ...)
       (max 0 (- (length vars) 6))]
      ;; [`(define (,_ ,params ...) : ,_ ,_ ,_ ,_)
      ;;  (max 0 (- (length vars) 6))]
      )))

;; need to add mappings to actual registers as well as stack locations
(debug-define allocate-registers
  (letrec ((assign-reg
            (lambda (mapping)
              (lambda (datum)
                (let ((recur (assign-reg mapping)))
                  (match datum
                    [`(var ,var)
                     (hash-ref mapping var)]
                    [`(if ,test ,true ,false)
                     `(if ,(map recur test)
                          ,(map recur true)
                          ,(map recur false))]
                    [`(,instr ,args ...)
                     `(,instr ,@(map recur args))]
                    [x x])))))
           (rem-live-vars
            (lambda (instrs)
              (if (null? instrs)
                  '()
                  (--> curr <- (car instrs)
                       rest <- (rem-live-vars (cdr instrs))
                       (match curr
                         [`((if ,test
                                ,true
                                ,false)
                            ,_)
                          (cons `(if ,(rem-live-vars test)
                                     ,(rem-live-vars true)
                                     ,(rem-live-vars false))
                                rest)]
                         [_ (cons (caar instrs)
                                  rest)]))))))
    (letrec ((get-stuff
              (lambda (type typed-vars graph instrs max-args-num)
                (--> get-color-var <- car
                     get-color-num <- cdr
                     colors <- (color-graph graph)
                     num-colors <- (if (null? colors)
                                       0
                                       (apply max 0 (map get-color-num colors)))
                     all-regs <- (append caller-save-regs
                                         callee-save-regs)
                     num-regs <- (length all-regs)
                     reg-mapping <- (map
                                     (lambda (color)
                                       (let ((color-num (cdr color)))
                                         (cons (get-color-var color)
                                               (if (>= (get-color-num color)
                                                       num-regs)
                                                   `(deref rsp ,(* word-size
                                                                   (+ max-args-num
                                                                      (- color-num
                                                                         num-regs))))
                                                   `(reg ,(list-ref all-regs
                                                                    color-num))))))
                                     colors)
                     reg-map <- (make-immutable-hasheqv reg-mapping)
                     (values (let ((stack-num (- (add1 num-colors)
                                                 (length all-regs))))
                               (max (* stack-num word-size) 0))
                             (map (lambda (vars)
                                    (concat-map (lambda (var)
                                                  (match (hash-ref reg-map var)
                                                    [`(reg ,reg)
                                                     `(,reg)]
                                                    [_ '()]))
                                                vars))
                                  (map cadr instrs))
                             (map (assign-reg reg-map)
                                  (rem-live-vars instrs))))))
             (allocate-def (lambda (max-args-num)
                             (lambda (def)
                               (match def
                                 [`(define (,name ,params ...) : ,type ,typed-vars ,graph ,body)
                                  (let-values ([(stack-num reg-mappings new-instrs)
                                                (get-stuff type typed-vars graph body max-args-num)])
                                    `(define (,name ,@params) :
                                       ,type ,typed-vars ,stack-num ,reg-mappings ,new-instrs))])))))
      (lambda (prog)
        (match prog
          [`(program ,type ,typed-vars ,graph ,defines ,instrs)
           (let ((max-args-num (apply max 0 (map get-stack-num defines))))
             (let-values ([(stack-num reg-mappings new-instrs)
                           (get-stuff type typed-vars graph instrs max-args-num)])
               `(program ,type
                         ,typed-vars
                         ,stack-num
                         ,reg-mappings
                         ,(map (allocate-def max-args-num) defines)
                         ,new-instrs)))]
          )))))

;; this will need modification if the saves and restores are to be offset by
;; more than a single instruction.
(debug-define add-register-saves
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
        [`(program ,type ,typed-vars ,num ,live-varss ,defines ,instrs)
         `(program ,type
                   ,num
                   ,defines
                   ,(helper live-varss instrs))]))))

(debug-define lower-conditionals
  (letrec ((lower-condition
            (lambda (instr)
              (match instr
                [`(if ((,cmp ,arg1 ,arg2))
                      ,true
                      ,false)
                 (let ((then-label (gensym))
                       (end-label (gensym)))
                   `((cmpq ,arg2 ,arg1)
                     (jmp-if equal (label ,then-label))
                     ,@(concat-map lower-condition
                                   false)
                     (jmp (label ,end-label))
                     (label-pos ,then-label)
                     ,@(concat-map lower-condition
                                   true)
                     (label-pos ,end-label)))]
                [_ (list instr)]))))
    (lambda (prog)
      (match prog
        [`(program ,type ,stack-num ,defines ,instrs)
         `(program ,type
                   ,stack-num
                   ,(map lower-conditionals defines)
                   ,(concat-map lower-condition
                                instrs))]
        [`(define (,name ,params ...) :
            ,type ,typed-vars ,stack-num ,reg-mappings ,instrs)
         `(define (,name ,@params) :
            ,type ,stack-num
            ,(concat-map lower-condition instrs))]))))

    ;; `(define (,name ,@params) :
    ;;    ,type ,typed-vars ,stack-num ,reg-mappings ,new-instrs))])))))

(define extend-env
  (lambda (var val env)
    (cons `(,var . ,val) env)))

(define extend-env-vars
  (lambda (cells env)
    (foldl (lambda (cell curr-env)
             (extend-env (car cell)
                         (cdr cell)
                         curr-env))
           env
           cells)))

(define is-type-boolean?
  (lambda (type)
    (eqv? type 'Boolean)))

(define check-type-int
  (lambda (type name)
    (if (eqv? type 'Integer)
        (void)
        (type-error name type 'Integer))))

(define check-types-equal
  (lambda (type1 type2 name)
    (if (not (eqv? type1 type2))
        (type-error name type2 type1)
        (void))))

(define type-error
  (lambda (func actual expected)
    (error func
           "Type error in ~a: Expected type was ~a, but got ~a"
           func
           expected
           actual)))

(define get-func-type-and-name
  (lambda (def)
    (match def
      [`(define (,name ,params ...) : ,type ,_)
       `(,name Function ,(map (compose parse-type caddr) params)
               ,(parse-type type))])))

(define parse-type
  (lambda (type)
    (if (list? type)
        (if (eq? (car type) 'Vector)
            (cons 'Vector (map parse-type (cdr type)))
            (let* ([arrowless (filter-not (lambda (x) (eq? x '->)) type)]
                   [ret-type (parse-type (car (reverse arrowless)))]
                   [arg-types (map parse-type (reverse (cdr (reverse arrowless))))])
              `(Function ,arg-types ,ret-type)))
        type)))


(define typecheck-R4-curry
  (lambda (env)
    (lambda (exp)
      (let ((recur (typecheck-R4-curry env)))
        (match exp
          [`(program ,defines ... ,body)
           (--> define-infos <- (map get-func-type-and-name defines)
              global-env <- (extend-env-vars define-infos env)
              typed-defines <- (map (typecheck-R4-curry global-env) defines)
              typed-body <- ((typecheck-R4-curry global-env) body)
              (match typed-body
                [`(has-type ,typed-body ,type)
                 `(program (type ,type)
                           ,typed-defines
                           ((has-type ,typed-body ,type)))]))]
          [`(define (,name ,vars ...) : ,type ,body)
           (let ((typed-body ((typecheck-R4-curry
                               (extend-env-vars (map (lambda (wrong-cell)
                                                       (cons (car wrong-cell)
                                                             (parse-type (caddr wrong-cell))))
                                                     vars)
                                                env)) body)))
             (if (not (equal? (parse-type type) (get-type typed-body)))
                 (type-error name (parse-type type) (get-type typed-body))
                 `(define (,name ,@vars) : ,(parse-type type) ,typed-body)))]
          [(? fixnum?)
           `(has-type ,exp Integer)]
          [(? boolean?)
           `(has-type ,exp Boolean)]
          [(? symbol?)
           `(has-type ,exp ,(variable-lookup exp env))]
          [`(read)
           `(has-type ((has-type read built-in)) Integer)]
          [`(void)
           `(has-type ((has-type void built-in)) Void)]
          [`(vector ,xs ...)
           (let ((xs-typed (map recur xs)))
             `(has-type ((has-type vector built-in)
                         ,@xs-typed)
                        (Vector ,@(map caddr xs-typed))))]
          [`(vector-ref ,vec-exp ,(? exact-nonnegative-integer? idx))
           (let ((typed-vec-exp (recur vec-exp))
                 (typed-idx-exp `(has-type ,idx Integer)))
             (match typed-vec-exp
               [`(has-type ,_ (Vector ,vec-types ...))
                (if (< idx (length vec-types))
                    `(has-type ((has-type vector-ref built-in)
                                ,typed-vec-exp
                                ,typed-idx-exp)
                               ,(list-ref vec-types idx))
                    (type-error 'vector-ref vec-types 'too-long))]
               [_ (type-error 'vector
                              (caddr typed-vec-exp)
                              'Vector-something)]))]
          [`(vector-set! ,vec ,(? exact-nonnegative-integer? idx) ,exp)
           (let ((typed-vec-exp (recur vec))
                 (typed-val-exp (recur exp)))
             (match `(,typed-vec-exp ,typed-val-exp)
               [`((has-type ,_ (Vector ,vec-types ...))
                  (has-type ,_ ,exp-type))
                (if (and (< idx (length vec-types))
                         (equal? exp-type (list-ref vec-types idx)))
                    `(has-type ((has-type vector-set! built-in)
                                ,typed-vec-exp
                                (has-type ,idx Integer)
                                ,typed-val-exp)
                               Void)
                    (type-error 'vector-set! exp-type (list-ref vec-types idx)))]
               [_ (type-error 'vector-set! 'idk-man 'something-good)]))]
          [`(lambda: ([,param-names : ,param-types] ...) : ,type ,body)
           (let ((typed-body ((typecheck-R4-curry (extend-env-vars (map (lambda (name type)
                                                                          (cons name (parse-type type)))
                                                                        param-names
                                                                        param-types)
                                                                   env))
                              body)))
             (if (not (equal? (get-type typed-body) (parse-type type)))
                 (type-error 'lambda type (get-type typed-body))
                 `(has-type (lambda ,(map (lambda (name type) `(,name : ,(parse-type type)))
                                     param-names
                                     param-types) :
                              ,(parse-type type)
                              ,typed-body)
                            (Function ,(map parse-type param-types) ,(parse-type type)))))]
          [`(- ,arg)
           (match (recur arg)
             [`(has-type ,typed-arg Integer)
              `(has-type ((has-type - built-in)
                          (has-type ,typed-arg Integer))
                         Integer)]
             [`(has-type ,typed-arg ,bad-type)
              (type-error '- bad-type 'Integer)])]
          [`(,(and (or '< '> '<= '>=)
                   op) ,arg1 ,arg2)
           (match `(,(recur arg1) ,(recur arg2))
             [`((has-type ,typed-arg1 Integer)
                (has-type ,typed-arg2 Integer))
              `(has-type ((has-type ,op built-in)
                          (has-type ,typed-arg1 Integer)
                          (has-type ,typed-arg2 Integer))
                         Boolean)]
             [`((has-type ,_ ,bad-arg1-type)
                (has-type ,_ ,bad-arg2-type))
              (type-error op
                          `(,bad-arg1-type ,bad-arg2-type)
                          `(Integer Integer))])]
          [`(,(and (or 'and)
                   rator)
             ,arg1
             ,arg2)
           (match `(,(recur arg1)
                    ,(recur arg2))
             [`((has-type ,typed-arg1 Boolean)
                (has-type ,typed-arg2 Boolean))
              `(has-type ((has-type ,rator built-in)
                          (has-type ,typed-arg1 Boolean)
                          (has-type ,typed-arg2 Boolean))
                         Boolean)])]
          [`(,(and (or 'eq?)
                   rator)
             ,arg1
             ,arg2)
           (match `(,(recur arg1)
                    ,(recur arg2))
             [`((has-type ,typed-arg1 ,type)
                (has-type ,typed-arg2 ,type))
              `(has-type ((has-type ,rator built-in)
                          (has-type ,typed-arg1 ,type)
                          (has-type ,typed-arg2 ,type))
                         Boolean)])]
          ;; (let ((arg1-type (recur arg1))
          ;;       (arg2-type (recur arg2)))
          ;;   (check-types-equal arg1-type arg2-type 'eq?)
          ;;   'Boolean)]
          [`(+ ,arg1 ,arg2)
           (match `(,(recur arg1)
                    ,(recur arg2))
             [`((has-type ,typed-arg1 Integer)
                (has-type ,typed-arg2 Integer))
              `(has-type ((has-type + built-in)
                          (has-type ,typed-arg1 Integer)
                          (has-type ,typed-arg2 Integer))
                         Integer)]
             [`((has-type ,_ ,bad-arg1-type)
                (has-type ,_ ,bad-arg2-type))
              (type-error '+
                          `(,bad-arg1-type ,bad-arg2-type)
                          `(Integer Integer))])]
          [`(not ,exp)
           (match (recur exp)
             [`(has-type ,typed-exp Boolean)
              `(has-type ((has-type not built-in) (has-type ,typed-exp Boolean)) Boolean)])]
          [`(if ,test ,true ,false)
           (let ((test-typed (recur test))
                 (true-typed (recur true))
                 (false-typed (recur false)))
             (match `(,test-typed
                      ,true-typed
                      ,false-typed)
               [`((has-type ,_ Boolean)
                  (has-type ,_ ,branch-type)
                  (has-type ,_ ,branch-type))
                `(has-type (if ,test-typed
                               ,true-typed
                               ,false-typed)
                           ,branch-type)]))]
          [`(let ((,vars ,vals) ...) ,body)
           (--> typed-pairs <- (map (lambda (var val)
                                   (list var (recur val)))
                                 vars
                                 vals)
              typed-body <- ((typecheck-R4-curry (foldl (lambda (pair ext-env)
                                                         (extend-env (car pair)
                                                                     (match (cadr pair)
                                                                       [`(has-type ,_
                                                                                   ,type)
                                                                        type])
                                                                     ext-env))
                                                       env
                                                       typed-pairs))
                            body)
              (match typed-body
                [`(has-type ,_ ,type)
                 `(has-type (let ,typed-pairs
                              ,typed-body)
                            ,type)]))]
          [`(,func ,params ...)
           (let ((func-typed (recur func))
                 (params-typed (map recur params)))
             (let ((func-type (get-type func-typed)))
               (match func-type
                 [`(Function ,param-types ,return-type)
                  (if (not (equal? (map get-type params-typed)
                                   param-types))
                      (type-error 'user-func param-types (map get-type params-typed))
                      `(has-type (,func-typed ,@params-typed) ,return-type))])))]
          )))))

(debug-define typecheck-R4
  (typecheck-R4-curry '()))

(define built-ins
  (map (lambda (x)
         (cons x x))
       '(+ - read < > <= >= eq? and not void vector-set! vector vector-ref)))

(define run-all
  (compose
   print-instructions
   patch-instructions
   add-bookkeeping
   lower-conditionals
   add-register-saves
   allocate-registers
   manage-root-stack
   build-interference
   uncover-live
   select-instructions
   flatten
   expose-allocation
   convert-to-closures
   reveal-functions
   uniquify
   typecheck-R4
   ))

(define run-some
  (compose
   (lambda (prog) (time (allocate-registers prog)))
   (lambda (prog) (time (build-interference prog)))
   (lambda (prog) (time (uncover-live prog)))
   (lambda (prog) (time (select-instructions prog)))
   (lambda (prog) (time (flatten prog)))
   (lambda (prog) (time ((uniquify (u-state built-ins 0)) prog)))))

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

         live-after-sets

         -->
         >>=
         concat-map

         get-param-locations
         lower-conditionals
         print-instructions
         patch-instructions
         add-bookkeeping
         add-register-saves
         manage-root-stack
         allocate-registers
         build-interference
         uncover-live
         select-instructions
         convert-to-closures
         flatten
         expose-allocation
         reveal-functions
         uniquify
         typecheck-R4
         u-state
         built-ins
         run-all
         )
