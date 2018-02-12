#lang racket


(define convert-cps
  (lambda (prog)
    (transform-form prog '(lambda (x) x))))

(define expand-form
  (lambda (form)
    (match form
      [`(lambda (,x) ,body)
       (let ((new-cont (gensym 'cont)))
         `(lambda (,x ,new-cont)
            ,(transform-form body new-cont)))]
      [sym sym])))

(define transform-form
  (lambda (form into-cont)
    (match form
      [`(lambda (,x) ,body)
       `(,into-cont ,(expand-form form))]
      [`(let ((,var ,val)) ,body)
       (let ((val-cont (gensym 'var)))
         (transform-form val
                         `(lambda (,val-cont)
                            (let ((,var ,val-cont))
                              ,(transform-form body into-cont)))))]
      [`(if ,test ,true ,false)
       (let ((test-cont (gensym 'test)))
         (transform-form test
                         `(lambda (,test-cont)
                            (if ,test-cont
                                ,(transform-form true into-cont)
                                ,(transform-form false into-cont)))))]
      ;; (λ (f cc) (f (λ (x _) (cc x)) cc))
      [`(call/cc ,receiver)
       (let ((receiver-cont (gensym 'receiver)))
         (transform-form receiver
                         `(lambda (,receiver-cont)
                            ((lambda (f cc)
                               (f (lambda (x _)
                                    (cc x))
                                  cc))
                             ,receiver-cont
                             ,into-cont))))]
      [`(+ ,rand1 ,rand2)
       (let ((rand1-cont (gensym 'rand1-cont))
             (rand2-cont (gensym 'rand2-cont)))
         (transform-form rand1
                         `(lambda (,rand1-cont)
                            ,(transform-form rand2
                                             `(lambda (,rand2-cont)
                                                (,into-cont (+ ,rand1-cont ,rand2-cont)))))))]
      [`(,rator ,rand)
       (let ((rator-cont (gensym 'rator-cont))
             (rand-cont (gensym 'rand-cont)))
         (transform-form rator
                         `(lambda (,rator-cont)
                            ,(transform-form rand
                                             `(lambda (,rand-cont)
                                                (,rator-cont ,rand-cont ,into-cont))))))]
      [sym
       `(,into-cont ,sym)])))


 ; Int -> Int
((lambda ([receiver150 : ((Int -> Int) -> (Int -> Int))])
   ((lambda ([f : ((Int -> Int) -> (Int -> Int))] cc) : (Int -> Int)
      (f (lambda (x _) (cc x)) cc))
    receiver150
    (lambda (rator-cont148)
      ((lambda (rand-cont149)
         (rator-cont148 rand-cont149
                        (lambda (x) x)))
       5))))
 (lambda ([k : Int -> Int] cont151)
   (cont151 k)))

;; ((lambda (receiver150)
;;    ((lambda (f cc)
;;       (f (lambda (x _) (cc x)) cc))
;;     receiver150
;;     (lambda (rator-cont148)
;;       ((lambda (rand-cont149)
;;          (rator-cont148 rand-cont149
;;                         (lambda (x) x)))
;;        5))))
;;  (lambda (k cont151) (cont151 k)))


(display
 (convert-cps '((call/cc (lambda (k) k)) 5)))
;; ((lambda (rator-cont104)
;;    ((lambda (rand-cont105)
;;       (rator-cont104 rand-cont105 return))
;;     x))
;;  f)

;; ((lambda (rand1-cont148)
;;    ((lambda (receiver150) ((lambda (f cc) (f (lambda (x _) (cc x)) cc)) receiver150 (lambda (rand2-cont149) ((lambda (x) x) (+ rand1-cont148 rand2-cont149))))) (lambda (k cont151) ((lambda (rator-cont152) ((lambda (rand-cont153) (rator-cont152 rand-cont153 cont151)) 2)) k))))
;;  1)
