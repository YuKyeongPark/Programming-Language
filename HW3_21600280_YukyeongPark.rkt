#lang plai
;; Problem 1:
; Solved by myself: y
; Time taken: about 2 hours

(define-type SDFAE
    [num (n number?)]
    [add (lhs SDFAE?) (rhs SDFAE?)]
    [sub (lhs SDFAE?) (rhs SDFAE?)]
    [id  (name symbol?)]
    [fun (sd symbol?) (param symbol?) (body SDFAE?)]
    [app (ftn SDFAE?) (arg SDFAE?)])

; [contract] parse : string → SDFAE
; [purpose] to convert string to SDFAE
; [test] (test (parse '{with {x 3} {with {f {s fun {y} {- x y}}} {with {y 5}(app (fun 's 'x (app (fun 's 'f (app (fun 's 'y (app (id 'f) (num 4))) (num 5))) (fun 's 'y (sub (id 'x) (id 'y))))) (num 3)))
;        (test (parse '{with {y 4} {+ 1 {f {d fun {y} {+ 2 y}}}}})(app (fun 's 'y (add (num 1) (app (id 'f) (fun 'd 'y (add (num 2) (id 'y))))))(num 4)))
 
(define (parse sexp)
  (match sexp
    [(? number?)                (num sexp)]
    [(list '+ l r)              (add (parse l) (parse r))]
    [(list '- l r)              (sub (parse l) (parse r))]
    [(list 'with (list i v) e)  (app (fun 's i (parse e)) (parse v))]
    [(? symbol?)                (id sexp)]
    [(list f a)                 (app (parse f) (parse a))]
    [(list 'd 'fun (list p) b)  (fun 'd p (parse b))]
    [(list 's 'fun (list p) b)  (fun 's p (parse b))]
    [else                       (error 'parse "bad syntax: ~a" sexp)]))


(define-type SDFAE-Value
  [numV     (n number?)]
  [closureV (sd symbol?) (param symbol?) (body SDFAE?) (ds DefrdSub?)])

(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?) (value SDFAE-Value?) (ds DefrdSub?)])

; num-op: (number number -> number) -> (SDFWAE SDFWAE -> SDFWAE)
(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x) (numV-n y)))))
(define num+ (num-op +))
(define num- (num-op -))

; lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()          (error 'lookup "free identifier")]
    [aSub  (i v saved) (if (symbol=? i name)
                           v
                           (lookup name saved))]))

;; Problem 2:
; Solved by myself: y
; Time taken: about  hours
; [contract] interp : SDFAE → result
; [purpose] to interpret SDFAE to real value
; [test] (test (interp (parse '{with {x 3} {with {f {s fun {y} {+ x y}}} {with {x 5} {f 2}}}}) (mtSub)) (numV 5))
;        (test (interp (parse '{with {x 3} {with {f {d fun {y} {+ x y}}} {with {x 5} {f 2}}}}) (mtSub)) (numV 7))
;        (test (interp (parse '{with {x 3} {with {f {d fun {x} {+ x y}}} {with {y 5} {f 3}}}}) (mtSub)) (numV 8))
(define (interp sdfae ds)
  (type-case SDFAE sdfae
    [num   (n)     (numV n)]
    [add   (l r)   (num+ (interp l ds) (interp r ds))]
    [sub   (l r)   (num- (interp l ds) (interp r ds))]
    [id    (s)     (lookup s ds)]
    [fun   (s p b) (closureV s p b ds)]
    [app   (f a)   (local
                     [(define f-val (interp f ds))
                      (define a-val (interp a ds))]
                     (interp (closureV-body f-val)
                             (aSub (closureV-param f-val)
                                   a-val
                                   (if (symbol=? 'd (closureV-sd f-val))
                                       ds
                                       (closureV-ds f-val)))))]))
