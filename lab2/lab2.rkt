#lang racket

(define (permutations size)
  (let ((elements (list #t #f)))
    (if (zero? size)
        '(())
        (append-map (lambda (p)
                      (map (lambda (e) (cons e p)) elements))
                    (permutations (sub1 size))))))

(define (implies p q)
  (or (not p) q))

(define (expr1 p q r)
  (implies p (implies q r)))

(define (expr2 p q r)
  (implies q (implies p r)))

(define (expr3 p q r)
  (implies r (implies q p)))

;q -> ~p
(define (expr4 p q)
  (implies q (not p)))

;~p ^ q
(define (expr5 p q)
  (and (not p) q))

;(~p V q) -> r
(define (expr6 p q r)
  (implies (or (not p) q) r))

;((r ^ (q -> (~p))) -> p)
(define (expr7 p q r)
  (implies (and r (implies q (not p))) p))

;(p->q)V(r->s)
(define (expr8 p q r s)
  (or (implies p q) (implies r s)))

;(p^q)V(r^s)
(define (expr9 p q r s)
  (or (and p q) (and r s)))
  
;map applies the function (apply expr (list-ref (permutations (procedure-arity expr)) i)))
;to the build-list conatining i
(define (truth-table expr)
  (map (lambda (i)
         (apply expr (list-ref (permutations (procedure-arity expr)) i)))
         (build-list (length (permutations (procedure-arity expr))) values)))