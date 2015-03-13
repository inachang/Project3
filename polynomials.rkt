#lang racket
;Yiana Chang and Shant Hairapetian
;March 17, 2015
;COMP333 Project 3 - Polynomials

(define coeff (lambda (t)
                (car t)))

(define expon (lambda (t)
                (last t)))

(define printTerm (lambda (t)
                    (if (= (coeff t) 0) (void) 
                        (begin
                          (let*
                              ([c (coeff t)]
                               [e (expon t)])
                            (cond
                              ((= c 1) (display ""))
                              ((= c -1) (display "-"))
                              (else (display c)))
                            (cond
                              ((= e 0) (display ""))
                              ((= e 1) (display "x"))
                              (else
                               (display "x^")
                               (display e))))))))

(define printpoly (lambda (p)
                    (cond
                      ((null? p) p)
                      ((= (length p) 1)
                       (printTerm (car p)))
                      (else 
                       (begin
                         (if (= (caar p) 0)
                             (printpoly (cdr p))
                             (begin
                               (if (and (= (length (cdr p)) 1) (= (caadr p) 0))
                                   (printTerm (car p))
                                   (begin
                                     (printTerm (car p))
                                     (display " + ")
                                     (printpoly (cdr p)))))))))))

(define evalpoly (lambda (p v)
                   (if (= (length p) 1)
                      (* (coeff (car p)) (expt v (expon (car p))))
                      (+ (* (coeff (car p)) (expt v (expon (car p))))
                        (evalpoly (cdr p) v)))))
                   
(define GT (lambda (t1 t2)
             (if (> (expon t1) (expon t2)) #t #f)))

(define EQExp? (lambda (t1 t2)
                 (if (= (expon t1) (expon t2)) #t #f)))



(define addTerm (lambda (t1 t2)
                  (list (+ (coeff t1) (coeff t2)) (expon t1)))) 


(define simplify (lambda (p)
  (simplifyRec (sort p GT))
))

(define simplifyRec (lambda (p)
  (if (<= (length p) 1) (list (car p))
     (let*(
       [t1 (car p)]
       [t2 (cadr p)]
      )
      (if (EQExp? t1 t2 )
       (if (> (length(cddr p)) 0)
        (simplifyRec (append (list (addTerm t1 t2)) (cddr p)))
        (append (list (addTerm t1 t2)))
       )
       (append (list (car p)) (simplifyRec (cdr p)))
      )
     ) 
  )
))

(define addpoly (lambda (p1 p2)
 (simplify (append p1 p2))
))


(define subtractpoly (lambda (p1 p2)
 (simplify (append p1 (map (lambda (x) (list (* (car x) -1) (cadr x))) p2)))
))

(define multiplyterms (lambda (t1 t2)
  (list (* (coeff t1)(coeff t2)) (+ (expon t1)(expon t2)))
))

(define multiplytermpoly (lambda (t1 p1)
  (simplify (map (lambda (x) (multiplyterms t1 x)) p1))
))

(define multiplypolyRec (lambda (p1 p2 acc)
  (if  (= (length p1) 0)
    acc
    (multiplypolyRec (cdr p1) p2 (append acc (multiplytermpoly (car p1) p2)))
  )
))

(define multiplypoly (lambda (p1 p2)
  (let* ([acc '()])
   (simplify (multiplypolyRec p1 p2 acc))
  )
  
))