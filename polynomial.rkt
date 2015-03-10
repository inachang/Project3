#lang racket

(define coeff (lambda (t)
                (car t)))

(define expon (lambda (t)
                (last t)))

(define printTerm (lambda (t)
                    (display (coeff t))
                    (cond
                      ((= (expon t) 0) (newline))
                      ((= (expon t) 1) (display "x"))
                      (else
                       (display "x^")
                       (display (expon t))))))

(define printpoly (lambda (p)
                    (cond
                      ((null? p) (newline))
                      ((= (length p) 1)
                       (printTerm (car p))
                       (printpoly (cdr p)))
                      (else 
                       (begin
                         (printTerm (car p))
                         (display " + ")
                         (printpoly (cdr p)))))))

(define evalpoly (lambda (p v)
                   
  