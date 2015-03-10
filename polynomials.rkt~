#lang racket
;Yiana Chang and Shant Hairapetian

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
                              ((= e 0) (newline))
                              ((= e 1) (display "x"))
                              (else
                               (display "x^")
                               (display e))))))))

(define printpoly (lambda (p)
                    (cond
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

