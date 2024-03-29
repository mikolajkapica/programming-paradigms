(define (cutAndMend lst a b)
    (define (aux lst current)
        (cond ((null? lst) '())
              ((and (>= current a) (<= current b)) (aux (cdr lst) (+ current 1)))
              (else (cons (car lst) (aux (cdr lst) (+ current 1))))))
    (aux lst 0))