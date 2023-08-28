(define (my-filter p lst)
  (cond ((null? lst) '())
        ((p (car lst)) (cons (car lst) (my-filter p (cdr lst))))
        (else (my-filter p (cdr lst)))))

(define (uniques lst)
  (if (null? lst) '()
      (cons (car lst) (uniques (my-filter (lambda (i) (not (equal? i (car lst)))) lst)))))
  
;zad 1
;I nachin
(define (uniques* lst)
  (define (loop lst res)
    (cond ((null? lst) res)
          ((member (car lst) res) (loop (cdr lst) res))
          (else (loop (cdr lst) (cons (car lst) res)))))
  (loop lst '()))

;II nachin
(define (uniques** l)
  (cond ((null? l) l)
        ((member (car l) (cdr l)) (uniques (cdr l)))
        (else (cons (car l) (uniques (cdr l))))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

;III nachin
(define (uniques*** lst)
  (foldr (lambda (el res) (if (member el res) res (cons el res))) '() lst))

(define (begins-with? needle haystack)
  (cond ((null? needle) #t)
        ((or (null? haystack) (not (equal? (car needle) (car haystack)))) #f)
        (else (begins-with? (cdr needle) (cdr haystack)))))
  
;zad 4
(define (sublist? needle haystack)
  ;(and (not (null? haystack)) (or (begins-with? needle haystack) (sublist? needle (cdr haystack)))
  (cond ((null? haystack) #f)
        ((begins-with? needle haystack) #t)
        (else (sublist? needle (cdr haystack)))))

;zad 2
(define (insert val lst)
  (cond ((null? lst) (list val))
        ((< (car lst) val) (cons (car lst) (insert val (cdr lst))))
        (else (cons val lst))))

;zad 3
;I nachin
(define (insertion-sort lst)
  (if (null? lst) '()
      (insert (car lst) (insertion-sort (cdr lst)))))

;II nachin
(define (insertion-sort lst)
  (foldr insert '() lst))

;zad 5
(define (add x val lst)
  (cond ((null? lst) (list (cons val (list x))))
        ((equal? val (car (car lst))) (cons (list (car (car lst))) (cons x (cdr (car lst)))) (cdr lst))
        (else (cons (car lst) (add x val (cdr lst))))))

(define (group-by f lst)
  (foldr (lambda (el res) (add el (f el) res)) '() lst))

(define (group-by* f lst)
  (let* ((values (map f lst))
         (unique-values (uniques*** values)))
    (map (lambda (val) (list val (my-filter (lambda (x) (equal? (f x) val)) lst))) unique-values)))
         