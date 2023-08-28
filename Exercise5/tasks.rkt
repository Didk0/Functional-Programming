(define (1+ x) (+ x 1))
(define (id x) x)

(define (accumulate op nv a b next term)
  (define (accumulate-helper curr res)
    (if (> curr b)
        res
        (accumulate-helper (next curr) (op (term curr) res))))
  (accumulate-helper a nv))

(define (count-divisors n)
  (accumulate + 0 1 n 1+ (lambda (i) (if (= 0 (remainder n i)) 1 0))))

(define (prime? n)
  (if (= n 1)
      #f
      (= 2 (count-divisors n))))

(define (list-from-range a b next)
  (if (> a b) '()
      (cons a (list-from-range (next a) b next))))

(define (my-map f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (my-map f (cdr lst)))))

(define (filter p? lst)
  (cond ((null? lst) '())
        ((p? (car lst)) (cons (car lst) (filter p? (cdr lst))))
        (else (filter p? (cdr lst)))))

;функция, която прави списък от простите числа от a до b
(define (list-primes a b)
  (filter prime? (list-from-range a b 1+)))

(define M '((-1 5 7)
            (42 13 37)
            (66 -19 0)))


(define (get-item-at list i)
  (if (= i 0)
      (car list)
      (get-item-at (cdr list) (- i 1))))

(define (get-row matrix i)
  (get-item-at matrix i))

(define (get-col matrix i)
  (cond ((null? matrix) '())
        ((= i 0) (my-map car matrix))
        (else (get-col (my-map cdr matrix) (- i 1)))))

(define (main-diagonal matrix)
  (define (diagonal-helper i)
    (if (= i (length matrix))
        '()
        (cons (get-item-at (get-row matrix i) i) (diagonal-helper (+ i 1)))))
  (diagonal-helper 0))
  

(define (transpose matrix)
  (define i 0)
  (if (= i (length matrix)) '()
      (cons (get-col matrix i) (transpose (cdr matrix)))))

(define (transpose matrix)
  (define (transpose-helper i)
    (if (= i (length (car matrix))) '()
      (cons (get-col matrix i) (transpose-helper (+ i 1)))))
  (if (null? matrix) '()
      (transpose-helper 0)))


(define (foldr op nv lst)
  (if (null? lst)
      nv
      (op (car lst) (foldr op nv (cdr lst)))))

(define (foldl op nv lst)
  (if (null? lst)
      nv
      (foldl op (op (car lst) nv) (cdr lst))))

(define (any p? lst)
  (foldl (lambda (x y) (or (p? x) y)) #f lst))

;flatten '((123) '() ((((("hello") #t) #f)))) -> (123 "hello" #t #f)
;(define (flatten lst))