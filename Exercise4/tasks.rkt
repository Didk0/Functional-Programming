(define (1+ x) (+ x 1))
(define (id x) x)

; accumulate recursive
(define (accumulate-rec op nv a b next term)
  (if (> a b)
      nv
      (op (term a) (accumulate-rec op nv (next a) b next term))))

(define (fact n)
  (accumulate-rec * 1 1 n 1+ id))

; accumulate with opashkova rekursiq
(define (accumulate op acc a b next term)
  (if (> a b)
      acc
      (accumulate op (op acc (term a)) (next a) b next term)))

(define (fact n)
  (accumulate * 1 1 n 1+ id))

(define (all? p? a b)
  (accumulate (lambda (x y) (and x y)) #t a b 1+ p?))

(define (count-divisors n)
  (accumulate + 0 1 n 1+ (lambda (i) (if (= (remainder n i) 0) 1 0))))

(define (is-prime? n)
  (if (= n 1) #f
  (= 2 (count-divisors n))))

(define (count-primes a b)
  (accumulate + 0 a b 1+ (lambda (x) (if (is-prime? x) 1 0))))

(define (list-length lst)
  (if (null? lst) 0
      (+ 1 (list-length (cdr lst)))))

(define (contains? x lst)
  (cond ((null? lst) #f)
        ((equal? (car lst) x) #t)
        (else (contains? x (cdr lst)))))

(define (list-identity lst)
  (if (null? lst) '()
      (cons (car lst) (list-identity (cdr lst)))))

(define (reverse-list lst)
  (define (reverse-helper curr res)
    (if (null? curr) res
        (reverse-helper (cdr curr) (cons (car curr) res))))
  (reverse-helper lst '()))