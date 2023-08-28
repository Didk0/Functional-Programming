;zad 1
(define (accumulate op nv a b term next)
  (if (> a b) nv
          (op (term a) (accumulate op nv (next a) b term next))))

(define (divisors-sum num)
  (define (helper div)
    (cond ((>= div (/ num 2)) (if (= (remainder num div) 0) div 0))
          ((= (remainder num div) 0) (+ div (helper (+ 1 div))))
          (else (helper (+ 1 div)))))
  (helper 1))

(define (done? num)
  (= (divisors-sum num) (+ 2 num)))

(define (1+ x) (+ 1 x))

(define (almost-done? num a b)
  (accumulate (lambda (x y) (or x y)) #f a b (lambda (i) (if (and (done? i) (< (abs (- num i)) (abs (- num a))) (< (abs (- num i)) (abs (- num b)))) #t #f)) 1+))
  
(define (sum-almost-done a b)
  (accumulate + 0 a b  (lambda (i) (if (almost-done? i a b) i 0)) 1+))

;zad 2
(define (do-operation op times lst)
  (define (helper op times lst res)
    (if   (or (null? (cdr lst)) (= times 0)) lst
          (helper op (- times 1) (cons (op (car lst) (cadr lst)) (cddr lst)) (cons (op (car lst) (cadr lst)) res))))
  (helper op times lst '()))

(define (run-machine ilst)
  (define (helper ilst stack)
    (cond ((null? ilst) stack)
          ((or (number? (car ilst)) (symbol? (car ilst))) (helper (cdr ilst) (cons (car ilst) stack)))
          ((procedure? (car ilst)) (helper (cdr ilst ) (map (lambda (x) (if (number? x) ((car ilst) x) x)) stack)))
          ((and (pair? (car ilst)) (number? (car stack)) (number? (car (cdr stack)))) (helper (cdr ilst) (do-operation (caar ilst) (cdr (car ilst)) stack)))
          (else (helper (cdr ilst) stack))))
  (helper ilst '()))

;zad 3
(define (are-major? l1 l2)
  (cond ((not (= (length l1) (length l2))) #f)
        ((null? l1) #t)
        ((> (car l1) (car l2)) #f)
        (else (are-major? (cdr l1) (cdr l2)))))

(define (are-major-sublist? l1 l2)
  (cond ((< (length l2) (length l1)) #f)
        ((are-major? l1 l2) #t)
        (else (are-major-sublist? l1 (cdr l2)))))