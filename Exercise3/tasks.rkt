;sum-interval
(define (sum-interval start end)
  (define (sum-helper curr res)
    (if (> curr end)
          res
          (sum-helper (+ 1 curr) (+ curr res))))
  (sum-helper start 0))

;product-interval
(define (product-interval start end)
    (if (> start end)
          1
          (* start (product-interval (+ start 1) end))))

;sum-cubes-interval
(define (sum-cubes-interval start end)
  (define (cube x) (*(* x x)x))
  (define (cubes-helper curr res)
    (if (> curr end)
        res
        (cubes-helper (+ 1 curr) (+ (cube curr) res))))
  (cubes-helper start 1))

;sum-interval-even
(define (sum-interval-even start end)
  (define (sum-even-helper curr res)
    (cond ((> curr end) res)
        ((even? curr)(sum-even-helper (+ 2 curr) (+ res curr)))
        (else (sum-even-helper (+ 1 curr) res))))
  (sum-even-helper start 0))

;sum-interval-even-rec
(define (sum-interval-even-rec start end)
  (cond ((> start end) 0)
        ((even? start) (+ start (sum-interval-even-rec (+ start 2) end)))
        (else (sum-interval-even-rec (+ start 1) end))))

;combine-interval
(define (combine-interval start end op null-value)
  (if (> start end)
      null-value
      (op start (combine-interval (+ 1 start) end op null-value))))

(define (plus1 x) (+ x 1))

(define (id x) x)

(define (accumulate start end op term next null-value)
  (if (> start end)
      null-value
      (op (term start) (accumulate (next start) end op term next null-value))))

(define (combine-interval2 start end)
  (accumulate start end + (lambda (x) x) (lambda (x) (+ 1 x)) 0))

(define (sum-interval2 start end)
  (combine-interval start end + 0))

;accumulate-iterative

(define (sum-interval3 start end)
  (accumulate start end + (lambda (x) x) (lambda (x) (+ x 1)) 0))

((lambda (x) (+ x 1)) 5)
((lambda (x y) (+ x y)) 2 3)
;(lambda (<arguments>) <body>)

(define (cube x) (expt x 3)) ; (define cube (lambda (x) (expt x 3))


; функция, която ни казва дали число е по-голямо от 10
(lambda (x) (> x 10))
; функция, която ни дава остатъка при деление с 5
(lambda (x) (remainder x 5))
; функция, която приема друга такава и я извиква с 6
(lambda (f) (f 6))

; compose - ((f . g) x) -> (f (g(x))
(define (compose f g)
  (lambda (x) (f (g x))))

; искаме функция, която е n-кратно прилагане на f
(define (repeat f n)
  (if (= n 1) f
      (compose f (repeat f (- n 1)))))

(define (repeat-iter f n)
  (define (repeat-helper f g n)
    (if (= n 1) f
        (repeat-helper (compose f g) g (- n 1))))
  (repeat-helper f f n))

; ((repeat (lambda (x) (+ x 1)) 5) 1)
