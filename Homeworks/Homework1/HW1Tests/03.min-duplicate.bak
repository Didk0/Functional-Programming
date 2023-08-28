#lang racket
(require rackunit rackunit/text-ui)

;### Зад 3
; Намира най-малкото число, което се среща поне 2 пъти в списъка `l`.
(define (is-duplicate? x l)
  (define (helper x l count)
    (cond ((and (null? l) (< count 2)) #f)
          ((>= count 2) #t)
          ((= x (car l)) (helper x (cdr l) (+ 1 count)))
          (else (helper x (cdr l) count))))
  (helper x l 0))

(define (find-min l)
  (define (helper res l)
    (cond ((null? l) res)
          ((helper (min (car l) res) (cdr l)))))
  (helper (car l) (cdr l)))

(define (my-filter p? l)
  (cond ((null? l) '())
        ((p? (car l)) (cons (car l) (my-filter p? (cdr l))))
        (else (my-filter p? (cdr l)))))

(define (min-duplicate l)
  (find-min (my-filter (lambda (i) (is-duplicate? i l)) l)))

(run-tests
  (test-suite "min-duplicate tests"
    (check-eq? (min-duplicate '(-8 -8))
               -8)
    (check-eq? (min-duplicate '(1 2 3 4 4 5 6))
               4)
    (check-eq? (min-duplicate '(5 1 2 3 4 5 3 6 2 3 2 3 2 3))
               2)
    (check-eq? (min-duplicate '(3 2 2 2 1 1))
               1))
  'verbose)
