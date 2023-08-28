#lang racket
(require rackunit rackunit/text-ui)

;### Зад 2
; Броят на наредените двойки цели числа от интервала [`a`,`b`],
; които имат най-голям общ делител равен на `n`.
(define (id x) x)
(define (1+ x) (+ 1 x))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (accumulate op (op nv (term a)) (next a) b term next)))

(define (count-pairs-gcd-for-one num a b n)
  (accumulate + 0 a b (lambda (i) (if (= (gcd num i) n) 1 0)) 1+))

(define (count-pairs-gcd n a b)
  (accumulate + 0 a b (lambda (i) (count-pairs-gcd-for-one i a b n)) 1+))

(run-tests
  (test-suite "count-pairs-gcd tests"
    (check-eq? (count-pairs-gcd 10 1 11)
               1)
    (check-eq? (count-pairs-gcd 3 1 11)
               7)
    (check-eq? (count-pairs-gcd 16 1 11)
               0)
    (check-eq? (count-pairs-gcd 4 1 11)
               3))
  'verbose)
