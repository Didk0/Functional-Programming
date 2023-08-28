#lang racket
(require rackunit rackunit/text-ui)

;### Зад 1
; Дали `x` е автоморфно?
; Едно число е автоморфно, ако квадратът му завършва на него.
(define (num-len num)
  (if (< num 10) 1
      (+ 1 (num-len (quotient num 10)))))

(define (sq x) (* x x))

(define (automorphic? x)
      (= x (remainder (sq x) (expt 10 (num-len x)))))

(run-tests
  (test-suite
    "automorphic? tests"
    (check-false (automorphic? 2))
    (check-false (automorphic? 3))
    (check-false (automorphic? 4))
    (check-false (automorphic? 7))
    (check-false (automorphic? 8))
    (check-false (automorphic? 9))
    (check-false (automorphic? 10))

    (check-true (automorphic? 1))
    (check-true (automorphic? 5))
    (check-true (automorphic? 6))
    (check-true (automorphic? 25))
    (check-true (automorphic? 76))
    (check-true (automorphic? 376))
    (check-true (automorphic? 625))
    (check-true (automorphic? 9376)))
  'verbose)
