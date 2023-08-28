(define sample-alist '((a . 3) (b . 4) (c . 5)))

(define (get-keys alist)
  (map car alist))

(define (get-values alist)
  (map cdr alist))

(define (sq x) (* x x))

(define (map-alist f keys)
  (map (lambda (x) (cons x (f x))) keys))

(define graph '((a . (b e))
                (b . (d c))
                (c . (f))
                (d . (c))
                (e . ())
                (f . ())))

(define (edge? v1 v2 g)
  (and (member v2 (cdr (assoc v1 g))) #t))

(define (children v g)
  (cdr (assoc v g)))

(define (out-degree v g)
  (length (children v g)))

(define (in-degree v g)
  (define (count-helper values count)
    (cond ((null? values) count)
          ((member v (car values)) (count-helper (cdr values) (+ 1 count)))
          (else (count-helper (cdr values) count))))
  (count-helper (get-values g) 0))

(define (degree v g)
  (+ (in-degree v g) (out-degree v g)))

(define (parents v g)
  (cond ((null? g) '())
        ((member v (cdr g)) (cons (caar g) (parents v (cdr g))))
        (else (parents v (cdr g)))))

(define (create-stack lst)
  (define (self prop . params)
    (cond ((equal? prop 'empty?) (null? lst))
          ((equal? prop 'top) (car lst))
          ((equal? prop 'push) (append (reverse params) lst))
          ((equal? prop 'pop) (cdr lst))))
    self)

(define (contains? x lst)
  (cond ((null? lst) #f)
        ((equal? x (car lst)) #t)
        (else (contains? x (cdr lst)))))

(define (dfs v g)
  (define (dfs-helper visited stack)
    (if (stack 'empty?) visited
        (let ((top (stack 'top))
              (tail (stack 'pop)))
          (if (contains? top visited) (dfs-helper visited (create-stack tail))
              (dfs-helper (cons top visited) (create-stack (append (children top g) tail)))))))
  (dfs-helper '() (create-stack (list v))))