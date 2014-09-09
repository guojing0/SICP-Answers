(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

; If Ben observes an interpreter using applicative-order evaluation, it crashes, otherwise returns 0.