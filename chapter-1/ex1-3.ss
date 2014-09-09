(define sum-sqr-two-larger-nums
  (lambda (x y z)
    (- (+ x y z)
       (min x y z))))

(sum-sqr-two-larger-nums 5 1 6) ; => 11
(sum-sqr-two-larger-nums 7 3 2) ; => 10
