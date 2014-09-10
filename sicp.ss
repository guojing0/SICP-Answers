;; Some useful functions I used in exercises:

(define square (lambda (x) (* x x)))
(define cube (lambda (x) (* x x x)))

(define double (lambda (x) (* 2 x)))
(define halve (lambda (x) (/ x 2)))

(define id (lambda (x) x))
(define inc2 (lambda (x) (+ x 2)))

(set! dx 0.00001)
(set! tolerance 0.000001)

(define deriv ; Compute the derivate of function g.
  (lambda (g)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x))
	 dx))))

(define newton-transform
  (lambda (g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x))))))

(define newtons-method
  (lambda (g guess)
    (fixed-point (newton-transform g) guess)))

;; 1.1

10 ; => 10

(+ 5 3 4) ; => 12

(- 9 1) ; => 8

(/ 6 2) ; 3

(+ (* 2 4) (- 4 6)) ; => 6

(define a 3) ; => nil

(define b (+ a 1)) ; => nil

(+ a b (* a b)) ; => 19

(= a b) ; => #f

(if (and (> b a) (< b (* a b)))
    b
    a) ; => 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; => 16

(+ 2 (if (> b a) b a)) ; => 6

(* (cond ((> a b) a)
   ((< a b) b)
   (else -1))
   (+ a 1)) ; => 16

;; 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))) ; => -37/150

;; 1.3

(define sum-of-squares
  (lambda (x y z)
    (- (+ (square x) (square y) (square z))
       (square (min x y z)))))

;; 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)) ; As it is.

;; 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

; If Ben observes an interpreter using applicative-order evaluation, it crashes, otherwise returns 0.

;; 1.6 pass

;; 1.7 wait

;; 1.8

(define cube-root
  (lambda (x)
    (try 1.0 x)))

(define improve
  (lambda (guess x)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3)))

(define try
  (lambda (guess x)
    (if (good-enough? guess x)
	guess
	(try (improve guess x) x))))

(define good-enough?
  (lambda (guess x)
    (< (abs (- (cube guess) x))
       0.001)))

;; 1.11

; Recursive process

(define f0
  (lambda (n)
    (cond ((< n 3) n)
	  (else (+ (f0 (- n 1))
		   (* 2 (f0 (- n 2)))
		   (* 3 (f0 (- n 3))))))))

; Iterative process

(define f1
  (lambda (n)
    (letrec ((f1-helper (lambda (a b c i input)
			  (if (= i input)
			      c
			      (f1-helper (+ a (* 2 b) (* 3 c))
					 a
					 b
					 (+ 1 i)
					 input)))))
      (f1-helper 2 1 0 0 n))))

;; 1.12

; Recursive process

(define pascal-rec
  (lambda (row col)
    (cond ((< row col) (error "error" "row must bigger than col")) 
	  ((or (= col 1)
	       (= row col)) 1)
	  (else (+ (pascal-rec (- row 1) (- col 1))
		   (pascal-rec (- row 1) col))))))

;; 1.16

(define faster-expt
  (lambda (b n)
    (letrec ((expt-helper (lambda (a b n)
			    (cond ((zero? n) a)
				  ((even? n) (square (expt-helper a b (/ n 2))))
				  ((odd? n) (expt-helper (* a b) b (- n 1)))))))
      (expt-helper 1 b n))))

;; 1.17

(define mult0
  (lambda (a b)
    (cond ((zero? b) 0)
	  ((even? b) (double (mult a (halve b))))
	  ((odd? b) (+ a (mult a (- b 1)))))))

;; 1.18

(define mult1
  (lambda (a b)
    (letrec ((mult-helper (lambda (x y product)
			    (cond ((zero? y) product)
				  ((even? y) (mult-helper
					      (double x)
					      (halve y)
					      product))
				  ((odd? y) (mult-helper
					     x
					     (- y 1)
					     (+ x product)))))))
      (mult-helper a b 0))))

;; 1.29 TO-DO

(define get-h
  (lambda (a b n)
    (/ (- b a) n)))

(define get-yk
  (lambda (func a b n k)
    (func (+ a (* k (get-h a b n))))))

; (define sum-helper
;  (lambda (func a b n k)
;    (cond ((zero? k) (get-yk func a b n 0))
;	  ((odd? k) (* 4 []))
;	  ((even? k) ([])))))

; (define simpson
;  (lambda (func a b n)
;    (/ (* (get-h a b n) (sum-helper [])) 3)))

;; 1.30

(define sum
  (lambda (term a next b)
    (letrec ((iter (lambda (a result)
		     (if (> a b)
			 result
			 (iter (next a) (+ (term a) result))))))
      (iter a 0))))

;; 1.31

; Recursive process - product

(define product-rec
  (lambda (term a next b)
    (if (= a b)
	a
	(* (term a) (product-rec term (next a) next b)))))

; Iterative process - product

(define product-iter
  (lambda (term a next b)
    (letrec ((iter (lambda (a result)
		     (if (> a b)
			 result
			 (iter (next a) (* (term a) result))))))
      (iter a 1))))

; Define factorial with product defined before

(define fac
  (lambda (n)
    (product-iter (lambda (x) x)
		  1
		  (lambda (y) (+ y 1))
		  n)))

; Compute approximations to pi

(define compute-pi
  (lambda (n)
     (* 4
	(exact->inexact
	 (/ (* 2 n (square (product-iter id
					 4
					 inc2
					 (1- n))))
	    (square (product-iter id
				  1
				  inc2
				  n)))))))

;; 1.32

; Recursive process - accumulate

(define accumulate-rec
  (lambda (combiner null-value term a next b)
    (if (> a b)
	null-value
	(combiner (term a)
		  (accumulate-rec combiner
				  null-value
				  term
				  (next a)
				  next b)))))

; Iterative process - accumulate

(define accumulate-iter
  (lambda (combiner null-value term a next b)
    (letrec ((helper (lambda (a result)
		       (if (> a b)
			   result
			   (helper (next a) (combiner result (term a)))))))
      (helper a null-value))))

; Define new sum and product with accumulate function defined before

(define sum-with-acc
  (lambda (term a next b)
    (accumulate-iter + 0 term a next b)))

(define product-with-acc
  (lambda (term a next b)
    (accumulate-iter * 1 term a next b)))

;; 1.33 TO-DO

;; 1.35

; Fixed-point function

(define fixed-point
  (lambda (f first-guess)
    (letrec ((close-enough? (lambda (v1 v2)
			      (< (abs (- v1 v2)) tolerance)))
	     (try (lambda (guess)
		    (let ((next (f guess)))
                        (if (close-enough? guess next)
                            next
                            (try next))))))
      (try first-guess))))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) ; Output: 1.6180327868852458

;; 1.36

(define display-steps
  (lambda (x)
    (display x)
    (newline)))

(define fixed-point-with-display-steps
  (lambda (f first-guess)
    (letrec ((close-enough? (lambda (v1 v2)
                              (< (abs (- v1 v2)) tolerance)))
	     (try (lambda (guess)
		    (display-tests guess)
                    (let ((next (f guess)))
		      (if (close-enough? guess next)
			  next
                            (try next))))))
      (try first-guess))))

;; 1.37 TO-DO
;; 1.38 TO-DO
;; 1.39 TO-DO

;; 1.40

(define cubic
  (lambda (a b c)
    (lambda (x)
      (+ (cube x) (* a (square x)) (* b x) c))))

;; 1.41

(define double-procedure
  (lambda (f)
    (lambda (x)
      (f (f x)))))

;; 1.42

(define compose
  (lambda (func0 func1)
    (lambda (x)
      (func0 (func1 x)))))

;; 1.43

(define repeated
  (lambda (f n)
    (if (= n 1)
	f
	(compose f (repeated f (1- n))))))