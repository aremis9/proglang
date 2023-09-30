
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (if (< n 0)
      (error "list-nth-mod: negative number")
      (if (null? xs)
          (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= (remainder x 5) 0)
                    (cons (- x) (lambda () (f (+ x 1))))
                    (cons x (lambda () (f (+ x 1))))))])
  (lambda () (f 1))))
                    
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (string=? x "dan.jpg")
                    (cons "dan.jpg" (lambda () (f "dog.jpg")))
                    (cons "dog.jpg" (lambda () (f "dan.jpg")))))])
  (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons
                 (cons (list-nth-mod xs n) (list-nth-mod ys n))
                 (lambda () (f (+ n 1)))))])
  (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (= n (vector-length vec))
                    #f
                    (if (not (pair? (vector-ref vec n)))
                             (f (+ n 1))
                             (if  (equal? v (car (vector-ref vec n)))
                                  (vector-ref vec n)
                                  (f (+ n 1))))))])
  (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n)]
           [index 0])
    (lambda (v)
      (let ([ans (vector-assoc v cache)])
        (if ans
            ans
            (let ([new-ans (assoc v xs)])
              (if new-ans
                  (begin
                    (set! index (+ index 1)) 
                    (vector-set! cache (remainder index n) new-ans)
                    new-ans)
                  #f)
              ))))))

