
#lang racket
(require rackunit)

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (if (>= high low)
      (cons low (sequence (+ low stride) high stride))
      null))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(eq? empty xs) (error "list-nth-mod: empty list")]
        [#t (letrec ([rem (remainder n (length xs))]
                     [f (lambda (lst i) (if (= i rem)
                                            (car lst)
                                            (f (list-tail lst 1) (+ 1 i))))])
              (f xs 0))]))

(define (stream-for-n-steps s n)
  (letrec ([helper (lambda (str i) (if (= i 0)
                                       null
                                       (let ([pr (str)]) (cons (car pr) (helper (cdr pr) (- i 1))))))]) (helper s n)))

(define funny-number-stream
  (letrec ([hl (lambda (x) (cons (if (= (remainder x 5) 0)
                                     (- 0 x)
                                     x) (lambda () (hl (+ x 1)))))])
    (lambda () (hl 1))))

(define dan-then-dog
  (letrec ([hl (lambda (b) (cons (if b
                                     "dan.jpg"
                                     "dog.jpg") (lambda () (hl (not b)))))])
    (lambda () (hl #t))))

(define (stream-add-zero s)
  (letrec ([hl (lambda (st) (let ([pr (st)]) (cons (cons 0 (car pr)) (lambda () (hl (cdr pr))))))])
    (lambda () (hl s))))

(define (cycle-lists xs ys)
  (letrec ([hl (lambda (xss yss) (let* ([new-cycle-x (null? xss)]
                                        [new-cycle-y (null? yss)]
                                        [x (if new-cycle-x
                                               (car xs)
                                               (car xss))]                                    
                                        [y (if new-cycle-y
                                               (car ys)
                                               (car yss))]) (cons (cons x y) (lambda () (hl (if new-cycle-x
                                                                                                (cdr xs)
                                                                                                (cdr xss)) (if new-cycle-y
                                                                                                               (cdr ys)
                                                                                                               (cdr yss)))))))])
    (lambda () (hl xs ys))))

(define (vector-assoc v vec)
  (letrec ([v-lth (vector-length vec)]
         [hl (lambda (n) (cond [(= n v-lth) #f]
                               [(not (pair? (vector-ref vec n))) (hl (+ 1 n))]
                               [#t (if (equal? v (car (vector-ref vec n)))
                                       (vector-ref vec n)
                                       (hl (+ 1 n)))]))])
    (hl 0)))


