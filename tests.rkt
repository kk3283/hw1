#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "scheme.rkt")

(provide scheme-tests)

(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define integers
  (integers-from 0))

;; Map a function over a list with left-to-right order of evaluation guaranteed
(define (map-ltor f xs)
  (if (null? xs)
      null
      (let [(y (f (first xs)))]
        (cons y (map-ltor f (rest xs))))))

(define scheme-tests
  (test-suite
   "Scheme Homework Tests"
   
   (test-suite
    "sigma"
    (test-case "(sigma 0 0)"
               (check-equal? (sigma 0 0)
                             0))
    (test-case "(sigma 0 5)"
               (check-equal? (sigma 0 5)
                             15))
    (test-case "(sigma 162 10056)"
               (check-equal? (sigma 162 10056)
                             50553555)))

   (test-suite
    "log"
    (test-case "(log 33 1)"
               (check-equal? (log 33 1)
                             0))
    (test-case "(log 5 100)"
               (check-equal? (log 5 100)
                             2))
    (test-case "(log 3 4782968)"
               (check-equal? (log 3 4782968)
                             13)))

   (test-suite
    "choose"
    (test-case "(choose 55 1)"
               (check-equal? (choose 55 1)
                             55))
    (test-case "(choose 303 303)"
               (check-equal? (choose 303 303)
                             1))
    (test-case "(choose 13 4)"
               (check-equal? (choose 13 4)
                             715)))

   (test-suite
    "binary"
    (test-case "(binary 0)"
               (check-equal? (binary 0)
                             0))
    (test-case "(binary 1)"
               (check-equal? (binary 1)
                             1))
    (test-case "(binary 2)"
               (check-equal? (binary 2)
                             10))
    (test-case "(binary 13)"
               (check-equal? (binary 13)
                             1101))
    (test-case "(binary 7134)"
               (check-equal? (binary 7134)
                             1101111011110)))

   (test-suite
    "scan"
    (test-case "scan base case"
               (check-equal? (scan + 0 '())
                             '(0)))
    (test-case "scan inductive case"
               (check-equal? (scan + 0 '(1 2 3 4 5 6))
                             '(0 1 3 6 10 15 21))))

   (test-suite
    "stream-scan"
    
    (test-case
     "stream-scan base case"
     (check-equal? (stream->list (stream-scan + 0 empty-stream))
                   '(0)))
    
    (test-case
     "stream-scan inductive case"
     (check-equal? (stream->list (stream-scan + 0 (stream 1 2 3 4 5 6)))
                   '(0 1 3 6 10 15 21))))

   (test-suite
    "stream-take-n"
    
    (test-case
     "stream-take-n base case with empty stream"
     (check-equal? (stream-take-n 0 empty-stream)
                   '()))
    
    (test-case
     "stream-take-n base case"
     (check-equal? (stream-take-n 0 integers)
                   '()))
    
    (test-case
     "(stream-take-n 10 integers)"
     (check-equal? (stream-take-n 10 integers)
                   '(0 1 2 3 4 5 6 7 8 9))))
   
   (test-suite
    "stream-pair-with"

    (test-case
     "(stream-pair-with (lambda (x) (+ x 1)) (1 2 3 4))"
     (check-equal? (stream->list (stream-pair-with (lambda (x) (+ x 1)) (stream 1 2 3 4)))
                   '((1 . 2) (2 . 3) (3 . 4) (4 . 5)))))
   
   (test-suite
    "cycles-streams"

    (test-case
     "(stream-take-n 8 (cycles-streams (stream 1 2 3) (stream \"a\" \"b\")))"
     (check-equal? (stream-take-n 8 (cycle-streams '(1 2 3) '("a" "b")))
                   '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b") (1 . "a") (2 . "b")))))

   (test-suite
    "seen"
    
    (test-case
     "(seen #f) (seen #f)"
     (check-equal? (map-ltor seen '(#f #f))
                   '(#f #t)))
    
    (test-case
     "(seen null) (seen null)"
     (check-equal? (map-ltor seen '(null null))
                   '(#f #t)))
    
    (test-case
     "(seen 1)"
     (check-equal? (seen 1)
                   #f))
    
    (test-case
     "(seen 2) (seen 2)"
     (check-equal? (map-ltor seen '(2 2))
                   '(#f #t)))
    
    (test-case
     "(seen 3) (seen 4)"
     (check-equal? (map-ltor seen '(3 4))
                   '(#f #f)))
    
    (test-case
     "(seen 10) (seen 11) (seen 12) (seen 13) (seen 14) (seen 15) (seen 16) (seen 17) (seen 10)"
     (check-equal? (map-ltor seen '(10 11 12 13 14 15 16 17 10))
                   '(#f #f #f #f #f #f #f #f #t)))
    
    (test-case
     "(begin (seen 'a) (seen 'b) (seen 'c) (seen 'd) (seen 'e) (seen 'a))"
     (check-equal? (map-ltor seen '(a b c d e a))
                   '(#f #f #f #f #f #t))))))
