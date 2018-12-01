#lang racket

; calculate-frequencies : (Listof Number) Number -> (Listof Number)
; Calculates the resulting frequencies after applying the given deltas to the
; given start frequency. The most recent frequency appears first in the
; resulting list.
(define (calculate-frequencies deltas start)
  (define (helper deltas current previous)
    (let ([new (cons current previous)])
      (if (empty? deltas)
        new
        (helper (rest deltas)
                (+ (first deltas) current)
                new))))
  (helper deltas start '()))

(define (find-first-change-repeat deltas)
  (define (helper previous)
    (let ([first-dup (check-duplicates (reverse previous))])
      (or first-dup
          (helper (append (calculate-frequencies deltas (first previous))
                          (rest previous))))))
  (helper '(0)))

(find-first-change-repeat (file->list "input.txt"))
