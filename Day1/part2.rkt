#lang racket

;; Find the first frequency repeated when repeatedly applying the given deltas.
(define (find-first-frequency-repeat deltas)
  ;; newest frequencies stored first
  (define (helper frequencies)
    ;; are there any duplicates in our list of accumulated frequencies?
    (or (check-duplicates (reverse frequencies))
        ;; apply the given deltas to our frequencies
        (helper (foldl (λ (delta deltas)
                                 (cons (+ delta (first deltas)) deltas))
                               frequencies
                               deltas))))
  (helper '(0)))

(find-first-frequency-repeat (file->list "input.txt"))
