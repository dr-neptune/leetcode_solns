#lang racket
(require racket)

;; need to add an attribute that we can append to

(define recent-counter%
  (class object%
    (super-new)

    (init-field
     [requests '()]
     [time-range '()])

    ; ping : exact-integer? -> exact-integer?
    (define/public (ping t)
      (set! time-range (list (- t 3000) t))
      (set! requests
            (dropf-right (cons t requests) (λ (r) (not (and (>= r (first time-range))
                                                            (<= r (last time-range)))))))
      (length requests))))


(define obj (new recent-counter%))
(send obj ping 1)
(send obj ping 100)
(send obj ping 3001)
(send obj ping 3002)
