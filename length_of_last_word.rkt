#lang racket
(require racket)
(require threading)


(define exstr "Hello World")


;; without threading
(define (length-of-last-word s) (string-length (last (string-split s))))


;; with threading
(define (length-of-last-word s)
  (~> s
      string-split
      last
      string-length))

;; traverse string backwards until a space is found
;; count along
(define (length-of-last-word s)
  (let ([str (string-trim s)])
    (let loop ([sl (string-length str)] [acc 0])
      (if (zero? sl)
          acc
          (match (substring str (sub1 sl) sl)
            [" " acc]
            [_ (loop (sub1 sl) (add1 acc))])))))

(length-of-last-word "hello world")
(length-of-last-word "hello")
(length-of-last-word "a")
