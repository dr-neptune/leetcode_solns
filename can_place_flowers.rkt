#lang racket
(require racket)

;; idea
;; take sum of array
;; integer division by 2 minus 1
;; if n <= ^ then yes, else no

(define (can-place-flowers flowerbed n)
  (let ([spots-taken (foldl + 0 flowerbed)])
    (<= (- (length flowerbed) (* 2 spots-taken)) n)))

;; there are k slots total
;; and l of them are taken
;; each slot must take 2 spaces
;; so we have (- k (* 2 l)) spaces left
;; if there is a flower as an end cap, it takes 2 slots
;; if we are not at an end cap, it will take 3 slots
(can-place-flowers (list 1 0 0 0 1) 1)
(can-place-flowers (list 1 0 0 0 1) 2)

;; idea
;; check end caps
;; check inner pots
;; (define (can-place-flowers flowerbed n)
;;   (let* ([front-cap (take flowerbed 2)]
;;          [end-cap (take-right flowerbed 2)]
;;          [rest-beds (drop-right (drop flowerbed 2) 2)])
;;     (match front-cap
;;       [(or (list 1 0) (list 0 1))])))


;; dumb way first
;; iterate through flowerbed
;; if a 1 is encountered, go forward 2
;; else add 1
(define (can-place-flowers flowerbed n)
  (let loop ([flowers flowerbed]
             [open-spots 0])
    (if (null? (rest flowers))
        open-spots
        (match (first flowers)
          [1 (loop (rest flowers) open-spots)]
          [_ (loop (rest flowers) (add1 open-spots))]))))

(can-place-flowers (list 1 0 0 0 1) 1)
(can-place-flowers (list 1 0 0 0 1) 2)

;; (1 0 0 0 1)
;; (1 0) -> no -> (0 0 1)
;; (0 0) -> yes, add1 -> (1)
;; (1) -> no -> return 1
(define (can-place-flowers flowerbed n)
  (let loop ([flowers flowerbed]
             [open-spots 0])
    (if (or (null? flowers)
            (null? (rest flowers)))
        (>= open-spots n)
        (match (take flowers 2)
          [(list 0 0) (loop (rest (rest flowers)) (add1 open-spots))]
          [_ (loop (rest (rest flowers)) open-spots)]))))

;; not done yet
