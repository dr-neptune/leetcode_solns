#lang racket
(require racket)


(define exwords '("This" "is" "an" "example" "of" "text" "justification."))
(define exwidth 16)

#|

idea

1. first we want to partition words into groups of <= width
2. then we want to distribute padding interior to the words
such that the total width == width

|#

;; 1
;; (for/fold ([chars-left exwidth]
;;            [strings '()]
;;            #:result (reverse strings))
;;           ([sl (map (compose add1 string-length) exwords)]
;;            #:break (>= sl chars-left))
;;   (values (- chars-left sl) (cons sl strings)))


(define (get-width-subset strings [width exwidth])
  (let ([len-pairs (map cons strings (map string-length strings))])
    (for/fold ([chars-left width]
               [strings '()]
               #:result (reverse strings))
              ([sl len-pairs]
               #:break (>= (+ (cdr sl) (length strings)) chars-left))
      (values (- chars-left (cdr sl))
              (cons sl strings)))))

;; (get-width-subset exwords)

;; now we want to consume the list with the above algorithm
;; (let loop ([words exwords]
;;            [lines '()])
;;   (let ([subset (get-width-subset words)])
;;     ;; (displayln (format "~a ~a ~a" words lines subset))
;;     (if (not (empty? words))
;;       (loop (drop words (length subset))
;;             (cons subset lines))
;;       lines)))

(define (partition-words words width)
  (let loop ([words words]
             [lines '()])
    (let ([subset (get-width-subset words width)])
      (if (not (empty? words))
          (loop (drop words (length subset))
                (cons subset lines))
          lines))))

(define inter (partition-words exwords exwidth))

;; 2. then we want to distribute padding interior to the words
;; such that the total width == width

;; first we should find the single gapped width of each sublist
;; then we should subtract that value from the needed width to get
;; the number of gaps to distribute
(map (λ (v) (- 16 v))
     (map (λ (ls) (+ (foldl + 0 ls)
                     (sub1 (length ls))))
          (map (curry map cdr) (reverse inter))))


;; once we have gaps, we must decide how to allocate them
;; greedily allocate to the left
;; maybe we can use a counter and allocate left then right then left, etc

;; perhaps we can start with just number of regular spaces
;; then tag that as 1
;; and add (quotient num-allocatable-spaces num-spaces) to each
;; then add 1 to each of the remainder spaces

;; (define-syntax (values->list values)
;;   (let-values))

(let ([num-spaces (map (compose (λ (v) (if (zero? v) 1 v)) sub1 length) inter)])
  (map (compose values->list quotient/remainder) (map (λ (v) (- 16 v))
              (map (λ (ls) (+ (foldl + 0 ls)
                              (sub1 (length ls))))
                   (map (curry map cdr) (reverse inter))))
       (reverse num-spaces)))

;; we can
