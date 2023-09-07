#lang racket
(require racket)

;; idea
;; multi-round queue?
(define exstr "RD")
(define exstr2 "RDD")

;; to check if victory can be announced
(define (all-equal? ls)
  (match ls
    ['() #f]
    [_ (let ([val (car ls)])
         (andmap (λ (a) (equal? a val)) ls))]))

;; given current
;; if next is not the same
;; then drop it and move on
;; at the end of each round, check if victory can be announced

(require racket/match)

(for/fold ([senate '()])
          ([senator (string->list exstr)])
  (displayln (format "~a ~a" senator senate))
  (match (list senator senate)
    [(list a '()) (values (list a))]
    [(? (λ (ls) (eq? (first ls) (first (rest ls)))))
     (values (cons senator senate))]
    [_ (values (cons senator (rest senate)))]))

;; logic is wrong
;; if a and b are different, we want to skip b and append a
;; we are currently appending a and dropping someone already there

;; if queue is empty, append the first element
;; if the next element is different, then skip it
;; if it is the same then append it



;; now loop until all-equal
(define (senate-round senators-ls)
  (for/fold ([senate (list (first senators-ls))])
            ([senator (rest senators-ls)])
    (displayln (format "~a ~a" senate senator))
    (match (list senator (first senate))
      [(? all-equal?) (values (cons senator senate))]
      [_ (values (cons senator (rest senate)))])))

(let loop ([senators (senate-round (string->list exstr2))])
  (if (all-equal? senators)
      (match (first senators)
        [#\D "Dire"]
        [#\R "Radiant"])
      (loop (senate-round senators))))


(define (predict-party-victory senate)
  (define (senate-round senators-ls)
    (for/fold ([senate (list (first senators-ls))])
              ([senator (rest senators-ls)])
      (displayln (format "~a ~a" senate senator))
      (match (list senator (first senate))
        [(? all-equal?) (values (cons senator senate))]
        [_ (values (cons senator (rest senate)))])))
  (let loop ([senators (senate-round (string->list senate))])
    (if (all-equal? senators)
        (match (first senators)
          [#\D "Dire"]
          [#\R "Radiant"])
        (loop (senate-round senators)))))

(predict-party-victory exstr)


;; R (R D D)
;; D (R D)
;; D (D)

;; we need to traverse the queue
;; and for the last member, give the check against the first

(let loop ([senate (string->list exstr2)]
           [decisions '()])
  (match senate
    [(list a) a]
    [(list a b ..1) (loop (cons a (rest senate)))]
    [(list )]))

;; (r d d)
;; r goes, bans d
;; (r d)
;; d goes, bans r
;; (d)
;; d wins

;; (r d)
;; r goes, bans d
;; (r)
;; r wins

(let loop ([senators (string->list exstr2)]
           [survivors '()])
  (match (list (first senators) (rest senators))
    [(list '() (list a ...)) survivors]
    [(list a (list a b ...)) (loop (rest senators) (cons a survivors))]
    [(or (list a (list b a ...))
         (list a (list b ..1)))
     (loop (rest (rest senators)) (cons a survivors))]
    [(list a '()) (loop (cons a survivors) '())]))

;; idea
;; traverse the list
;; if R, use remove for D
;; if D, use remove for R
;; if remove doesn't do anything, return the value

(let loop ([senators (string->list exstr)]
           [recent '()])
  (displayln (format "~a" senators))
  (match senators
    ['() recent]
    [(list a) a]
    [(list #\R ..1 #\D ..1) (loop (remove #\D (rest senators)) #\R)]
    [(list #\D ..1 #\R ..1) (loop (remove #\R (rest senators)) #\D)]))

(define (all-equal? ls)
  (match ls
    ['() #f]
    [_ (let ([val (car ls)])
         (andmap (λ (a) (equal? a val)) ls))]))

(define (predict-party-victory senate)
  (define (match-result result)
    (match result
      [#\D "Dire"]
      [#\R "Radiant"]))
  (let loop ([senators (string->list senate)]
             [recent '()])
    (displayln (format "~a ~a" senators recent))
    (if (all-equal? senators)
        (match-result (first senators))
        (match senators
          ['() (match-result recent)]
          [(list a) (match-result a)]
          [(list #\R ..1 #\D ..1 _ ...) (loop (remove #\D (rest senators)) #\R)]
          [(list #\D ..1 #\R ..1 _ ...) (loop (remove #\R (rest senators)) #\D)]))))

;; not working
;; try (R R R)

(predict-party-victory "RRR")
(predict-party-victory "RDR")
(predict-party-victory "DDRRR")
(predict-party-victory exstr)
(predict-party-victory exstr2)
