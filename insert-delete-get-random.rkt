#lang racket
(require racket
         (only-in racket/random random-ref))

(define randomized-set%
  (class object%
    (super-new)
    (init-field [rset (make-hash)])

    ; insert : exact-integer? -> boolean?
    (define/public (insert val)
      ;; (displayln (format "~a ~a" val rset))
      (let ([found (hash-ref rset val #f)])
        (if (false? found)
            (begin
              (hash-set! rset val 1)
              #t)
            #f)))

    ; remove : exact-integer? -> boolean?
    (define/public (remove val)
      ;; (displayln (format "~a ~a" val rset))
      (let ([found (hash-ref rset val #f)])
        (if (false? found)
            #f
            (begin
              (hash-remove! rset val)
              #t))))

    ; get-random : -> exact-integer?
    (define/public (get-random)
      (let ([hk (hash-keys rset)]
            [rv (random (hash-count rset))])
        (list-ref hk rv)))))


;; do it with sets
(define randomized-set%
  (class object%
    (super-new)
    (init-field [rset (weak-set)])

    ; insert : exact-integer? -> boolean?
    (define/public (insert val)
      ;; (displayln (format "~a ~a" val rset))

      (let ([found (set-member? rset val)])
        (if (false? found)
            (begin
              (set-add! rset val)
              #t)
            #f)))

    ; remove : exact-integer? -> boolean?
    (define/public (remove val)
      ;; (displayln (format "~a ~a" val rset))
      (let ([found (set-member? rset val)])
        (if (false? found)
            #f
            (begin
              (set-remove! rset val)
              #t))))

    ; get-random : -> exact-integer?
    (define/public (get-random)
      (let ([hk (set->vector rset)]  ;; this is still not great :/, O(n) operation
            [rv (random (set-count rset))])
        (list-ref hk rv) ;; here as well
        ))))


;; with sets and a helper function
(define randomized-set%
  (class object%
    (super-new)
    (init-field [rset (weak-set)])

    (define/private (crud fn val [found #f] [not-found #t])
      (let ([found (set-member? rset val)])
        (if (false? found)
            (begin
              (fn rset val)
              not-found)
            found)))

    ; insert : exact-integer? -> boolean?
    (define/public (insert val)
      ;; (displayln (format "~a ~a" val rset))
      (crud (λ (set val) (set-add! set val)) val)
      ;; (let ([found (set-member? rset val)])
      ;;   (if (false? found)
      ;;       (begin
      ;;         (set-add! rset val)
      ;;         #t)
      ;;       #f))
      )

    ; remove : exact-integer? -> boolean?
    (define/public (remove val)
      ;; (displayln (format "~a ~a" val rset))
      (let ([found (set-member? rset val)])
        (if (false? found)
            #f
            (begin
              (set-remove! rset val)
              #t))))

    ; get-random : -> exact-integer?
    (define/public (get-random)
      (let ([hk (set->list rset)]  ;; this is still not great :/, O(n) operation
            [rv (random (set-count rset))])
        (list-ref hk rv) ;; here as well
        ))))


;; (let ([ephsh (make-ephemeron-hash)])
;;   (displayln ephsh)
;;   (hash-set! ephsh 2 3)
;;   (displayln ephsh)
;;   (hash-set! ephsh 3 4)
;;   (displayln ephsh)
;;   (hash-ref ephsh 2))

;; (curry-spots hash-set! hsh _ 1)

;; Your randomized-set% object will be instantiated and called as such:
;; (define obj (new randomized-set%))
;; (define val 10)
;; (define param_1 (send obj insert val))
;; (define param_2 (send obj remove val))
;; (define param_3 (send obj get-random))

(define obj (new randomized-set%))
(send obj insert 1)
(send obj remove 2)
(send obj insert 2)
(send obj get-random)
(send obj remove 1)
(send obj insert 2)
(send obj get-random)


;; ["RandomizedSet","insert","remove","insert","getRandom","remove","insert","getRandom"]
;; [[],[1],[2],[2],[],[1],[2],[]]

;; [null,true,false,true,1,true,false,2]
