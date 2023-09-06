#lang racket
(require racket)

;; idea
;; stack
;; add an element to the stack
;; if the top of the stack is
;;    opposite sign, going left and the element is going right,
;;        then append since they don't collide
;;    opposite sign, larger then discard current val
;;    opposite sign, smaller, discard top of stack and add current val
;;    opposite sign, same size, discard top of stack and current val
;;    same sign, add to the stack

(define exls '(5 10 -5))
(define exls '(10 2 -5))

(for/fold ([asteroids '()]
           #:result (reverse asteroids))
          ([current exls])
  ;; (displayln (format "~a ~a" current asteroids))
  (match (list current asteroids)
    [(list a '()) (values (cons a asteroids))]
    [(list a b)
     (let ([top-asteroid (first b)])
       (match (eq? (sgn a) (sgn top-asteroid))
         [#t (values (cons a asteroids))]  ;; same sign
         ;; opposite sign
         [_ (cond [(eq? (abs a) (abs top-asteroid))
                   (values (rest asteroids))]
                  [(> (abs a) (abs top-asteroid))
                   (values (cons a (rest asteroids)))]
                  [else (values asteroids)])]))]))

;; ah, it needs to keep going!
;; what if we apply the function until we hit a fixed point?

(define (compress-asteroids asls)
  (for/fold ([asteroids '()]
             #:result (reverse asteroids))
            ([current asls])
    (displayln (format "~a ~a" current asteroids))
    (match (list current asteroids)
      [(list a '()) (values (cons a asteroids))]
      [(list a b)
       (let ([top-asteroid (first b)])
         (match (eq? (sgn a) (sgn top-asteroid))
           [#t (values (cons a asteroids))]  ;; same sign
           ;; opposite sign
           [_ (cond [(eq? (abs a) (abs top-asteroid))
                     (values (rest asteroids))]
                    [(> (abs a) (abs top-asteroid))
                     (values (cons a (rest asteroids)))]
                    [else (values asteroids)])]))])))

(define (asteroid-collision asteroids)
  (let loop ([iter (compress-asteroids asteroids)])
    (displayln (format "iter: ~a" iter))
    (if (equal? iter (compress-asteroids iter))
        iter
        (loop (compress-asteroids iter)))))


(asteroid-collision exls)
(asteroid-collision '(10 2 -5))
(asteroid-collision '(8 -8))

(asteroid-collision '(-2 -1 1 2))

;; ah, -2 -1 are moving left, 1 2 are moving right
;; so -2 -1 will never collide, 1 2 will never collide
;; to adjust
;; if we have opposite sign
;; and a is - and b is +, then append

(define (compress-asteroids asls)
  (for/fold ([asteroids '()]
             #:result (reverse asteroids))
            ([current asls])
    (match (list current asteroids)
      [(list a '()) (values (cons a asteroids))]
      [(list a b)
       (let ([top-asteroid (first b)])
         (match (eq? (sgn a) (sgn top-asteroid))
           ;; same sign
           [#t (values (cons a asteroids))]
           ;; opposite sign
           [_ (match (sgn a)
                ;; if a is going right and prev asteroid is going left
                ;; then they won't collide
                [1 (values (cons a asteroids))]
                [_ (cond [(eq? (abs a) (abs top-asteroid))
                          (values (rest asteroids))]
                         [(> (abs a) (abs top-asteroid))
                          (values (cons a (rest asteroids)))]
                         [else (values asteroids)])])]))])))

(define (asteroid-collision asteroids)
  (let loop ([iter (compress-asteroids asteroids)])
    (displayln (format "iter: ~a" iter))
    (if (equal? iter (compress-asteroids iter))
        iter
        (loop (compress-asteroids iter)))))
