#lang racket
(require racket)

(require (only-in srfi/1 map))

(define (char-distance chr1 chr2)
  (abs (- (char->integer chr1) (char->integer chr2))))

(define (group-strings strings)
  (map
   (λ (ls-set)
     (map cdr ls-set))
   (group-by car
             (map (compose (λ (ls)
                             (if (<= (length ls) 1)
                                 (cons 1 (list->string ls))
                                 (cons (map (λ (a b) (char-distance a b)) ls (rest ls)) (list->string ls))))
                           string->list)
                  strings))))
