#lang racket
(require racket)

#|

idea

for each i, j

check row / column and see if there are any black pixels
if not, add1 to a counter

|#

(λ (x y)
  (let ([horizontal (list-ref f-words x)]
        [vertical (map (curryr list-ref y) f-words)])
    (equal? horizontal vertical)))

(define (get-horiz-vert x y mat)
  (let ([horizontal (list-ref mat x)]
        [vertical (map (curryr list-ref y) mat)])
    (list horizontal vertical)))

(define exmat '(("W" "W" "B")
                ("W" "B" "W")
                ("B" "W" "W")))

(define exmat (list->vector (map list->vector exmat)))

(define (get-horiz-vert mat x y)
  (let ([horizontal (vector-ref mat x)]
        [vertical (vector-map (curryr vector-ref y) mat)])
    (map vector->list (list horizontal vertical))))

(get-horiz-vert 1 1 exmat)

(define (vector-mat-ref vec x y)
  (vector-ref (vector-ref vec y) x))

(for*/fold ([num-lonely 0])
           ([i (in-range (vector-length exmat))]
            [j (in-range (vector-length exmat))]
            #:when (equal? (vector-mat-ref exmat i j) "B"))
  (match-let ([(list horz vert) (get-horiz-vert exmat i j)])
    (displayln (format "~a ~a ~a" num-lonely horz vert))
    (if (and (false? (member "B" (remove "B" horz)))
             (false? (member "B" (remove "B" vert))))
        (values (add1 num-lonely))
        num-lonely)))

(define pic (list->vector (map list->vector picture)))

(define (get-horiz-vert mat x y)
  (let ([horizontal (vector-ref mat x)]
        [vertical (vector-map (curryr vector-ref y) mat)])
    (map vector->list (list horizontal vertical))))

(define (vector-mat-ref vec x y)
  (vector-ref (vector-ref vec y) x))

(define (find-lonely-pixel picture)
  (let ([pic (list->vector (map list->vector picture))])
    (for*/fold ([num-lonely 0]
                #:result num-lonely)
               ([i (in-range (vector-length pic))]
                [j (in-range (vector-length pic))]
                #:when (equal? (vector-mat-ref pic i j) #\B))
      (match-let ([(list horz vert) (get-horiz-vert pic i j)])
        (if (and (equal? 1 (length (indexes-of horz #\B)))
                 (equal? 1 (length (indexes-of vert #\B))))
            (values (add1 num-lonely))
            num-lonely)))))

(find-lonely-pixel '((#\W #\W #\B) (#\W #\B #\W) (#\B #\W #\W)))
(find-lonely-pixel exmat)

(find-lonely-pixel
 '((#\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W #\W)
   (#\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\B #\W #\W #\W)))
