#lang racket
(require racket)

(define exgrid '((2 1 1)
                 (1 1 0)
                 (0 1 1)))

;; idea
;; check if anything isn't 4 directionally linked to another
;; if so, return -1
;; otherwise for each rotten orange, rot each directionally linked non-rotten orange
;; then check if all oranges are rotten
;; if so, return num-minutes
;; if not, increment num-minutes

;; better yet
;; start by checking globally
;; if no unlinked oranges then
;; scan for a rotten orange
;; start at a rotten orange

(define (grid->vec grid)
  (vector-map list->vector (list->vector grid)))

(define (get-element grid x y)
  (cond [(or (negative? x)
             (negative? y)
             (> x (sub1 (vector-length (vector-ref grid 0))))
             (> y (sub1 (vector-length grid)))) -1]
        [else
         (let ([y-row (vector-ref grid y)])
           (vector-ref y-row x))]))

(let ([grid exgrid])
  (get-element (grid->vec grid) 0 0))

;; now we want to get all adjacent elements
(define (get-adjacent grid x y)
  (filter (compose not negative?)
          (map (curry get-element grid)
               `((,(add1 x) y)
                 (,(sub1 x) y)
                 (x ,(add1 y))
                 (x ,(sub1 y))))))

;; rework this to go clockwise starting from midnight
;; or use an alist to tag them
(let ([x 0] [y 0])
  (filter (compose not negative? cadr)
          (map (λ (ls dir) (list dir (apply (curry get-element (grid->vec exgrid)) ls)))
               (list
                (list x (sub1 y))  ;; N
                (list (add1 x) y)  ;; E
                (list x (add1 y))  ;; S
                (list (sub1 x) y)) ;; W
               (list #\n #\e #\s #\w))))

(define (get-adjacent grid x y)
  (filter (compose not negative? cdr)
          (map (λ (ls dir) (cons dir (apply (curry get-element grid) ls)))
               (list
                (list x (sub1 y))  ;; N
                (list (add1 x) y)  ;; E
                (list x (add1 y))  ;; S
                (list (sub1 x) y)) ;; W
               (list #\n #\e #\s #\w))))

(define exadj (get-adjacent (grid->vec exgrid) 1 1))

;; make a rot function
;; have it take the output of get-adjacent
;; if any of the values in the alists are 1
;; set them to 2 and add 1 to the counter
;; in order to avoid n^2 performance, use mutation

(define (get-location dir x y)
  (match dir
    [#\n (list x (sub1 y))]
    [#\e (list (add1 x) y)]
    [#\s (list x (add1 y))]
    [#\w (list (sub1 x) y)]))

(map (λ (pair) (get-location (car pair) orig-x orig-y)) (filter (λ (p) (= 1 (cdr p))) exadj))

(define grid (grid->vec exgrid))

(define (rot adj-list orig-x orig-y)
   (let ([rot-coords (map (λ (pair) (get-location (car pair) orig-x orig-y))
                          (filter (λ (p) (= 1 (cdr p))) adj-list))])
     (displayln (format "rot coords: ~a" rot-coords))
     (for ([c rot-coords])
       (apply (curry update-grid-position! grid 2) c)
       (displayln (format "grid: ~a" grid)))))

;; (rot exadj 1 1)

(define (update-grid-position! grid v x y)
  (when (and (not (negative? x))
             (not (negative? y)))
    (let ([tbm (vector-ref grid y)])
      (vector-set! tbm x v)
      (vector-set! grid y tbm))))

(define (vector-ref-2 grid x y)
  (vector-ref (vector-ref grid y) x))

(define grid (grid->vec exgrid))

(define grid (grid->vec '((2 1 1)
                          (0 1 1)
                          (1 0 1))))

(define grid (grid->vec '((0 2))))

(define grid (grid->vec '((1))))

(define grid (grid->vec '((0))))

(define grid (grid->vec '((1 2))))


(define grid (grid->vec
              '((2 0 1 1 1 1 1 1 1 1)
                (1 0 1 0 0 0 0 0 0 1)
                (1 0 1 0 1 1 1 1 0 1)
                (1 0 1 0 1 0 0 1 0 1)
                (1 0 1 0 1 0 0 1 0 1)
                (1 0 1 0 1 1 0 1 0 1)
                (1 0 1 0 0 0 0 1 0 1)
                (1 0 1 1 1 1 1 1 0 1)
                (1 0 0 0 0 0 0 0 0 1)
                (1 1 1 1 1 1 1 1 1 1))))


(let ([num-mins 0])
  (if (not (findf-grid (curry = 1) grid))
      0
      (begin
        (for* ([x (vector-length (vector-ref grid 0))]
               [y (vector-length grid)])
          (when (= 2 (vector-ref-2 grid x y))
            (let* ([adj-grid (get-adjacent grid x y)]
                   [rot-coords (get-rot-coords adj-grid x y)])
              (displayln (format "adj: ~a rot: ~a" adj-grid rot-coords))
              (when (not (empty? rot-coords))
                (set! num-mins (add1 num-mins))
                (update-rot-coords! grid rot-coords)))))
        (cond [(not (findf-grid (curry = 2) grid)) -1]
              [(= num-mins 0) 0]
              [(findf-grid (curry = 1) grid) -1]
              [else (max 1 (sub1 num-mins))]))))

(findf-grid (curry = 1) grid)

(rot (get-adjacent grid 0 0) 0 0)

;; currently we are updating the grid
;; but we need to count how many rounds it takes to change everything to 2
;; and we can't double count for times when everything is already a 2

;; idea
;; update rot to return a 1 if anything is updated, 0 otherwise
;; (define (rot adj-list orig-x orig-y)
;;    (let ([rot-coords (map (λ (pair) (get-location (car pair) orig-x orig-y))
;;                           (filter (λ (p) (= 1 (cdr p))) adj-list))])
;;      (for ([c rot-coords])
;;        (apply (curry update-grid-position! grid 2) c)
;;        (displayln (format "grid: ~a" grid)))))

(define (grid->vec grid)
  (vector-map list->vector (list->vector grid)))

(define (get-element grid x y)
  (cond [(or (negative? x)
             (negative? y)
             (> x (sub1 (vector-length (vector-ref grid 0))))
             (> y (sub1 (vector-length grid)))) -1]
        [else
         (let ([y-row (vector-ref grid y)])
           (vector-ref y-row x))]))

(define (get-adjacent grid x y)
  (filter (compose not negative? cdr)
          (map (λ (ls dir) (cons dir (apply (curry get-element grid) ls)))
               (list
                (list x (sub1 y))  ;; N
                (list (add1 x) y)  ;; E
                (list x (add1 y))  ;; S
                (list (sub1 x) y)) ;; W
               (list #\n #\e #\s #\w))))

(define (get-location dir x y)
  (match dir
    [#\n (list x (sub1 y))]
    [#\e (list (add1 x) y)]
    [#\s (list x (add1 y))]
    [#\w (list (sub1 x) y)]))

(define (update-grid-position! grid v x y)
  (when (and (not (negative? x))
             (not (negative? y)))
    (let ([tbm (vector-ref grid y)])
      (vector-set! tbm x v)
      (vector-set! grid y tbm))))

(define (vector-ref-2 grid x y)
  (vector-ref (vector-ref grid y) x))

(define (get-rot-coords adj-list orig-x orig-y)
  (map (λ (pair) (get-location (car pair) orig-x orig-y))
       (filter (λ (p) (= 1 (cdr p))) adj-list)))

(define (update-rot-coords! grid coords [v 2])
  (for ([c coords])
       (apply (curry update-grid-position! grid v) c)))

(define (findf-grid pred grid)
  (for*/or ([x (in-range (vector-length (vector-ref grid 0)))]
            [y (in-range (vector-length grid))])
    (pred (get-element grid x y))))

(define (oranges-rotting grid)
  (let ([num-mins 0]
        [grid (grid->vec grid)])
    (if (not (findf-grid (curry = 1) grid))
      0
      (begin
        (for* ([x (vector-length (vector-ref grid 0))]
               [y (vector-length grid)])
          (when (= 2 (vector-ref-2 grid x y))
            (let* ([adj-grid (get-adjacent grid x y)]
                   [rot-coords (get-rot-coords adj-grid x y)])
              (when (not (empty? rot-coords))
                (set! num-mins (add1 num-mins))
                (update-rot-coords! grid rot-coords)))))
        (cond [(not (findf-grid (curry = 2) grid)) -1]
              [(= num-mins 0) 0]
              [(findf-grid (curry = 1) grid) -1]
              [else (max 1 (sub1 num-mins))])))))

(oranges-rotting grid)
