#lang racket
(require racket)


(define (string->slist str)
  (map string (string->list str)))


(define (check-strings strls [vals '()])
  (cond [(empty? (first strls)) vals]
        [(apply string-ci=? (map first strls))
         (check-strings (map rest strls) (cons vals (caar strls)))]
        [else vals]))


(define (longest-common-prefix strs)
  (let* ([string-lists (map string->slist strs)]
         [min-size (apply min (map length string-lists))]
         [string-lists (map (lambda (x) (take x min-size)) string-lists)])
  (string-append* (flatten (check-strings string-lists)))))


(module+ test
  (require rackunit)
  (check-equal? (longest-common-prefix '("flower" "flow" "flight")) "fl")
  (check-equal? (longest-common-prefix '("dog" "racecar" "car")) ""))

;; alternative leetcoders soln
;; (define (longest-common-prefix strs)
;;   (let ([clsts (map string->list strs)])
;;     (list->string (foldl take-common-prefix (car clsts) (cdr clsts)))))
