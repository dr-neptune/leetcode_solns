#lang racket
(require racket)

;; idea
;; for the given column number, see how many times 26 divides and and its remainder
;; then return a string concatenating each of the divides as "z" and the remainder as the last

(define (convert-to-title columnNumber)
  (let*-values ([(q r) (quotient/remainder columnNumber 26)]
                [(last-char) (if (= r 0)
                                 '()
                                 (list (integer->char (+ r 64))))]
                [(z) (integer->char (+ 1 64))])
    (list->string (append (build-list q (lambda (_) z)) last-char))))




(convert-to-title 25)
(convert-to-title 26)
(convert-to-title 27)
(convert-to-title 28)
(convert-to-title 112)
(convert-to-title 156)
(convert-to-title 701)

(integer->char (+ 1 192))


(define-values (q r) (quotient/remainder 112 26))

(list->string (append (build-list 5 (lambda (_) (integer->char (+ 26 66))))
                      (list (integer->char (+ 8 65)))))



(integer->char (+ 8 96))

(char->integer #\Z)

;; idea
;; Z AA AB AC ... AZ BA BB BC ... BZ CA CB CC ...
;; if q = 0, return end
;; if q = 1, return A + end
;; if q = 2, return B + end
;; ...
;; if q = 26, return Z + end
;; if q = 27, get q r for it and return 1 1 -> AA + end
;; if q = 55, then q = 2 3 and return BC
;; if q = 677, then q = 26 1, and return ZA
;; if q = 701, then q = 26 25, and return ZY
;; if q = 703, then q = 27 1, then q2(27) = 1 1 = AAA
;; if q = 745, then q = 28 17, then q2(28) = 1 2 and AB + 17

;; if q = 1, return A
;; if q = 26, return Z
;; if q = 27, q > 26 -> q2(1) q r = 0 1 + r = 0 1 1 -> A A
;; if q = 52, q > 26 -> q2(52 - 26 = 26) -> q2 >

;; if num = 52, q r = 2 0 then q > 26 -> q2(52 - 26 = 26), then q !> 26 and we return 1 26 = AZ
;; if q = 104, then q >


;; if num = 1, then q r = 0 1 -> 1
;; if num = 26, then
;; if num = 52, then q r = 2 0 -> 2 1


(define (get-initials number)
  (if (> number 26)
      (+ 1 (get-initials (- number 26)))
      number))

(get-initials 52)


(quotient/remainder (get-initials 52) 26)


(quotient/remainder 52 26)
(- 52 26)

(quotient/remainder 703 26)
(quotient/remainder 28 26)

(define (get-initials num-q)
  (let-values ([(q r) (quotient/remainder num-q 26)])
    (cond [(> q 26)
           (append (get-initials (quotient num-q 26)) (list (add1 r)))]
          [(and (= q 1) (= r 0)) (list 26 r)]
          [else (list q r)])))

(quotient/remainder 26 26)

(get-initials 26)
(get-initials 27)
(get-initials 28)
(get-initials 701)
(get-initials 703)
(get-initials 745)

(list->string (map (lambda (i) (integer->char (+ i 64))) (get-initials 701)))

(define (convert-to-title columnNumber)
  (list->string (map (lambda (i) (integer->char (+ i 64))) (filter positive? (get-initials columnNumber)))))

(convert-to-title 1)
(convert-to-title 28)
(convert-to-title 701)
(convert-to-title 745)

(convert-to-title 23)
(convert-to-title 24)
(convert-to-title 25)
(convert-to-title 26)
(convert-to-title 27)

(convert-to-title 52)
(convert-to-title 53)


(filter posit8ive(get-initials 26))

(integer->char (+ 26 64))


(quotient/remainder 701 26)
(quotient/remainder 703 26)
(quotient/remainder 27 26)


(define (get-initials num-q)
  (let-values ([(q r) (quotient/remainder num-q 26)])
    (cond [(> q 26)
           (append (get-initials (quotient num-q 26)) (list r))]
          [else (list q r)])))

(get-initials 701)
(get-initials 702)

(quotient/remainder 702 26)

(get-initials 703)
(get-initials 702)


;; 1 -> 26     A  B  C  ... Y  Z
;; 27 -> 52    AA AB AC ... AY AZ
;; 53 -> 78    BA BB BC ... BY BZ
;; 79 -> 104   CA CB CC ... CY CZ

;; 701 -> 726  ZY ZZ AAA .. AAB AAC

(quotient/remainder 701 26)

(define (get-initials num-q)
  (let-values ([(q r) (quotient/remainder num-q 26)])
    (cond [(> q 25)
           (append (get-initials (quotient num-q 26)) (list r))]
          [else (list q r)])))

(get-initials 1)
(get-initials 26)
(get-initials 27)
(get-initials 52)  ;; AZ
(get-initials 53)  ;; BA
(get-initials 78)  ;; BZ
(get-initials 79)  ;; CA

;; get first remainder. This is the last letter
(remainder 25 26)
(remainder 1 26)
(remainder 25 26)


(define (get-initials num [result '()])
  (if (< num 0)
      (result)
      (get-initials (remainder (sub1 num) 26)
                    (append result (remainder (sub1 num) 26)))))

(define (get-initials num)
  (if (< num 0)
      '()
      (append
       (list num)
       (get-initials (remainder (sub1 num) 26)))))


(get-initials 1)
(get-initials 26)
(get-initials 27)
(get-initials 52)  ;; AZ
(get-initials 53)  ;; BA
(get-initials 78)  ;; BZ
(get-initials 79)  ;; CA
