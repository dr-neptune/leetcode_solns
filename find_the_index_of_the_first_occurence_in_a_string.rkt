#lang racket
(require racket)

(define (str-str haystack needle)
  (let ([hay-ls (string->list haystack)]
        [needle-ls (string->list needle)]
        [needle-len (string-length needle)])
    (let iter ([hls hay-ls]
               [curr-idx 0])
      (cond [(< (length hls) needle-len) -1]
            [(equal? (take hls needle-len) needle-ls) curr-idx]
            [else (iter (rest hls) (add1 curr-idx))]))))

(str-str "sadbutsad" "sad")
(str-str "leetcode" "leeto")
