;;
;; Useful for storing random'ish utilities
;;

(define (inc x)
  (+ 1 x))

(define (dec x)
  (- x 1))

(define (float-val x)
  (exact->inexact x))

(define (int-val x)
  (inexact->exact (truncate x)))
