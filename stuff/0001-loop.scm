(define c 0)

(display
 (milliseconds
  (lambda ()
    (do ((i 10000 (- i 1)))
	((zero? i))
      (set! c (+ c 1))))))
(newline)

(display c)
