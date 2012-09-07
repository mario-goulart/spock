;;;; cells.scm


(define w 30)
(define h 30)

(define cells (make-array (shape 0 w 0 h)))
(define col #t)

(define (create)
  (do ((x 0 (+ x 1)))
      ((>= x w))
    (do ((y 0 (+ y 1)))
	((>= y h))
      (let ((div ((native document.createElement) "div")))
	(array-set! cells x y div)
	(set! (.style.left div) (string-append (number->string (+ (* x 5) 50)) "px"))
	(set! (.style.top div) (string-append (number->string (+ (* y 5) 50)) "px"))
	(set! (.stype.backgroundColor div) (if col "red" "green"))
	(set! (.stype.position div) "absolute")
	(set! (.stype.width div) 5)
	(set! (.stype.height div) 5)
	(set! col (not col))
	((native document.body.appendChild) div)))))

(create)
