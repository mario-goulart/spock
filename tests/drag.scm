;;;; drag.scm


(define-native-method document.getElementById)

(define (box) (%inline "document.getElementById" "box"))
(define (info) (%inline "document.getElementById" "info"))

(define down #f)

(define (mouse-position event)
  (values 
   (- (+ (.clientX event) document.body.scrollLeft) document.body.clientLeft)
   (- (+ (.clientY event) document.body.scrollTop) document.body.clientTop)))

(define (mouse-move event)
  (let ((event (if (void? event) window.event event)))
    (when down
      (call-with-values (cut mouse-position event)
	(lambda (x y)
	  (move-element (box) x y)
	  (show-position x y))))))

(define (move-element elt x y)
  (set! (.style.left elt) x)
  (set! (.style.top elt) y))

(define (move-element-by elt x y)
  (call-with-values (cut element-position elt)
    (lambda (x1 y1)
      (move-element elt (+ x1 x) (+ y1 y)))))

(define (element-position elt)
  (values 
   (.offsetLeft elt)
   (.offsetTop elt)))

(define (show-position x y)
  (set! (.innerHTML (info))
    (jstring
     (string-append
      (number->string x) "/" (number->string y)))))

(set! document.onmousemove (callback mouse-move))
(set! document.onmousedown (callback (lambda () (set! down #t))))
(set! document.onmouseup (callback (lambda () (set! down #f))))
