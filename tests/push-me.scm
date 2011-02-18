;;;; push-me.scm


(define-native-method document.getElementById)
(define-native setTimeout)
(define-native setInterval)
(define-native clearInterval)
(define-native alert)
(define-native Math.random)

(define (box) (document.getElementById document. "box"))
(define (info) (document.getElementById document. "info"))

(define (mouse-position event)
  (values 
   (- (+ (.clientX event) document.body.scrollLeft) document.body.clientLeft)
   (- (+ (.clientY event) document.body.scrollTop) document.body.clientTop)))

(define (mouse-move event)
  (call-with-values (cut mouse-position event)
    (lambda (x y)
      (move-element (box) x y)
      (show-position x y))))

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

(define mode 'jitter)
(define timer #f)

(define (show-position x y)
  (set! (.innerHTML (info))
    (jstring
     (string-append
      (number->string x) "/" (number->string y)))))

(define (jitter elt)
  (let ((x (- (* (Math.random) 4) 2))
	(y (- (* (Math.random) 4) 2)))
    (move-element-by elt x y)))

(define (start-timer to)
  (unless timer
    (set! timer (setInterval (callback (lambda () (jitter (box)))) to))))

(set! document.onclick
  (callback
   (lambda ()
     (set! mode
       (case mode
	 ((jitter)
	  (set! document.onmousemove (callback mouse-move))
	  (when timer
	    (clearInterval timer)
	    (set! timer #f))
	  'follow)
	 ((follow) 
	  (set! document.onmousemove (callback void))
	  (start-timer 100)
	  'jitter))))))

(start-timer 100)
