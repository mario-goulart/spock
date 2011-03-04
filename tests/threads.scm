;;;; threads.scm


;; http://www.pixelwit.com/blog/2008/04/how-to-draw-a-spiral/

;; centerX-- X origin of the spiral.
;; centerY-- Y origin of the spiral.
;; radius--- Distance from origin to outer arm.
;; sides---- Number of points or sides along the spiral's arm.
;; coils---- Number of coils or full rotations. (Positive numbers spin clockwise, negative numbers spin counter-clockwise)
;; rotation- Overall rotation of the spiral. ('0'=no rotation, '1'=360 degrees, '180/360'=180 degrees)
      
(define (spiral ctx center-x center-y radius sides coils rotation)
  (let* ((away-step (/ radius sides))
	 (around-step (/ coils sides))
	 (around-radians (* around-step 2 Math.PI))
	 (rotation (* rotation 2 Math.PI)))
    (let loop ((i 0) (px center-x) (py center-y))
      (yield)
      (cond ((<= i sides)
	     (%inline ".beginPath" ctx)
	     (%inline ".moveTo" ctx px py)
	     (let* ((away (* i away-step))
		    (around (+ (* i around-radians) rotation))
		    (x (+ center-x (* (%inline "Math.cos" around) away)))
		    (y (+ center-y (* (%inline "Math.sin" around) away))))
	       (%inline ".lineTo" ctx x y)
	       (%inline ".stroke" ctx)
	       (loop (+ i 1) x y)))
	    (else
	     (%inline 
	      ".fillRect" ctx 
	      (- center-x radius 10) (- center-y radius 10)
	      (+ 20 (* radius 2)) (+ 20 (* radius 2)))
	     (loop 0 center-x center-y))))))

(define canvas (%inline "document.getElementById" "canvas"))
(define ctx (%inline ".getContext" canvas "2d"))

(set! (.lineWidth ctx) 5)
(set! (.lineStyle ctx) "rgb(0, 0, 255)")
(set! (.fillStyle ctx) "rgb(255, 200, 255)")

(%inline ".fillRect" ctx 0 0 600 600)

(define halt #f)
(define threads '())

(let* ((n 3)
       (wh (/ 600 n)))
  (do ((x 1 (+ x 1)))
      ((> x n))
    (let ((cx (- (* wh x) (/ wh 2))))
      (do ((y 1 (+ y 1)))
	  ((> y n))
	(let ((cy (- (* wh y) (/ wh 2))))
	  (set! threads
	    (cons 
	     (lambda () 
	       ;;(%inline "console.log" cx cy)
	       (spiral ctx cx cy (/ wh 2) 100 4 (%inline "Math.random")))
	     threads)))))))

(define current threads)

(define (yield)
  (call-with-current-continuation
   (lambda (k)
     (set-car! current (lambda () (k #f)))
     (set! current (cdr current))
     (when (null? current) (set! current threads))
     (%inline "setTimeout" (callback (lambda () ((car current)))) 10)
     (halt))))

(call-with-current-continuation
 (lambda (k)
   (set! halt (lambda () (k #f)))
   ((car threads))))
