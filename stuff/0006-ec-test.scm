; Examples for Eager Comprehensions in [outer..inner|expr]-Convention
; ===================================================================
;
; sebastian.egner@philips.com, Eindhoven, The Netherlands, 26-Dec-2007.
; Scheme R5RS (incl. macros), SRFI-23 (error).
;
; Ported to Chicken Scheme by Ivan Raikov.
; 


; Tools for checking results
; ==========================

(define (my-equal? x y)
  (cond
   ((or (boolean? x) 
        (null? x)
        (symbol? x) 
        (char? x) 
        (input-port? x)
        (output-port? x) )
    (eqv? x y) )
   ((string? x)
    (and (string? y) (string=? x y)) )
   ((vector? x)
    (and (vector? y)
         (my-equal? (vector->list x) (vector->list y)) ))
   ((pair? x)
    (and (pair? y)
         (my-equal? (car x) (car y))
         (my-equal? (cdr x) (cdr y)) ))
   ((real? x)
    (and (real? y)
         (eqv? (exact? x) (exact? y))
         (if (exact? x)
             (= x y)
             (< (abs (- x y)) (/ 1 (expt 10 6))) ))) ; will do here
   (else
    (error "unrecognized type" x) )))

(define my-check-correct 0)
(define my-check-wrong   0)

(define-syntax my-check
  (syntax-rules (=>)
    ((my-check ec => desired-result)
     (begin
       (newline)
       (write (quote ec))
       (newline)
       (let ((actual-result ec))
         (display "  => ")
         (write actual-result)
         (if (my-equal? actual-result desired-result)
             (begin
               (display " ; correct")
               (set! my-check-correct (+ my-check-correct 1)) )
             (begin
               (display " ; *** wrong ***, desired result:")
               (newline)
               (display "  => ")
               (write desired-result)
               (set! my-check-wrong (+ my-check-wrong 1)) ))
         (newline) )))))
             

; ==========================================================================
; do-of 
; ==========================================================================

(my-check 
  (let ((x 0)) (do-of (set! x (+ x 1))) x) 
  => 1)

(my-check 
  (let ((x 0)) (do-of (:range i 10) (set! x (+ x 1))) x) 
  => 10)

(my-check 
  (let ((x 0)) (do-of (:range n 10) (:range k n) (set! x (+ x 1))) x) 
  => 45)


; ==========================================================================
; list-of and basic qualifiers 
; ==========================================================================

(my-check (list-of 1) => '(1))

(my-check (list-of (:range i 4) i) => '(0 1 2 3))

(my-check (list-of (:range n 3) (:range k (+ n 1)) (list n k)) 
  => '((0 0) (1 0) (1 1) (2 0) (2 1) (2 2)) )

(my-check 
  (list-of (:range n 5) (if (even? n)) (:range k (+ n 1)) (list n k)) 
  => '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4)) )

(my-check 
  (list-of (:range n 5) (not (even? n)) (:range k (+ n 1)) (list n k)) 
  => '((1 0) (1 1) (3 0) (3 1) (3 2) (3 3)) )

(my-check
  (list-of (:range n 5) 
           (and (even? n) (> n 2)) 
           (:range k (+ n 1)) 
           (list n k) )
  => '((4 0) (4 1) (4 2) (4 3) (4 4)) )

(my-check
  (list-of (:range n 5) 
           (or (even? n) (> n 3)) 
           (:range k (+ n 1)) 
           (list n k) )
  => '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4)) )

(my-check
 (let ((x 0)) (list-of (:range n 10) (begin (set! x (+ x 1))) n) x)
 => 10 )

(my-check
 (list-of (nested (:range n 3) (:range k n)) k)
 => '(0 0 1) )


; ==========================================================================
; Other comprehensions
; ==========================================================================

(my-check (append-of '(a b)) => '(a b))
(my-check (append-of (:range i 0) '(a b)) => '())
(my-check (append-of (:range i 1) '(a b)) => '(a b))
(my-check (append-of (:range i 2) '(a b)) => '(a b a b))

(my-check (string-of #\a) => (string #\a))
(my-check (string-of (:range i 0) #\a) => "")
(my-check (string-of (:range i 1) #\a) => "a")
(my-check (string-of (:range i 2) #\a) => "aa")

(my-check (string-append-of "ab") => "ab")
(my-check (string-append-of (:range i 0) "ab") => "")
(my-check (string-append-of (:range i 1) "ab") => "ab")
(my-check (string-append-of (:range i 2) "ab") => "abab")

(my-check (vector-of 1) => (vector 1))
(my-check (vector-of (:range i 0) i) => (vector))
(my-check (vector-of (:range i 1) i) => (vector 0))
(my-check (vector-of (:range i 2) i) => (vector 0 1))

(my-check (vector-of-length-of 1 1) => (vector 1))
(my-check (vector-of-length-of 0 (:range i 0) i) => (vector))
(my-check (vector-of-length-of 1 (:range i 1) i) => (vector 0))
(my-check (vector-of-length-of 2 (:range i 2) i) => (vector 0 1))

(my-check (sum-of 1) => 1)
(my-check (sum-of (:range i 0) i) => 0)
(my-check (sum-of (:range i 1) i) => 0)
(my-check (sum-of (:range i 2) i) => 1)
(my-check (sum-of (:range i 3) i) => 3)

(my-check (product-of 1) => 1)
(my-check (product-of (:range i 1 0) i) => 1)
(my-check (product-of (:range i 1 1) i) => 1)
(my-check (product-of (:range i 1 2) i) => 1)
(my-check (product-of (:range i 1 3) i) => 2)
(my-check (product-of (:range i 1 4) i) => 6)

(my-check (min-of 1) => 1)
(my-check (min-of (:range i 1) i) => 0)
(my-check (min-of (:range i 2) i) => 0)

(my-check (max-of 1) => 1)
(my-check (max-of (:range i 1) i) => 0)
(my-check (max-of (:range i 2) i) => 1)

(my-check (first-of #f 1) => 1)
(my-check (first-of #f (:range i 0) i) => #f)
(my-check (first-of #f (:range i 1) i) => 0)
(my-check (first-of #f (:range i 2) i) => 0)

(my-check 
  (let ((last-i -1))
    (first-of #f (:range i 10) (begin (set! last-i i)) i)
    last-i )
  => 0 )

(my-check (last-of #f 1) => 1)
(my-check (last-of #f (:range i 0) i) => #f)
(my-check (last-of #f (:range i 1) i) => 0)
(my-check (last-of #f (:range i 2) i) => 1)

(my-check (any-of #f) => #f)
(my-check (any-of #t) => #t)
(my-check (any-of (:range i 2 2) (even? i)) => #f)
(my-check (any-of (:range i 2 3) (even? i)) => #t)

(my-check (every-of #f) => #f)
(my-check (every-of #t) => #t)
(my-check (every-of (:range i 2 2) (even? i)) => #t)
(my-check (every-of (:range i 2 3) (even? i)) => #t)
(my-check (every-of (:range i 2 4) (even? i)) => #f)

(my-check 
 (let ((sum-sqr (lambda (x result) (+ result (* x x)))))
   (fold-of 0 (:range i 10) i sum-sqr) )
 => 285 )

(my-check 
 (let ((minus-1 (lambda (x) (- x 1)))
       (sum-sqr (lambda (x result) (+ result (* x x)))))
   (fold3-of (error "wrong") (:range i 10) i minus-1 sum-sqr) )
 => 284 )

(my-check 
 (fold3-of 'infinity (:range i 0) i min min)
 => 'infinity )


; ==========================================================================
; Typed generators
; ==========================================================================

(my-check (list-of (:list x '()) x) => '())
(my-check (list-of (:list x '(1)) x) => '(1))
(my-check (list-of (:list x '(1 2 3)) x) => '(1 2 3))
(my-check (list-of (:list x '(1) '(2)) x) => '(1 2))
(my-check (list-of (:list x '(1) '(2) '(3)) x) => '(1 2 3))

(my-check (list-of (:string c "") c) => '())
(my-check (list-of (:string c "1") c) => '(#\1))
(my-check (list-of (:string c "123") c) => '(#\1 #\2 #\3))
(my-check (list-of (:string c "1" "2") c) => '(#\1 #\2))
(my-check (list-of (:string c "1" "2" "3") c) => '(#\1 #\2 #\3))

(my-check (list-of (:vector x (vector)) x) => '())
(my-check (list-of (:vector x (vector 1)) x) => '(1))
(my-check (list-of (:vector x (vector 1 2 3)) x) => '(1 2 3))
(my-check (list-of (:vector x (vector 1) (vector 2)) x) => '(1 2))
(my-check 
 (list-of (:vector x (vector 1) (vector 2) (vector 3)) x)
 => '(1 2 3))

(my-check (list-of (:range x -2) x) => '())
(my-check (list-of (:range x -1) x) => '())
(my-check (list-of (:range x  0) x) => '())
(my-check (list-of (:range x  1) x) => '(0))
(my-check (list-of (:range x  2) x) => '(0 1))

(my-check (list-of (:range x  0  3) x) => '(0 1 2))
(my-check (list-of (:range x  1  3) x) => '(1 2))
(my-check (list-of (:range x -2 -1) x) => '(-2))
(my-check (list-of (:range x -2 -2) x) => '())

(my-check (list-of (:range x 1 5  2) x) => '(1 3))
(my-check (list-of (:range x 1 6  2) x) => '(1 3 5))
(my-check (list-of (:range x 5 1 -2) x) => '(5 3))
(my-check (list-of (:range x 6 1 -2) x) => '(6 4 2))

(my-check (list-of (:real-range x 0.0 3.0)     x) => '(0. 1. 2.))
(my-check (list-of (:real-range x 0   3.0)     x) => '(0. 1. 2.))
(my-check (list-of (:real-range x 0   3   1.0) x) => '(0. 1. 2.))

(my-check 
 (string-of (:char-range c #\a #\z) c) 
 => "abcdefghijklmnopqrstuvwxyz" )

;; (my-check 
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-of (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (my-call-with-input-file "tmp1"
;;     (lambda (port) (list-of (:port x port read) x)) ))
;;  => (list-of (:range n 10) n) )

;; (my-check 
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-of (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (my-call-with-input-file "tmp1"                 
;;      (lambda (port) (list-of (:port x port) x)) ))
;;  => (list-of (:range n 10) n) )


; ==========================================================================
; The special generators :do :let :parallel :while :until
; ==========================================================================

(my-check (list-of (:do ((i 0)) (< i 4) ((+ i 1))) i) => '(0 1 2 3))

(my-check 
 (list-of 
  (:do (let ((x 'x)))
       ((i 0)) 
       (< i 4) 
       (let ((j (- 10 i))))
       #t
       ((+ i 1)) )
  j )
 => '(10 9 8 7) )

(my-check (list-of (:let x 1) x) => '(1))
(my-check (list-of (:let x 1) (:let y (+ x 1)) y) => '(2))
(my-check (list-of (:let x 1) (:let x (+ x 1)) x) => '(2))

(my-check 
 (list-of (:parallel (:range i 1 10) (:list x '(a b c))) (list i x))
 => '((1 a) (2 b) (3 c)) )

(my-check 
 (list-of (:while (:range i 1 10) (< i 5)) i)
 => '(1 2 3 4) )

(my-check 
 (list-of (:until (:range i 1 10) (>= i 5)) i)
 => '(1 2 3 4 5) )

; with generator that might use inner bindings

(my-check
 (list-of (:while (:list i '(1 2 3 4 5 6 7 8 9)) (< i 5)) i)
 => '(1 2 3 4) )
; Was broken in original reference implementation as pointed
; out by sunnan@handgranat.org on 24-Apr-2005 comp.lang.scheme.
; Refer to http://groups-beta.google.com/group/comp.lang.scheme/
; browse_thread/thread/f5333220eaeeed66/75926634cf31c038#75926634cf31c038

(my-check 
 (list-of (:until (:list i '(1 2 3 4 5 6 7 8 9)) (>= i 5)) i)
 => '(1 2 3 4 5) )

(my-check
 (list-of (:while (:vector x (index i) '#(1 2 3 4 5))
		  (< x 10))
	  x)
 => '(1 2 3 4 5))
; Was broken in reference implementation, even after fix for the
; bug reported by Sunnan, as reported by Jens-Axel Soegaard on
; 4-Jun-2007.

; combine :while/:until and :parallel

(my-check
 (list-of (:while (:parallel (:range i 1 10)
                             (:list j '(1 2 3 4 5 6 7 8 9)))
                  (< i 5))
          (list i j))
 => '((1 1) (2 2) (3 3) (4 4)))

(my-check
 (list-of (:until (:parallel (:range i 1 10)
                             (:list j '(1 2 3 4 5 6 7 8 9)))
                  (>= i 5))
          (list i j))
 => '((1 1) (2 2) (3 3) (4 4) (5 5)))

; check that :while/:until really stop the generator

(my-check
 (let ((n 0))
   (do-of (:while (:range i 1 10) (begin (set! n (+ n 1)) (< i 5)))
          (if #f #f))
   n)
 => 5)

(my-check
 (let ((n 0))
   (do-of (:until (:range i 1 10) (begin (set! n (+ n 1)) (>= i 5)))
          (if #f #f))
   n)
 => 5)

(my-check
 (let ((n 0))
   (do-of (:while (:parallel (:range i 1 10)
                             (:do () (begin (set! n (+ n 1)) #t) ()))
                  (< i 5))
          (if #f #f))
   n)
 => 5)

(my-check
 (let ((n 0))
   (do-of (:until (:parallel (:range i 1 10)
                             (:do () (begin (set! n (+ n 1)) #t) ()))
                  (>= i 5))
          (if #f #f))
   n)
 => 5)

; ==========================================================================
; The dispatching generator
; ==========================================================================

(my-check (list-of (: c '(a b)) c) => '(a b))
(my-check (list-of (: c '(a b) '(c d)) c) => '(a b c d))

(my-check (list-of (: c "ab") c) => '(#\a #\b))
(my-check (list-of (: c "ab" "cd") c) => '(#\a #\b #\c #\d))

(my-check (list-of (: c (vector 'a 'b)) c) => '(a b))
(my-check (list-of (: c (vector 'a 'b) (vector 'c)) c) => '(a b c))

(my-check (list-of (: i 0) i) => '())
(my-check (list-of (: i 1) i) => '(0))
(my-check (list-of (: i 10) i) => '(0 1 2 3 4 5 6 7 8 9))
(my-check (list-of (: i 1 2) i) => '(1))
(my-check (list-of (: i 1 2 3) i) => '(1))
(my-check (list-of (: i 1 9 3) i) => '(1 4 7))

(my-check (list-of (: i 0.0 1.0 0.2) i) => '(0. 0.2 0.4 0.6 0.8))

(my-check (list-of (: c #\a #\c) c) => '(#\a #\b #\c))

;; (my-check 
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-of (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (my-call-with-input-file "tmp1"                 
;;      (lambda (port) (list-of (: x port read) x)) ))
;;  => (list-of (:range n 10) n) )
    
;; (my-check 
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-of (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (my-call-with-input-file "tmp1"                 
;;      (lambda (port) (list-of (: x port) x)) ))
;;  => (list-of (:range n 10) n) )


; ==========================================================================
; With index variable
; ==========================================================================

(my-check (list-of (:list c (index i) '(a b)) (list c i)) => '((a 0) (b 1)))
(my-check (list-of (:string c (index i) "a") (list c i)) => '((#\a 0)))
(my-check (list-of (:vector c (index i) (vector 'a)) (list c i)) => '((a 0)))

(my-check 
 (list-of (:range i (index j) 0 -3 -1) (list i j)) 
 => '((0 0) (-1 1) (-2 2)) )

(my-check 
 (list-of (:real-range i (index j) 0 1 0.2) (list i j)) 
 => '((0. 0) (0.2 1) (0.4 2) (0.6 3) (0.8 4)) )

(my-check 
 (list-of (:char-range c (index i) #\a #\c) (list c i)) 
 => '((#\a 0) (#\b 1) (#\c 2)) )

(my-check 
 (list-of (: x (index i) '(a b c d)) (list x i))
 => '((a 0) (b 1) (c 2) (d 3)) )

;; (my-check 
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-of (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (my-call-with-input-file "tmp1"
;;      (lambda (port) (list-of (: x (index i) port) (list x i))) ))
;;  => '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9)) )


; ==========================================================================
; The examples from the SRFI document
; ==========================================================================

; from Abstract

(my-check (list-of (: i 5) (* i i)) => '(0 1 4 9 16))

(my-check 
  (list-of (: n 1 4) (: i n) (list n i)) 
  => '((1 0) (2 0) (2 1) (3 0) (3 1) (3 2)) )

; from Generators

(my-check 
  (list-of (: x (index i) "abc") (list x i)) 
  => '((#\a 0) (#\b 1) (#\c 2)) )

(my-check
  (list-of (:string c (index i) "a" "b") (cons c i))
  => '((#\a . 0) (#\b . 1)) )


; ==========================================================================
; Little Shop of Horrors
; ==========================================================================

(my-check (list-of (:range x 5) (:range x x) x) => '(0 0 1 0 1 2 0 1 2 3))

(my-check (list-of (:list x '(2 "23" (4))) (: y x) y) => '(0 1 #\2 #\3 4))

(my-check 
 (list-of (:parallel (:integers x) 
                     (:do ((i 10)) (< x i) ((- i 1))))
          (list x i))
 => '((0 10) (1 9) (2 8) (3 7) (4 6)) )


; ==========================================================================
; Less artificial examples
; ==========================================================================

(define (factorial n) ; n * (n-1) * .. * 1 for n >= 0
  (product-of (:range k 2 (+ n 1)) k) )

(my-check (factorial  0) => 1)
(my-check (factorial  1) => 1)
(my-check (factorial  3) => 6)
(my-check (factorial  5) => 120)


(define (eratosthenes n) ; primes in {2..n-1} for n >= 1
  (let ((p? (make-string n #\1)))
    (do-of (:range k 2 n)
           (if (char=? (string-ref p? k) #\1))
           (:range i (* 2 k) n k)
           (string-set! p? i #\0) )
    (list-of (:range k 2 n) (if (char=? (string-ref p? k) #\1)) k) ))

(my-check 
 (eratosthenes 50)
 => '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47) )

(my-check
 (length (eratosthenes 5000))
 => 669 )


(define (pythagoras n) ; a, b, c s.t. 1 <= a <= b <= c <= n, a^2 + b^2 = c^2
  (list-of 
   (:let sqr-n (* n n))
   (:range a 1 (+ n 1))
; (begin (display a) (display " "))
   (:let sqr-a (* a a))
   (:range b a (+ n 1)) 
   (:let sqr-c (+ sqr-a (* b b)))
   (if (<= sqr-c sqr-n))
   (:range c b (+ n 1))
   (if (= (* c c) sqr-c))
   (list a b c) ))
           
(my-check
 (pythagoras 15)
 => '((3 4 5) (5 12 13) (6 8 10) (9 12 15)) )

(my-check
 (length (pythagoras 100))
 => 52 )


(define (qsort xs) ; stable
  (if (null? xs)
      '()
      (let ((pivot (car xs)) (xrest (cdr xs)))
        (append
         (qsort (list-of (:list x xrest) (if (<  x pivot)) x))
         (list pivot)
         (qsort (list-of (:list x xrest) (if (>= x pivot)) x)) ))))

(my-check 
 (qsort '(1 5 4 2 4 5 3 2 1 3))
 => '(1 1 2 2 3 3 4 4 5 5) )


(define (pi-BBP m) ; approx. of pi within 16^-m (Bailey-Borwein-Plouffe)
  (sum-of 
    (:range n 0 (+ m 1))
    (:let n8 (* 8 n))
    (* (- (/ 4 (+ n8 1))
          (+ (/ 2 (+ n8 4))
             (/ 1 (+ n8 5))
             (/ 1 (+ n8 6))))
       (/ 1 (expt 16 n)) )))

(my-check
 (pi-BBP 5)
 => (/ 40413742330349316707 12864093722915635200) )


;; (define (read-line port) ; next line (incl. #\newline) of port
;;   (let ((line
;;          (string-of 
;;           (:until (:port c port read-char)
;;                   (char=? c #\newline) )
;;           c )))
;;     (if (string=? line "")
;;         (read-char port) ; eof-object
;;         line )))

;; (define (read-lines filename) ; list of all lines
;;   (my-call-with-input-file 
;;    filename
;;    (lambda (port)
;;      (list-of (:port line port read-line) line) )))

;; (my-check
;;  (begin
;;    (let ((f (my-open-output-file "tmp1")))
;;      (do-of (:range n 10) (begin (write n f) (newline f)))
;;      (close-output-port f))
;;    (read-lines "tmp1") )
;;  => (list-of (:char-range c #\0 #\9) (string c #\newline)) )


; ==========================================================================
; Summary
; ==========================================================================

(begin
  (newline)
  (newline)
  (display "correct examples : ")
  (display my-check-correct)
  (newline)
  (display "wrong examples   : ")
  (display my-check-wrong)
  (newline)
  (newline) )
