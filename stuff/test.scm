;;;; test.scm - support definitions for test-suite


(define +column1-width+ 60)
(define +column2-width+ 10)

(define tests-succeeded 0)
(define tests-failed 0)

(define (->string x)
  (if (string? x)
      x
      (with-output-to-string (cut write x))))

(define (padl s n)
  (let ((len (string-length s)))
    (cond ((>= len n) 
	   (string-append (substring s 0 (- len 4)) " ..."))
	  (else 
	   (string-append (make-string (- n len) #\space) s)))))

(define (padr s n)
  (let ((len (string-length s)))
    (cond ((>= len n) 
	   (string-append (substring s 0 (- n 4)) " ..."))
	  (else 
	   (string-append s (make-string (- n len) #\space))))))

(define (test-begin sec)
  (display (string-append "\n" (symbol->string sec) ":\n\n")))

(define (test msg thunk)
  (display (padr msg +column1-width+))
  (cond ((thunk)
	 (set! tests-succeeded (+ tests-succeeded 1))
	 (display (padl "OK" +column2-width+)))
	(else
	 (set! tests-failed (+ tests-failed 1))
	 (display (padl "FAIL" +column2-width+))))
  (newline))

(define (testt form thunk)
  (test (->string form) thunk))

(define-syntax t
  (syntax-rules ()
    ((_ msg form) (test msg (lambda () form)))
    ((_ form) (testt 'form (lambda () form)))))

(define (testf form thunk)
  (test (string-append "(not " (->string form) ")") thunk))

(define-syntax f
  (syntax-rules ()
    ((_ msg form) (test msg (lambda () (not form))))
    ((_ form) (testf 'form (lambda () (not form))))))

(define (teste result form thunk)
  (let ((tmp (string-append (->string form) " == " (->string result))))
    (test tmp thunk)))

(define-syntax e
  (syntax-rules ()
    ((_ msg result form) (test msg (lambda () (equal? result form))))
    ((_ result form) (teste result 'form (lambda () form)))))
