;;;; misc.scm - miscellaneous utility functions


(define temp
  (let ((count 1))
    (lambda prefix
      (let ((i count))
	(set! count (+ count 1))
	(string->symbol
	 (string-append
	  (if (pair? prefix)
	      (car prefix)
	      "t")
	  (number->string i)))))))

(define (dribble . args)
  (for-each
   (cut display <> (current-error-port))
   args)
  (newline (current-error-port)))

(define (read-forms file-or-port)
  ((if (string? file-or-port)
       call-with-input-file
       (lambda (fp p) (p fp)))
   file-or-port
   (lambda (port)
     (let loop ((xs '()))
       (let ((x (read port)))
	 (if (eof-object? x)
	     `(begin ,@(reverse xs))
	     (loop (cons x xs))))))))

(define (copy-file-data file)
  (with-input-from-file file
    (lambda ()
      (let loop ()
	(let ((c (read-char)))
	  (unless (eof-object? c)
	    (write-char c)
	    (loop)))))))

(define (emit . xs)
  (for-each display xs))

(define (emit-list xs)
  (match xs
    (() #f)
    ((x) (emit x))
    ((x1 xs ...)
     (emit x1)
     (for-each (lambda (x) (emit ", " x)) xs))))

(define (stringify x)
  (cond ((symbol? x) (symbol->string x))
	((string? x) x)
	(else (error "can't stringify" x))))

(define (symbolify x)
  (cond ((symbol? x) x)
	((string? x) (string->symbol x))
	(else (error "can't symbolify" x))))

(define (join xs sep)
  (apply
   string-append
   (let loop ((xs xs))
     (cond ((null? xs) '())
	   ((null? (cdr xs)) xs)
	   (else (cons (car xs) (cons sep (loop (cdr xs)))))))))

(define (fail msg . arg)
  (let ((out (current-error-port)))
    (display "\nError: " out)
    (display msg out)
    (cond ((pair? arg)
	   (display ":\n\n" out)
	   (pp (car arg) out))
	  (else (newline out)))
    (exit 1)))

(define (symbol<? s1 s2)
  (string<? (symbol->string s1) (symbol->string s2)))

(define (butlast lst)
  (let loop ((lst lst))
    (if (null? (cdr lst))
	'()
	(cons (car lst) (loop (cdr lst))))))

(define (last lst)
  (let loop ((lst lst))
    (if (null? (cdr lst))
	(car lst)
	(loop (cdr lst)))))

(define (test-option opt state)
  (cond ((assq opt state) => cdr)
	(else #f)))

(define (identifier id)
  (let* ((str (stringify id))
	 (out (open-output-string))
	 (n (string-length str)))
    (display "___" out)
    (do ((i 0 (+ i 1)))
	((>= i n) (get-output-string out))
      (let ((c (string-ref str i)))
	(if (and (not (char-lower-case? c))
		 (or (not (char-numeric? c)) (= i 0)))
	    (let ((i (char->integer c)))
	      (write-char #\_ out)
	      (when (< i 16) (write-char #\0 out))
	      (display (number->string i 16) out))
	    (write-char c out))))))

(define (read-library state name . reader)
  (let loop ((lpath (test-option 'library-path state)))
    (if (null? lpath)
	((or (test-option 'fail state) fail)
	 "library not found" name)
	(let ((lib (file-exists? (string-append (car lpath) "/" name))))
	  (cond (lib
		 (when (test-option 'verbose state)
		   (dribble "reading library " lib))
		 ((if (pair? reader) (car reader) read-forms) lib))
		(else (loop (cdr lpath))))))))

(define (parse-llist llist)
  (let loop ((ll llist) (vars '()))
    (match ll
      (() (list (reverse vars) #f))
      ((? symbol?) (list (reverse (cons ll vars)) ll))
      (((? symbol? v) . more)
       (loop more (cons v vars)))
      (_ (fail "bad lambda list" llist)))))

(define (string-find-char c str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (and (< i len)
	   (or (char=? c (string-ref str i))
	       (loop (+ i 1)))))))

(define (note loc state x . args)
  (when (or (not state) (test-option 'verbose state))
    (apply
     dribble
     (append
      (if loc
	  `("(" ,loc ") ")
	  '())
      args)))
  x)

(define (read-contents filename)
  (with-input-from-file filename
    (lambda ()
      (with-output-to-string
	(lambda ()
	  (let loop ()
	    (let ((c (read-char)))
	      (unless (eof-object? c)
		(write-char c)
		(loop)))))))))
