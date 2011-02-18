;;;; library.scm - runtime-library (Scheme part)


(let-syntax
    ((case-lambda   ; this is not the same as the usual `case-lambda'!
      (letrec-syntax ((scan
		       (syntax-rules ()
			 ((_ (x) (lst ...)) (%dispatch lst ... x))
			 ((_ ((llist . body) . more) (lst ...))
			  (scan more (lst ... (lambda llist . body)))))))
	(syntax-rules ()
	  ((_ clause ...)
	   (lambda args
	     (scan (clause ...) ()))))))
     (define-inline
	 (syntax-rules 
	     ___ ()
	     ((_ (name . args) body ___)
	      (define-syntax name
		(lambda args body ___))))))
  (begin

    (define-library-section internal-essentials

      (default

	;; (%string->jstring STRING) -> JSTRING
	(define-inline (%string->jstring x) (%inline "SPOCK.jstring" x))

	;; (%jstring->string JSTRING) -> STRING
	(define-inline (%jstring->string x) (%inline "new SPOCK.String" x))

	;; (%list ...) -> LIST
	;; used for manifest `lambda' with rest argument
	(define (%list . xs) xs)

	;; (%car X) -> Y
	(define-inline (%car x) (%property-ref "car" x))

	;; (%cdr X) -> Y
	(define-inline (%cdr x) (%property-ref "cdr" x))

	))


    (define-library-section essentials

      (default

	(define-inline (eq? x y) 
	  (%inline (1 "===" 2) x y))

	(define-inline (eqv? x y)
	  (%inline "SPOCK.eqvp" x y))

	(define-inline (equal? x y) (%inline "SPOCK.equalp" x y))
	(define-inline (not x) (if x #f #t))

	))

    
    (define-library-section basic-type-predicates

      (default

	(define-inline (symbol? x) (%inline (1 "instanceof SPOCK.Symbol") x))
	(define-inline (pair? x) (%inline (1 "instanceof SPOCK.Pair") x))
	(define-inline (number? x) (eq? (%inline "typeof" x) "number"))
	(define-inline (char? x) (%inline (1 "instanceof SPOCK.Char") x))
	(define-inline (void? x) (%void? x))
	(define-inline (vector? x) (%inline (1 "instanceof Array") x))
	(define-inline (procedure? x) (eq? (%inline "typeof" x) "function"))
	(define-inline (eof-object? x) (eq? x (%host-ref "SPOCK.EOF")))
	(define-inline (boolean? x) (or (eq? x #t) (eq? x #f)))

	(define-inline (string? x)
	  (or (eq? (%inline "typeof" x) "string")
	      (%inline (1 "instanceof SPOCK.String") x)))

	))


    (define-library-section multiple-values

      (default

	(define values
	  (%native-lambda
	   "return K.apply(this, Array.prototype.slice.call(arguments, 1));"))

	(define call-with-values
	  (%native-lambda
	   "var thunk = arguments[ 1 ];"
	   "var proc = arguments[ 2 ];"
	   "function k2() {"
	   " var args = Array.prototype.slice.call(arguments);"
	   " args.unshift(K);"
	   " return proc.apply(this, args);}"
	   "return thunk(k2);"))

	))


    (define-library-section multiple-value-hacks

      (default

	;; (%call-with-saved-values THUNK1 THUNK2)
	(define %call-with-saved-values
	  (%native-lambda
	   "var t1 = arguments[ 1 ];"
	   "var t2 = arguments[ 2 ];"
	   "var args;"
	   "function k2() { return K.apply(this, args); }"
	   "function k1() {"
	   " args = Array.prototype.slice.call(arguments);"
	   " return t2(k2);}"
	   "return t1(k1);"))

	))


    (define-library-section nonstandard-essentials

      (default

	(define-inline (void) (%void))	; ignores extra arguments

	))


    (define-library-section basic-list-operations

      (default

	(define-inline (null? x) (eq? x '()))
	(define-inline (car x) (%car (%check ("SPOCK.Pair") x)))
	(define-inline (cdr x) (%cdr (%check ("SPOCK.Pair") x)))
	(define-inline (list . xs) xs)
	(define-inline (cons x y) (%inline "new SPOCK.Pair" x y))
	
	(define-inline (set-car! x y)
	  (%inline (1 ".car = " 2) (%check ("SPOCK.Pair") x) y))

	(define-inline (set-cdr! x y)
	  (%inline (1 ".cdr = " 2) (%check ("SPOCK.Pair") x) y))

	(define (list? x)
	  (let loop ((fast x) (slow x))
	    (or (null? fast)
		(and (pair? fast)
		     (let ((fast (%cdr fast)))
		       (or (null? fast)
			   (and (pair? fast)
				(let ((fast (%cdr fast))
				      (slow (%cdr slow)))
				  (and (not (eq? fast slow))
				       (loop fast slow))))))))))

	(define-inline (caar x) (car (car x)))
	(define-inline (cadr x) (car (cdr x)))
	(define-inline (cdar x) (cdr (car x)))
	(define-inline (cddr x) (cdr (cdr x)))
	(define (caaar x) (car (car (car x))))
	(define (caadr x) (car (car (cdr x))))
	(define (cadar x) (car (cdr (car x))))
	(define (caddr x) (car (cdr (cdr x))))
	(define (cdaar x) (cdr (car (car x))))
	(define (cdadr x) (cdr (car (cdr x))))
	(define (cddar x) (cdr (cdr (car x))))
	(define (cdddr x) (cdr (cdr (cdr x))))
	(define (caaaar x) (car (car (car (car x)))))
	(define (caaadr x) (car (car (car (cdr x)))))
	(define (caadar x) (car (car (cdr (car x)))))
	(define (caaddr x) (car (car (cdr (cdr x)))))
	(define (cadaar x) (car (cdr (car (car x)))))
	(define (cadadr x) (car (car (car (cdr x)))))
	(define (caddar x) (car (car (cdr (car x)))))
	(define (cadddr x) (car (car (cdr (cdr x)))))
	(define (cdaaar x) (cdr (car (car (car x)))))
	(define (cdaadr x) (cdr (car (car (cdr x)))))
	(define (cdadar x) (cdr (car (cdr (car x)))))
	(define (cdaddr x) (cdr (car (cdr (cdr x)))))
	(define (cddaar x) (cdr (cdr (car (car x)))))
	(define (cddadr x) (cdr (cdr (car (cdr x)))))
	(define (cdddar x) (cdr (cdr (cdr (car x)))))
	(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

	(define-inline (length lst) (%inline "SPOCK.length" lst))

	(define (append . lsts)
	  (if (null? lsts)
	      '()
	      (let loop ((lsts lsts))
		(if (null? (%cdr lsts))
		    (%car lsts)
		    (let copy ((node (%car lsts)))
		      (if (pair? node)
			  (cons (%car node) (copy (%cdr node)))
			  ;; ignores non-list node
			  (loop (%cdr lsts))))))))

	(define (reverse lst)
	  (let loop ((lst lst) (rest '()))
	    (if (pair? lst)
		(loop (%cdr lst) (cons (%car lst) rest))
		;; ignores non-list node
		rest)))

	(define (list-tail lst i)
	  (let loop ((i (%check "number" i))
		     (lst lst))
	    (if (%inline (1 " <= 0") i)
		lst
		(loop (%inline (1 " - 1") i)
		      (%cdr (%check ("SPOCK.Pair") lst))))))

	(define list-ref
	  (let ((list-tail list-tail))
	    (lambda (lst i)
	      (%car (%check ("SPOCK.Pair") (list-tail lst i))))))

	(define memq
	  (%native-lambda
	   "var x = arguments[ 1 ];"
	   "for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {"
	   "  if(n.car === x) return K(n);"
	   "}"
	   "return K(false);"))

	(define memv
	  (%native-lambda
	   "var x = arguments[ 1 ];"
	   "for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {"
	   "  if(SPOCK.eqvp(n.car, x)) return K(n);"
	   "}"
	   "return K(false);"))

	(define member
	  (%native-lambda
	   "var x = arguments[ 1 ];"
	   "for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {"
	   "  if(SPOCK.equalp(n.car, x)) return K(n);"
	   "}"
	   "return K(false);"))

	(define assq
	  (%native-lambda
	   "var x = arguments[ 1 ];"
	   "for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {"
	   "  var p = n.car;"
	   "  if(p instanceof SPOCK.Pair && p.car === x) return K(p);"
	   "}"
	   "return K(false);"))

	(define assv
	  (%native-lambda
	   "var x = arguments[ 1 ];"
	   "for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {"
	   "  var p = n.car;"
	   "  if(p instanceof SPOCK.Pair && SPOCK.eqvp(p.car, x)) return K(p);"
	   "}"
	   "return K(false);"))

	(define assoc
	  (%native-lambda
	   "var x = arguments[ 1 ];"
	   "for(var n = arguments[ 2 ]; n instanceof SPOCK.Pair; n = n.cdr) {"
	   "  var p = n.car;"
	   "  if(p instanceof SPOCK.Pair && SPOCK.equalp(p.car, x)) return K(p);"
	   "}"
	   "return K(false);"))

	))


    (define-library-section numeric-predicates

      (default
	
	(define-inline (zero? x) (eq? 0 (%check "number" x)))
	(define-inline (positive? x) (%inline (1 ">" 2) (%check "number" x) 0))
	(define-inline (negative? x) (%inline (1 "<" 2) (%check "number" x) 0))
	(define-inline (odd? x) (not (eq? 0 (%inline (1 "%" 2) (%check "number" x) 2))))
	(define-inline (even? x) (eq? 0 (%inline (1 "%" 2) (%check "number" x) 2)))
	(define-inline (complex? x) (eq? (%inline "typeof" x) "number"))
	(define-inline (rational? x) (eq? (%inline "typeof" x) "number"))
	(define-inline (real? x) (eq? (%inline "typeof" x) "number"))

	(define-inline (integer? x) 
	  (and (eq? (%inline "typeof" x) "number")
	       (eq? x (%inline "Math.round" x) x)))

	(define-inline (exact? x)
	  (let ((x (%check "number" x)))
	    (eq? x (%inline "Math.round" x) x)))
	
	(define-inline (inexact? x) (not (exact? x)))
	
	))


    (define-library-section native-basic-arithmetic

      (debug

	(define %+
	  (%native-lambda
	   "var len = arguments.length;"
	   "switch(len) {"
	   "case 1: return K(0);"
	   "case 2: return K(SPOCK.check(arguments[ 1 ], 'number', '+'));"
	   "default:"
	   " var p = SPOCK.check(arguments[ 1 ], 'number', '+');"
	   " for(var i = 2; i < len; ++i) {"
	   "  p += SPOCK.check(arguments[ i ], 'number', '+');"
	   " }"
	   " return K(p);}"))

	(define %-
	  (%native-lambda
	   "var len = arguments.length;"
	   "switch(len) {"
	   "case 1: SPOCK.error('(-) bad argument count', len);"
	   "case 2: return K(-SPOCK.check(arguments[ 1 ], 'number', '-'));"
	   "default:"
	   " var p = SPOCK.check(arguments[ 1 ], 'number', '-');"
	   " for(var i = 2; i < len; ++i) {"
	   "  p -= SPOCK.check(arguments[ i ], 'number', '-');"
	   " }"
	   " return K(p);}"))

	(define %*
	  (%native-lambda
	   "var len = arguments.length;"
	   "switch(len) {"
	   "case 1: return K(1);"
	   "case 2: return K(SPOCK.check(arguments[ 1 ], 'number', '*'));"
	   "default:"
	   " var p = SPOCK.check(arguments[ 1 ], 'number', '*');"
	   " for(var i = 2; i < len; ++i) {"
	   "  p *= SPOCK.check(arguments[ i ], 'number', '*');"
	   " }"
	   " return K(p);}"))

	(define %/
	  (%native-lambda
	   "var len = arguments.length;"
	   "switch(len) {"
	   "case 1: SPOCK.error('(/) bad argument count', len);"
	   "case 2: return K(1/SPOCK.check(arguments[ 1 ], 'number', '/'));"
	   "default:"
	   " var p = SPOCK.check(arguments[ 1 ], 'number', '/');"
	   " for(var i = 2; i < len; ++i) {"
	   "  p /= SPOCK.check(arguments[ i ], 'number', '/');"
	   " }"
	   " return K(p);}"))

	)

      (default

       (define %+
	 (%native-lambda
	  "var len = arguments.length;"
	  "switch(len) {"
	  "case 1: return K(0);"
	  "case 2: return K(arguments[ 1 ]);"
	  "default:"
	  " var p = arguments[ 1 ];"
	  " for(var i = 2; i < len; ++i) {"
	  "  p += arguments[ i ];"
	  " }"
	  " return K(p);}"))

       (define %-
	 (%native-lambda
	  "var len = arguments.length;"
	  "switch(len) {"
	  "case 2: return K(-arguments[ 1 ]);"
	  "default:"
	  " var p = arguments[ 1 ];"
	  " for(var i = 2; i < len; ++i) {"
	  "  p -= arguments[ i ];"
	  " }"
	  " return K(p);}"))

       (define %*
	 (%native-lambda
	  "var len = arguments.length;"
	  "switch(len) {"
	  "case 1: return K(1);"
	  "case 2: return K(arguments[ 1 ]);"
	  "default:"
	  " var p = arguments[ 1 ];"
	  " for(var i = 2; i < len; ++i) {"
	  "  p *= arguments[ i ];"
	  " }"
	  " return K(p);}"))

       (define %/
	 (%native-lambda
	  "var len = arguments.length;"
	  "switch(len) {"
	  "case 2: return K(1/arguments[ 1 ]);"
	  "default:"
	  " var p = arguments[ 1 ];"
	  " for(var i = 2; i < len; ++i) {"
	  "  p /= arguments[ i ];"
	  " }"
	  " return K(p);}"))

	))


    (define-library-section basic-arithmetic

      (default

	(define-syntax +
	  (case-lambda
	   (() 0)
	   ((n) (%check "number" n))
	   ((n1 n2)
	    (%inline (1 " + " 2) (%check "number" n1) (%check "number" n2)))
	   %+))

	(define-syntax *
	  (case-lambda
	   (() 1)
	   ((n) (%check "number" n))
	   ((n1 n2)
	    (%inline (1 " * " 2) (%check "number" n1) (%check "number" n2)))
	   %*))

	(define-syntax -
	  (case-lambda
	   ((n) (%inline ("-" 1) (%check number n)))
	   ((n1 n2) 
	    (%inline (1 " - " 2) (%check number n1) (%check number n2)))
	   %-))

	(define-syntax /
	  (case-lambda
	   ((n) (%inline ("1 / " 1) (%check number n)))
	   ((n1 n2) 
	    (%inline (1 " / " 2) (%check number n1) (%check number n2)))
	   %/))

	))


    (define-library-section native-numeric-comparison

      ;;XXX need non-debug versions
      (default

	(define %=
	  (%native-lambda
	   "var argc = arguments.length;"
	   "var last = SPOCK.check(arguments[ 1 ], 'number', '=');"
	   "for(var i = 2; i < argc; ++i) {"
	   " var x = SPOCK.check(arguments[ i ], 'number', '=');"
	   " if(last !== x) return K(false);"
	   " else last = x;}"
	   "return K(true);"))

	(define %>
	  (%native-lambda
	   "var argc = arguments.length;"
	   "var last = SPOCK.check(arguments[ 1 ], 'number', '>');"
	   "for(var i = 2; i < argc; ++i) {"
	   " var x = SPOCK.check(arguments[ i ], 'number', '>');"
	   " if(last <= x) return K(false);"
	   " else last = x;}"
	   "return K(true);"))

	(define %<
	  (%native-lambda
	   "var argc = arguments.length;"
	   "var last = SPOCK.check(arguments[ 1 ], 'number', '<');"
	   "for(var i = 2; i < argc; ++i) {"
	   " var x = SPOCK.check(arguments[ i ], 'number', '<');"
	   " if(last >= x) return K(false);"
	   " else last = x;}"
	   "return K(true);"))

	(define %>=
	  (%native-lambda
	   "var argc = arguments.length;"
	   "var last = SPOCK.check(arguments[ 1 ], 'number', '>=');"
	   "for(var i = 2; i < argc; ++i) {"
	   " var x = SPOCK.check(arguments[ i ], 'number', '>=');"
	   " if(last < x) return K(false);"
	   " else last = x;}"
	   "return K(true);"))

	(define %<=
	  (%native-lambda
	   "var argc = arguments.length;"
	   "var last = SPOCK.check(arguments[ 1 ], 'number', '<=');"
	   "for(var i = 2; i < argc; ++i) {"
	   " var x = SPOCK.check(arguments[ i ], 'number', '<=');"
	   " if(last > x) return K(false);"
	   " else last = x;}"
	   "return K(true);"))

	))


    (define-library-section numeric-comparison

      (default

	(define-syntax =
	  (case-lambda
	   ((n1 n2)
	    (%inline (1 " === " 2) (%check "number" n1) (%check "number" n2)))
	   %=))

	(define-syntax >
	  (case-lambda
	   ((n1 n2)
	    (%inline (1 " > " 2) (%check "number" n1) (%check "number" n2)))
	   %>))

	(define-syntax <
	  (case-lambda
	   ((n1 n2)
	    (%inline (1 " < " 2) (%check "number" n1) (%check "number" n2)))
	   %<))

	(define-syntax >=
	  (case-lambda
	   ((n1 n2)
	    (%inline (1 " >= " 2) (%check "number" n1) (%check "number" n2)))
	   %>=))

	(define-syntax <=
	  (case-lambda
	   ((n1 n2)
	    (%inline (1 " <= " 2) (%check "number" n1) (%check "number" n2)))
	   %<=))

	))


    (define-library-section native-numeric-operations

      (debug

	(define %max
	  (%native-lambda
	   "var argc = arguments.length;"
	   "var n = SPOCK.check(arguments[ 1 ], 'number', 'max');"
	   "for(var i = 2; i < argc; ++i) {"
	   " var x = SPOCK.check(arguments[ i ], 'number', 'max');"
	   " if(n < x) n = x;}"
	   "return K(n);"))

	(define %min
	  (%native-lambda
	   "var argc = arguments.length;"
	   "var n = SPOCK.check(arguments[ 1 ], 'number', 'max');"
	   "for(var i = 2; i < argc; ++i) {"
	   " var x = SPOCK.check(arguments[ i ], 'number', 'max');"
	   " if(n > x) n = x;}"
	   "return K(n);"))

	)

      (default

       (define %max
	 (%native-lambda
	  "return K(Math.max.apply(this, arguments));"))

       (define %max
	 (%native-lambda
	  "return K(Math.min.apply(this, arguments));"))

	))


    (define-library-section numeric-operations

      (default

	(define-inline (round n) (%inline "Math.round" (%check "number" n)))
	(define-inline (floor n) (%inline "Math.floor" (%check "number" n)))
	(define-inline (ceiling n) (%inline "Math.ceil" (%check "number" n)))

	(define-inline (truncate n)
	  (%check "number" n)
	  (if (%inline (1 " < 0") n)
	      (%inline "Math.ceil" n)
	      (%inline "Math.floor" n)))

	(define-inline (log n) (%inline "Math.log" (%check "number" n)))
	(define-inline (abs n) (%inline "Math.abs" (%check "number" n)))
	(define-inline (sin n) (%inline "Math.sin" (%check "number" n)))
	(define-inline (cos n) (%inline "Math.cos" (%check "number" n)))
	(define-inline (tan n) (%inline "Math.tan" (%check "number" n)))
	(define-inline (asin n) (%inline "Math.asin" (%check "number" n)))
	(define-inline (acos n) (%inline "Math.acos" (%check "number" n)))
	(define-inline (sqrt n) (%inline "Math.sqrt" (%check "number" n)))

	(define-inline (expt n m)
	  (%inline "Math.pow" (%check "number" n) (%check "number" m)))

	(define-inline (atan y x)
	  (if (void? x)
	      (%inline "Math.atan" (%check "number" y))
	      (%inline "Math.atan2" (%check "number" y) (%check "number" x))))

	(define-syntax max
	  (case-lambda
	   ((n) (%check "number" n))
	   ((n1 n2)
	    (%inline "Math.max" (%check "number" n1) (%check "number" n2)))
	   %max))

	(define-syntax min
	  (case-lambda
	   ((n) (%check "number" n))
	   ((n1 n2)
	    (%inline "Math.min" (%check "number" n1) (%check "number" n2)))
	   %min))

	(define-inline (quotient x y)
	  (truncate (/ x y)))		;XXX correct?

	(define-inline (remainder x y)
	  (- x (* (quotient x y) y)))

	(define (modulo a b) ; copied from chibi scheme without asking Alex
	  (let ((res (remainder a b)))
	    (if (< b 0)
		(if (<= res 0) res (+ res b))
		(if (>= res 0) res (+ res b)))))

	(define-inline (exact->inexact n) (%check "number" n))
	(define-inline (inexact->exact n) (truncate n))

	;; not implemented: numerator denominator rationalize
	;; not implemented: make-rectangular make-polar imag-part real-part magnitude angle

	))


    (define-library-section gcd-and-lcm

      (default

	;;XXX slow

	(define %gcd
	  (let ((remainder remainder))
	    (lambda (x y)
	      (let loop ((x x) (y y))
		(if (zero? y)
		    (abs x)
		    (loop y (remainder x y)) ) ) ) ) )

	(define (gcd . ns)
	  (if (null? ns)
	      0
	      (let loop ((ns ns) (f #t))
		(let ((head (%car ns))
		      (next (%cdr ns)))
		  (when f (%check "number" head))
		  (if (null? next)
		      (abs head)
		      (let ((n2 (%car next)))
			(%check "number" n2)
			(loop 
			 (cons (%gcd head n2) (%cdr next))
			 #f) ) ) ) ) ) )

	(define (%lcm x y)
	  (quotient (* x y) (%gcd x y)) )

	(define (lcm . ns)
	  (if (null? ns)
	      1
	      (let loop ((ns ns) (f #t))
		(let ((head (%car ns))
		      (next (%cdr ns)))
		  (when f (%check "number" head))
		  (if (null? next)
		      (abs head)
		      (let ((n2 (%car next)))
			(%check "number" n2)
			(loop
			 (cons (%lcm head n2) (%cdr next))
			 #f) ) ) ) ) ) )

	))


    (define-library-section characters

      (default

	(define-inline (char->integer c)
	  (%inline ".charCodeAt" (%property-ref "character" (%check ("SPOCK.Char") c)) 0))

	(define-inline (integer->char c)
	  (%inline "new SPOCK.Char" (%inline "String.fromCharCode" (%check "number" c))))

	(define-inline (char=? x y)
	  (eq? (%property-ref "character" (%check ("SPOCK.Char") x))
	       (%property-ref "character" (%check ("SPOCK.Char") y))))

	(define-inline (char>? x y)
	  (%inline
	   (1 " > " 2) 
	   (%property-ref "character" (%check ("SPOCK.Char") x))
	   (%property-ref "character" (%check ("SPOCK.Char") y))))

	(define-inline (char<? x y)
	  (%inline
	   (1 " < " 2) 
	   (%property-ref "character" (%check ("SPOCK.Char") x))
	   (%property-ref "character" (%check ("SPOCK.Char") y))))

	(define-inline (char>=? x y)
	  (%inline
	   (1 " >= " 2) 
	   (%property-ref "character" (%check ("SPOCK.Char") x))
	   (%property-ref "character" (%check ("SPOCK.Char") y))))

	(define-inline (char<=? x y)
	  (%inline
	   (1 " <= " 2) 
	   (%property-ref "character" (%check ("SPOCK.Char") x))
	   (%property-ref "character" (%check ("SPOCK.Char") y))))

	(define-inline (char-ci=? x y)
	  (eq? (%inline ".toLowerCase" (%property-ref "character" (%check ("SPOCK.Char") x)))
	       (%inline ".toLowerCase" (%property-ref "character" (%check ("SPOCK.Char") y)))))

	(define-inline (char-ci>? x y)
	  (%inline
	   (1 " > " 2)
	   (%inline ".toLowerCase" (%property-ref "character" (%check ("SPOCK.Char") x)))
	   (%inline ".toLowerCase" (%property-ref "character" (%check ("SPOCK.Char") y)))))

	(define-inline (char-ci<? x y)
	  (%inline
	   (1 " < " 2)
	   (%inline ".toLowerCase" (%property-ref "character" (%check ("SPOCK.Char") x)))
	   (%inline ".toLowerCase" (%property-ref "character" (%check ("SPOCK.Char") y)))))

	(define-inline (char-ci>=? x y)
	  (%inline
	   (1 " >= " 2)
	   (%inline ".toLowerCase" (%property-ref "character" (%check ("SPOCK.Char") x)))
	   (%inline ".toLowerCase" (%property-ref "character" (%check ("SPOCK.Char") y)))))

	(define-inline (char-ci<=? x y)
	  (%inline
	   (1 " <= " 2)
	   (%inline ".toLowerCase" (%property-ref "character" (%check ("SPOCK.Char") x)))
	   (%inline ".toLowerCase" (%property-ref "character" (%check ("SPOCK.Char") y)))))

	(define-inline (char-upcase c)
	  (%inline 
	   "new SPOCK.Char"
	   (%inline 
	    ".toUpperCase"
	    (%property-ref "character" (%check ("SPOCK.Char") c)))))

	(define-inline (char-downcase c)
	  (%inline 
	   "new SPOCK.Char"
	   (%inline 
	    ".toLowerCase"
	    (%property-ref "character" (%check ("SPOCK.Char") c)))))

	(define-inline (char-alphabetic? c)	;XXX not unicode aware
	  (not 
	   (null? 
	    (%inline 
	     (1 ".character.match(/^[A-Za-z]$/)") 
	     (%check ("SPOCK.Char") c)))))

	(define-inline (char-numeric? c)	;XXX not unicode aware?
	  (not (null? (%inline (1 ".character.match(/^\\d$/)") (%check ("SPOCK.Char") c)))))

	(define-inline (char-whitespace? c)
	  (not (null? (%inline (1 ".character.match(/^\\s$/)") (%check ("SPOCK.Char") c)))))

	(define-inline (char-upper-case? c)	;XXX not unicode aware
	  (not 
	   (null? 
	    (%inline 
	     (1 ".character.match(/^[A-Z]$/)")
	     (%check ("SPOCK.Char") c)))))

	(define-inline (char-lower-case? c)	;XXX not unicode aware
	  (not
	   (null?
	    (%inline
	     (1 ".character.match(/^[a-z]$/)")
	     (%check ("SPOCK.Char") c)))))

	))


    (define-library-section symbols

      (default
	
	(define-inline (symbol->string sym)
	  (%property-ref "name" (%check ("SPOCK.Symbol") sym)))

	(define string->symbol
	  (%native-lambda
	   "var str = SPOCK.jstring(arguments[ 1 ]);"
	   "return K(SPOCK.intern(str));"))

	))


    (define-library-section property-lists

      (default
	
	(define (get sym prop)
	  (let ((val
		 (%inline
		  (1 ".plist[" 2 "]") 
		  (%check ("SPOCK.Symbol") sym)
		  (%property-ref "name" (%check ("SPOCK.Symbol") prop)))))
	    (and (not (void? val)) val))) ;XXX doesn't allow storing void

	(define (put! sym prop val)
	  (%inline
	   (1 ".plist[" 2 "] = " 3)
	   (%check ("SPOCK.Symbol") sym)
	   (%property-ref "name" (%check ("SPOCK.Symbol") prop))
	   val))

	))


    (define-library-section strings

      (default

	(define-inline (string-length str)
	  (%property-ref "length" (%string->jstring str)))

	(define string-append
	  (%native-lambda
	   "var args = Array.prototype.slice.call(arguments, 1);"
	   "var strs = SPOCK.map(function(x) { return SPOCK.jstring(x); }, args);"
	   "return K(new SPOCK.String(strs));"))

	;;XXX does no bounds/exactness check
	(define-inline (substring str i j)
	  (let ((str (%string->jstring str)))
	    (%inline 
	     ".substring" str
	     (%check "number" i)
	     (if (void? j)
		 (%property-ref "length" str)
		 (%check "number" j)))))

	;;XXX we need non-debug versions of all of these

	(define string
	  (%native-lambda
	   "var str = [];"
	   "var len = arguments.length - 1;"
	   "for(var i = 1; i <= len; ++i) {"
	   " var x = arguments[ i ];"
	   " if(x instanceof SPOCK.Char) str.push(x.character);"
	   " else SPOCK.error('bad argument type - not a character', x);}"
	   "return K(new SPOCK.String(str.join('')));"))

	(define string->list
	  (%native-lambda
	   "var str = SPOCK.jstring(arguments[ 1 ]);"
	   "var lst = null;"
	   "var len = str.length;"
	   "for(var i = len - 1; i >= 0; --i)"
	   " lst = new SPOCK.Pair(new SPOCK.Char(str.charAt(i)), lst);"
	   "return K(lst);"))

	(define list->string
	  (%native-lambda
	   "var lst = arguments[ 1 ];"
	   "var str = [];"
	   "while(lst instanceof SPOCK.Pair) {"
	   " str.push(SPOCK.check(lst.car, SPOCK.Char).character);"
	   " lst = lst.cdr;}"
	   "return K(new SPOCK.String(str.join('')));"))

	(define make-string
	  (%native-lambda
	   "var n = SPOCK.check(arguments[ 1 ], 'number', 'make-string');"
	   "var c = arguments[ 2 ];"
	   "var a = new Array(n);"
	   "if(c !== undefined)"
	   " c = SPOCK.check(c, SPOCK.Char, 'make-string').character;"
	   "else c = ' ';"
	   "for(var i = 0; i < n; ++i) a[ i ] = c;"
	   "return K(new SPOCK.String(a.join('')));"))

	;;XXX no bounds/exactness checks
	(define string-ref     ;XXX consider inlining the fast case
	  (%native-lambda
	   "var str = arguments[ 1 ];"
	   "var i = SPOCK.check(arguments[ 2 ], 'number', 'string-ref');"
	   "if(typeof str === 'string')"
	   " return K(new SPOCK.Char(str.charAt(i)));"
	   "else if(str instanceof SPOCK.String) {"
	   " var parts = str.parts;"
	   " for(var p in parts) {"
	   "  var l = parts[ p ].length;"
	   "  if(i <= l) return K(new SPOCK.Char(parts[ p ].charAt(i)));"
	   "  else i -= l;}"
	   " SPOCK.error('`string-ref\\\' out of range', str, i);}"))

	(define string-set!
	  (%native-lambda
	   "var str = arguments[ 1 ];"
	   "var i = SPOCK.check(arguments[ 2 ], 'number', 'string-set!');"
	   "var c = SPOCK.check(arguments[ 3 ], SPOCK.Char, 'string-set!');"
	   "if(typeof str === 'string')"
	   " SPOCK.error('argument to `string-set!\\\' is not a mutable string', str);"
	   "else if(str instanceof SPOCK.String) {"
	   " var parts = str.parts;"
	   " for(var p in parts) {"
	   "  var part = parts[ p ];"
	   "  var l = part.length;"
	   "  if(i <= l) {"
	   "   parts[ p ] = part.substring(0, i) + c.character + part.substring(i + 1);"
	   "   return K(undefined);"
	   "  } else i -= l;}"
	   " SPOCK.error('`string-set!\\\' out of range', str, i);}"))

	(define-inline (string=? s1 s2) 
	  (eq? (%string->jstring s1) (%string->jstring s2))) ;XXX may cons a lot

	(define-inline (string>? s1 s2)
	  (%inline (1 " > " 2) (%string->jstring s1) (%string->jstring s2)))

	(define-inline (string<? s1 s2)
	  (%inline (1 " < " 2) (%string->jstring s1) (%string->jstring s2)))

	(define-inline (string>=? s1 s2)
	  (%inline (1 " >= " 2) (%string->jstring s1) (%string->jstring s2)))

	(define-inline (string<=? s1 s2)
	  (%inline (1 " <= " 2) (%string->jstring s1) (%string->jstring s2)))

	(define-inline (string-ci=? s1 s2) ;XXX ugly
	  (eq?
	   (%inline ".toLowerCase" (%string->jstring s1))
	   (%inline ".toLowerCase" (%string->jstring s2))))

	(define-inline (string-ci>? s1 s2)
	  (%inline 
	   (1 " > " 2)
	   (%inline ".toLowerCase" (%string->jstring s1))
	   (%inline ".toLowerCase" (%string->jstring s2))))

	(define-inline (string-ci<? s1 s2)
	  (%inline 
	   (1 " < " 2)
	   (%inline ".toLowerCase" (%string->jstring s1))
	   (%inline ".toLowerCase" (%string->jstring s2))))

	(define-inline (string-ci>=? s1 s2)
	  (%inline 
	   (1 " >= " 2)
	   (%inline ".toLowerCase" (%string->jstring s1))
	   (%inline ".toLowerCase" (%string->jstring s2))))

	(define-inline (string-ci<=? s1 s2)
	  (%inline 
	   (1 " <= " 2)
	   (%inline ".toLowerCase" (%string->jstring s1))
	   (%inline ".toLowerCase" (%string->jstring s2))))

	(define (string-copy str from to)
	  (let* ((str (%string->jstring str))
		 (from (if (void? from) 0 (%check "number" from)))
		 (to (if (void? to) (%property-ref "length" str) (%check "number" to))))
	    (%jstring->string (%inline ".slice" str from to))))

	(define (string-fill! str char from to)
	  (unless (%check (%inline (1 "instanceof SPOCK.String") str))
	    (%error "bad argument type - not a mutable string" str))
	  (let* ((text (%inline ".normalize" str))
		 (char (%check ("SPOCK.Char") char))
		 (from (if (void? from) 0 (%check "number" from)))
		 (to (if (void? to) (%property-ref "length" text) (%check "number" to))))
	    ((%native-lambda
	      "var str = arguments[ 1 ];"
	      "var from = arguments[ 2 ];"
	      "var to = arguments[ 3 ];"
	      "var c = arguments[ 4 ];"
	      "var snew = new Array(to - from);"
	      "for(var i in snew) snew[ i ] = c;"
	      "str.parts = [str.parts[ 0 ].substring(0, from), snew.join(''),"
	      " str.parts[ 0 ].substring(to)];"
	      "return K(str);")
	     str from to char)))

	))


    (define-library-section vectors

      ;;XXX add non-debug variants

      (default

	(define-inline (vector-length v)
	  (%property-ref "length" (%check ("Array") v)))

	;;XXX make these two safe (bounds-checking and exactness)
	(define-inline (vector-ref v i)
	  (%inline (1 "[" 2 "]") (%check ("Array") v) (%check "number" i)))

	(define-inline (vector-set! v i x)
	  (%inline (1 "[" 2 "] = " 3) (%check ("Array") v) (%check "number" i) x))

	(define vector
	  (%native-lambda
	   "return K(Array.prototype.slice.call(arguments, 1));"))

	(define make-vector
	  (%native-lambda
	   "var n = SPOCK.check(arguments[ 1 ], 'number', 'make-vector');"
	   "var x = arguments[ 2 ];"
	   "var a = new Array(n);"
	   "if(x !== undefined) {"
	   " for(var i = 0; i < n; ++i) a[ i ] = x;}"
	   "return K(a);"))

	(define vector->list
	  (%native-lambda
	   "var vec = SPOCK.check(arguments[ 1 ], Array, 'vector->list');"
	   "var lst = null;"
	   "var len = vec.length;"
	   "for(var i = len - 1; i >= 0; --i)"
	   " lst = new SPOCK.Pair(vec[ i ], lst);"
	   "return K(lst);"))

	(define list->vector
	  (%native-lambda
	   "var lst = arguments[ 1 ];"
	   "var vec = [];"
	   "while(lst instanceof SPOCK.Pair) {"
	   " vec.push(lst.car);"
	   " lst = lst.cdr;}"
	   "return K(vec);"))

	(define vector-fill!
	  (%native-lambda
	   "var vec = SPOCK.check(arguments[ 1 ], Array, 'vector-fill!');"
	   "var x = arguments[ 2 ];"
	   "var from = arguments[ 3 ];"
	   "var to = arguments[ 4 ];"
	   "if(from === undefined) from = 0;"
	   "if(to === undefined) to = vec.length;"
	   "for(var i = from; i < to; ++i)"
	   " vec[ i ] = x;"
	   "return K(undefined);"))

	))


    (define-library-section number-string-conversion

      (default

	(define-inline (number->string num base)
	  (%inline 
	   "new SPOCK.String"
	   (%inline
	    ".toString" 
	    (%check "number" num) 
	    (if (void? base) 
		10
		(%check "number" base)))))

	;;XXX add non-debug version?
	(define string->number
	  (%native-lambda
	   "var str = SPOCK.jstring(arguments[ 1 ]);"
	   "var base = arguments[ 2 ];"
	   "if(!base) base = 10;"
	   "else base = SPOCK.check(base, 'number', 'string->number');"
	   "var m = true, neg = 1;"
	   "while(m) {"
	   " m = str.match(/^#[eboxid]/);"
	   " if(m) {"
	   "  switch(str[ 1 ]) {"
	   "  case 'e':"
	   "  case 'i': break;"
	   "  case 'd': base = 10; break;"
	   "  case 'o': base = 8; break;"
	   "  case 'x': base = 16; break;"
	   "  case 'b': base = 2; break;"
	   "  default: return K(false);}"
	   "  str = str.substring(2);}}"
	   "switch(str[ 0 ]) {"
	   "case '-': neg = -1; str = str.substring(1); break;"
	   "case '+': str = str.substring(1);}"
	   "var num, den = false;"
	   "if((m = str.match(/^([^\\/]+)\\/(.+)$/))) {"
	   "  str = m[ 1 ];"
	   "  den = m[ 2 ];}"
	   "function num3(s) {"
	   " var tr = null;"
	   " switch(base) {"
	   " case 2: tr = /^[0-1]+$/; break;"
	   " case 8: tr = /^[0-7]+$/; break;"
	   " case 10: tr = /^[#0-9]*\\.?[#0-9]+([esdfl][0-9]+)?$/; break;"
	   " case 16: tr = /^[0-9a-fA-F]+$/;}"
	   " if(tr && !s.match(tr)) return false;"
	   " var s2 = s.replace(/#/g, '0');"
	   " if(base === 10) s2 = parseFloat(s2.replace(/[esdfl]/g, 'e'));"
	   " else if(s2 !== s) return false;"
	   " else s2 = parseInt(s2, base);"
	   " return isNaN(s2) ? false : s2;}"
	   "if(!(num = num3(str))) return K(false);"
	   "if(den && !(den = num3(den))) return K(false);"
	   "return K(neg * num / (den || 1));"))

	))


    (define-library-section unsafe-internal-i/o

      (default

	;; (%show STRING PORT)
	(define %show
	  (%native-lambda
	   "arguments[ 2 ].write(arguments[ 1 ]);"
	   "return K(undefined);"))

	;; (%fetch N PORT)
	(define %fetch
	  (%native-lambda
	   "return K(arguments[ 2 ].read(arguments[ 1 ]));"))

	))


    (define-library-section port-checks

      (debug

	;; (%check-port X DIR LOC)
	(define %check-port
	  (%native-lambda
	   "var port = arguments[ 1 ];"
	   "var dir = arguments[ 2 ];"
	   "if(port instanceof SPOCK.Port) {"
	   " if(port.closed)"
	   "  SPOCK.error('port is already closed', port);"
	   " else if(port.direction !== dir)"
	   "  SPOCK.error('bad argument type - not an ' + dir + ' port', port, arguments[ 3 ]);"
	   "}"
	   "else SPOCK.error('bad argument type - not a port', port, arguments[ 3 ]);"
	   "return K(port);"))
	)
      
      (default

       (define-inline (%check-port x dir loc) x)

	))


    (define-library-section basic-i/o

      (default

	(define-inline (current-input-port) (%host-ref "SPOCK.stdin"))
	(define-inline (current-output-port) (%host-ref "SPOCK.stdout"))

	(define (newline port)
	  (%show 
	   "\n" 
	   (if (void? port) 
	       (%host-ref "SPOCK.stdout")
	       (%check-port port "output" "newline"))))

	(define (read-char port)
	  (let ((s (%fetch
		    1
		    (if (void? port)
			(%host-ref "SPOCK.stdin")
			(%check-port port "input" "read-char")))))
	    (if (eof-object? s)
		s
		(%inline "new SPOCK.Char" s))))

	(define (write-char chr port)
	  (%show
	   (%property-ref "character" (%check ("SPOCK.Char") chr))
	   (if (void? port)
	       (%host-ref "SPOCK.stdout")
	       (%check-port port "output" "write-char"))))

	(define peek-char
	  (let ((read-char read-char))
	    (lambda (port)
	      (let ((c (read-char port)))
		(unless (eof-object? c)
		  (%inline (1 ".peeked = " 2) port (%property-ref "character" c)))
		c))))

	(define (char-ready? port)
	  (%check-port port "input" "char-ready?")
	  (%inline ".ready" port))

	))


    (define-library-section data-output

      (default

	;; (%print-hook X PORT READABLE?)         called for unknown object
	(define (%print-hook x port readable)
	  (%show "#<unknown object>" port))

	(define (display x port)
	  (let ((port (if (void? port)
			  (%host-ref "SPOCK.stdout")
			  (%check-port port "output" "display"))))
	    (let show ((x x))
	      (cond ((null? x) (%show "()" port))
		    ((number? x)
		     ;;XXX this could be optimized
		     (%show (%string->jstring (number->string x)) port))
		    ((string? x)
		     (%show (%inline "SPOCK.jstring" x) port))
		    ((symbol? x)
		     (%show (%property-ref "name" x) port))
		    ((char? x)
		     (%show (%property-ref "character" x) port))
		    ((eof-object? x) (%show "#<eof>" port))
		    ((procedure? x) (%show "#<procedure>" port))
		    ((boolean? x) (%show (if x "#t" "#f") port))
		    ((pair? x)
		     (%show "(" port)
		     (let loop ((y x))
		       (cond ((null? y) (%show ")" port))
			     ((not (pair? y))
			      (%show " . " port)
			      (show y)
			      (%show ")" port))
			     (else
			      (unless (eq? x y) (%show " " port))
			      (show (%car y))
			      (loop (cdr y))))))
		    ((void? x) (%show "#<undefined>" port))
		    ((vector? x)
		     (let ((len (%property-ref "length" x)))
		       (%show "#(" port)
		       (do ((i 0 (%inline ("1+" 1) i)))
			   ((%inline (1 ">=" 2) i len)
			    (%show ")" port))
			 (unless (eq? i 0) (%show " " port))
			 (show (%inline (1 "[" 2 "]") x i)))))
		    ((%inline (1 "instanceof SPOCK.Port") x)
		     (%show (%inline "SPOCK.stringify" x) port))
		    ((%inline (1 "instanceof SPOCK.Promise") x)
		     (%show "#<promise>" port))
		    ((eq? "object" (%inline "typeof" x))
		     (%print-hook x port #f))
		    (else (%show "#<unknown object>" port))))))

	(define write
	  (let ((display display))
	    (define escape 
	      (%native-lambda 
	       "var str = arguments[ 1 ];"
	       "var a = [];"
	       "var len = str.length;"
	       "for(var i = 0; i < len; ++i) {"
	       " var c = str.charAt(i);"
	       " switch(c) {"
	       " case '\\n': a.push('\\n'); break;"
	       " case '\\t': a.push('\\t'); break;"
	       " case '\\r': a.push('\\r'); break;"
	       " case '\\\"': a.push('\\\"'); break;"
	       " case '\\\\': a.push('\\\\'); break;"
	       " default: a.push(c);}}"
	       "return K(a.join(''));"))
	    (lambda (x port)
	      (let ((port (if (void? port)
			      (%host-ref "SPOCK.stdout")
			      (%check-port port "output" "write"))))
		(let show ((x x))
		  (cond ((string? x)
			 (%show "\"" port)
			 (%show (escape (%inline "SPOCK.jstring" x)) port)
			 (%show "\"" port))
			((char? x)
			 (%show "#\\" port)
			 (%show
			  (let ((c (%property-ref "character" x)))
			    (case c
			      (("\n") "newline") ; don't worry
			      (("\r") "return")
			      (("\t") "tab")
			      ((" ") "space")
			      (else c)))
			  port))
			((pair? x)
			 (%show "(" port)
			 (let loop ((y x))
			   (cond ((null? y) (%show ")" port))
				 ((not (pair? y))
				  (%show " . " port)
				  (show y)
				  (%show ")" port))
				 (else
				  (unless (eq? x y) (%show " " port))
				  (show (%car y))
				  (loop (cdr y))))))
			((vector? x)
			 (let ((len (%property-ref "length" x)))
			   (%show "#(" port)
			   (do ((i 0 (%inline ("1+" 1) i)))
			       ((%inline (1 ">=" 2) i len)
				(%show ")" port))
			     (unless (eq? i 0) (%show " " port))
			     (show (%inline (1 "[" 2 "]") x i)))))
			(else (display x port))))))))

	))


    (define-library-section extended-i/o

      (default

	(define-inline (current-error-port) (%host-ref "SPOCK.stderr"))

	))


    (define-library-section higher-order-operations

      (default

	(define apply
	  (%native-lambda
	   "var proc = arguments[ 1 ];"
	   "var argc = arguments.length;"
	   "var lst = arguments[ argc - 1 ];"
	   "var vec = [K].concat(Array.prototype.slice.call(arguments, 2, argc - 1));"
	   "if(lst instanceof Array) vec = vec.concat(lst);"
	   "else{"
	   " var len = SPOCK.length(lst);"
	   " var vec2 = new Array(len);"
	   " for(var i = 0; lst instanceof SPOCK.Pair; lst = lst.cdr)"
	   "  vec2[ i++ ] = lst.car;"
	   " vec = vec.concat(vec2);}"
	   "return proc.apply(this, vec);"))

	(define (for-each proc lst1 . lsts)
	  (if (null? lsts)
	      (if (vector? lst1)
		  (let ((len (vector-length lst1)))
		    (do ((i 0 (+ i 1)))
			((>= i len))
		      (proc (vector-ref lst1 i))))
		  (let loop ((lst lst1))
		    (when (pair? lst)
		      (proc (%car lst))
		      (loop (%cdr lst)))))
	      (let loop ((lsts (cons lst1 lsts)))
		(let ((hds (let loop2 ((lsts lsts))
			     (if (null? lsts)
				 '()
				 (let ((x (%car lsts)))
				   (and (pair? x)
					(cons (%car x) (loop2 (%cdr lsts)))))))))
		  (when hds
		    (apply proc hds)
		    (loop
		     (let loop3 ((lsts lsts))
		       (if (null? lsts)
			   '()
			   (cons (%cdr (%car lsts)) (loop3 (%cdr lsts)))))))))))

	(define (map proc lst1 . lsts)
	  (if (null? lsts)
	      (if (vector? lst1)
		  (let* ((len (vector-length lst1))
			 (rv (make-vector len)))
		    (do ((i 0 (+ i 1)))
			((>= i len) rv)
		      (vector-set! rv i (proc (vector-ref lst1 i)))))
		  (let loop ((lst lst1))
		    (if (pair? lst)
			(cons (proc (%car lst))
			      (loop (%cdr lst)))
			'())))
	      (let loop ((lsts (cons lst1 lsts)))
		(let ((hds (let loop2 ((lsts lsts))
			     (if (null? lsts)
				 '()
				 (let ((x (%car lsts)))
				   (and (pair? x)
					(cons (%car x) (loop2 (%cdr lsts)))))))))
		  (if hds
		      (cons
		       (apply proc hds)
		       (loop
			(let loop3 ((lsts lsts))
			  (if (null? lsts)
			      '()
			      (cons (%cdr (%car lsts)) (loop3 (%cdr lsts)))))))
		      '())))))

	))


    (define-library-section continuations

      (default

	(define dynamic-wind
	  (let ((call-with-values call-with-values)
		(values values))
	    (lambda (before thunk after)
	      (before)
	      (%host-set! 
	       "SPOCK.dynwinds" 
	       (cons (cons before after) (%host-ref "SPOCK.dynwinds")))
	      (%call-with-saved-values
	       thunk
	       (lambda ()
		 (%host-set! "SPOCK.dynwinds" (%cdr (%host-ref "SPOCK.dynwinds")))
		 (after))))))

	;; (%call-with-current-continuation PROC)
	;;
	;; - does not unwind
	(define %call-with-current-continuation
	  (%native-lambda
	   "var proc = arguments[ 1 ];"
	   "function cont() {"
	   " return K.apply(this, Array.prototype.slice.call(arguments, 1));}"
	   "return proc(K, cont);"))

	(define call-with-current-continuation
	  (let ()
	    (define (unwind winds n)
	      (cond ((eq? (%host-ref "SPOCK.dynwinds") winds))
		    ((< n 0)
		     (unwind (%cdr winds) (%inline (1 " + 1") n))
		     ((%car (%car winds)))
		     (%host-set! "SPOCK.dynwinds" winds))
		    (else
		     (let ((after (%cdr (%car (%host-ref "SPOCK.dynwinds")))))
		       (%host-set! "SPOCK.dynwinds" (%cdr (%host-ref "SPOCK.dynwinds")))
		       (after)
		       (unwind winds (%inline (1 " - 1") n)) ) )))
	    (lambda (proc)
	      (let ((winds (%host-ref "SPOCK.dynwinds")))
		(%call-with-current-continuation
		 (lambda (cont)
		   (proc
		    (lambda results	;XXX suboptimal
		      (let ((winds2 (%host-ref "SPOCK.dynwinds")))
			(unless (eq? winds2 winds)
			  (unwind winds (- (length winds2) (length winds))) )
			(apply cont results) ) ) ) ) ) ))))

	))


    (define-library-section suspensions

      (default

	(define (%get-context k)
	  (vector 
	   k
	   (%host-ref "SPOCK.dynwinds")
	   (%host-ref "SPOCK.stdin")
	   (%host-ref "SPOCK.stdout")
	   (%host-ref "SPOCK.stderr")))

	(define %restore-context
	  (%native-lambda
	   "var state = arguments[ 1 ];"
	   "SPOCK.dynwinds = state[ 1 ];"
	   "SPOCK.stdin = state[ 2 ];"
	   "SPOCK.stdout = state[ 3 ];"
	   "SPOCK.stderr = state[ 4 ];"
	   "return (state[ 0 ])(undefined);")) ; drops K
	
	(define (suspend proc)
	  (%call-with-current-continuation
	   (lambda (k)
	     (proc (%get-context k))
	     ((%native-lambda "throw new SPOCK.Result(undefined);")))))

	(define-inline (resume state)
	  (%restore-context state))

	))


    (define-library-section promises

      (default

	(define (%make-promise thunk)
	  (%inline
	   "new SPOCK.Promise"
	   (let ((ready #f)
		 (results #f))
	     (lambda ()
	       ;;XXX this can possibly be optimized
	       (if ready
		   (apply values results)
		   (call-with-values thunk
		     (lambda xs
		       (cond (ready (apply values results))
			     (else
			      (set! ready #t)
			      (set! results xs)
			      (apply values results))))))))))

	(define (force p)
	  (if (%inline (1 " instanceof SPOCK.Promise") p)
	      ((%property-ref "thunk" p))
	      p))

	))


    (define-library-section port-redirection

      (default

	(define with-input-from-port
	  (let ((dynamic-wind dynamic-wind))
	    (lambda (port thunk)
	      (%check-port port "input" "with-input-from-port")
	      (let ((old #f))
		(dynamic-wind
		    (lambda ()
		      (set! old (%host-ref "SPOCK.stdin"))
		      (%host-set! "SPOCK.stdin" port))
		    thunk
		    (lambda ()
		      (%host-set! "SPOCK.stdin" old)))))))

	(define with-output-to-port
	  (let ((dynamic-wind dynamic-wind))
	    (lambda (port thunk)
	      (%check-port port "output" "with-output-to-port")
	      (let ((old #f))
		(dynamic-wind
		    (lambda ()
		      (set! old (%host-ref "SPOCK.stdout"))
		      (%host-set! "SPOCK.stdout" port))
		    thunk
		    (lambda ()
		      (%host-set! "SPOCK.stdout" old)))))))

	))


    (define-library-section file-operations

      (default

	(define-inline (input-port? x)
	  (and (%inline (1 "instanceof SPOCK.Port") x)
	       (eq? "input" (%property-ref "direction" x))))

	(define-inline (output-port? x)
	  (and (%inline (1 "instanceof SPOCK.Port") x)
	       (eq? "output" (%property-ref "direction" x))))

	(define %close-port
	  (%native-lambda
	   "var port = arguments[ 1 ];"
	   "port.close();"
	   "port.closed = true;"
	   "return K(port);"))

	(define open-input-file
	  (%native-lambda
	   "var fn = SPOCK.check(arguments[ 1 ], 'string', 'open-input-file');"
	   "return K(SPOCK.openInputFile(fn));"))

	(define open-output-file
	  (%native-lambda
	   "var fn = SPOCK.check(arguments[ 1 ], 'string', 'open-input-file');"
	   "var exp = null;"
	   "if(arguments.length === 3)"
	   " exp = SPOCK.check(arguments[ 2 ], 'number', 'open-input-file');"
	   "return K(SPOCK.openOutputFile(fn, exp));"))

	(define (close-input-port port)
	  (let ((port (%check-port port "input" "close-input-port")))
	    (%close-port port)))

	(define (close-output-port port)
	  (let ((port (%check-port port "output" "close-output-port")))
	    (%close-port port)))

	(define call-with-input-file
	  (let ((call-with-values call-with-values)
		(open-input-file open-input-file)
		(values values)
		(apply apply))
	    (lambda (file proc)
	      (let ((in (open-input-file file)))
		(%call-with-saved-values 
		 (lambda () (proc in))
		 (lambda ()
		   (close-input-port in)))))))

	(define call-with-output-file
	  (let ((call-with-values call-with-values)
		(open-output-file open-output-file)
		(values values)
		(apply apply))
	    (lambda (file proc)
	      (let ((out (open-output-file file)))
		(%call-with-saved-values 
		 (lambda () (proc out))
		 (lambda ()
		   (close-output-port out)))))))

	(define with-input-from-file
	  (let ((with-input-from-port with-input-from-port)
		(open-input-file open-input-file)
		(apply apply)
		(values values)
		(call-with-values call-with-values)
		(close-input-port close-input-port))
	    (lambda (filename thunk)
	      (let ((in (open-input-file filename)))
		(with-input-from-port in 
		  (lambda ()
		    (%call-with-saved-values 
		     thunk
		     (lambda ()
		       (close-input-port in)))))))))

	(define with-output-to-file
	  (let ((with-output-to-port with-output-to-port)
		(open-output-file open-output-file)
		(apply apply)
		(values values)
		(call-with-values call-with-values)
		(close-output-port close-output-port))
	    (lambda (filename thunk)
	      (let ((out (open-output-file filename)))
		(with-output-to-port out
		  (lambda ()
		    (%call-with-saved-values 
		     thunk
		     (lambda ()
		       (close-output-port out)))))))))

	))


    (define-library-section string-ports

      (default

	(define (open-input-string str)
	  (define open
	    (%native-lambda
	     "var buffer = arguments[ 1 ];"
	     "var pos = 0;"
	     "var len = buffer.length;"
	     "function read(n) {"
	     " if(pos >= len) return SPOCK.EOF;"
	     " var str = buffer.substring(pos, pos + n);"
	     " pos += n;"
	     " return str;}"
	     "return K(new SPOCK.Port('input', { read: read }));"))
	  (open (%string->jstring str)))

	(define open-output-string
	  (%native-lambda
	   "var buffer = [];"
	   "function write(s) { buffer.push(s); }"
	   "var port = new SPOCK.Port('output', { write: write });"
	   "port.buffer = buffer;"
	   "port.isStringPort = true;"
	   "return K(port);"))

	(define (get-output-string port)
	  (let ((port (%check ("SPOCK.Port") port)))
	    (if (not (void? (%property-ref "isStringPort" port)))
		(let ((str (%jstring->string
			    (%inline ".join" (%property-ref "buffer" port) ""))))
		  (%inline (1 ".buffer = []") port)
		  str)
		;;XXX unnecessary in non-debug mode
		(%inline "SPOCK.error" "bad argument type - not a string port" port))))

	(define (with-input-from-string str thunk)
	  (let ((in (open-input-string str)))
	    (with-input-from-port in thunk)))

	(define (with-output-to-string thunk)
	  (let ((out (open-output-string)))
	    (with-output-to-port out thunk)
	    (get-output-string out)))

	))


    (define-library-section reader

      (default

	(define read
	  (let ((read-char read-char)
		(reverse reverse)
		(peek-char peek-char)
		(list->vector list->vector)
		(list->string list->string)
		(current-input-port current-input-port)
		(string->number string->number))
	    (lambda (port)
	      (let ((port (if (void? port) (current-input-port) port)))
		(define (parse-token t)
		  (or (string->number t)
		      (string->symbol t)))
		(define (read1)
		  (let ((c (read-char port)))
		    (if (eof-object? c) 
			c
			(case c
			  ((#\#) (read-sharp))
			  ((#\() (read-list #\)))
			  ((#\[) (read-list #\]))
			  ((#\{) (read-list #\}))
			  ((#\,) (if (eqv? (peek-char port) #\@)
				     (list 'unquote-splicing (read1))
				     (list 'unquote (read1))))
			  ((#\`) (list 'quasiquote (read1)))
			  ((#\') `',(read1))
			  ((#\;) (skip-line) (read1))
			  ((#\") (read-string))
			  ((#\) #\] #\}) (%error "unexpected delimiter" c))
			  (else
			   (if (char-whitespace? c)
			       (read1)
			       (parse-token (read-token (list c)))))))))
		(define (skip-line)
		  (let ((c (read-char port)))
		    (unless (or (eof-object? c) (char=? #\newline c))
		      (skip-line))))
		(define (skip-whitespace) ; returns peeked char
		  (let ((c (peek-char port)))
		    (cond ((char-whitespace? c)
			   (read-char port)
			   (skip-whitespace))
			  (else c))))
		(define (read-sharp)
		  (let ((c (read-char port)))
		    (if (eof-object? c)
			(%error "unexpected EOF after `#'")
			(case c
			  ((#\() (list->vector (read-list #\))))
			  ((#\;) (read1) (read))
			  ((#\% #\!) (string->symbol (read-token (list c #\#))))
			  ((#\\) 
			   (let ((t (read-token '())))
			     (cond ((string-ci=? "newline" t) #\newline)
				   ((string-ci=? "tab" t) #\tab)
				   ((string-ci=? "space" t) #\space)
				   ((zero? (string-length t))
				    (%error "invalid character syntax"))
				   (else (string-ref t 0)))))
			  (else (%error "invalid `#' syntax" c))))))
		(define (read-list delim)
		  (let loop ((lst '()))
		    (let ((c (skip-whitespace)))
		      (cond ((eof-object? c)
			     (%error "unexpected EOF while reading list"))
			    ((char=? c delim)
			     (read-char port)
			     (reverse lst))
			    (else
			     (if (eqv? #\. c)
				 (let ((t (read-token '())))
				   (if (string=? "." t)
				       (let ((rest (read1)))
					 (skip-whitespace)
					 (if (eqv? (read-char port) delim)
					     (append (reverse lst) rest)
					     (%error "missing closing delimiter" delim)))
				       (loop (cons (parse-token t)) lst)))
				 (loop (cons (read1) lst))))))))
		(define (read-string)
		  (let loop ((lst '()))
		    (let ((c (read-char port)))
		      (cond ((eof-object? c)
			     (%error "unexpected EOF while reading string"))
			    ((char=? #\" c) 
			     (list->string (reverse lst)))
			    ((char=? #\\ c)
			     (let ((c (read-char port)))
			       (if (eof-object? c)
				   (%error "unexpected EOF while reading string")
				   (case c
				     ((#\n) (loop (cons #\newline lst)))
				     ((#\t) (loop (cons #\tab lst)))
				     (else (loop (cons c lst)))))))
			    (else (loop (cons c lst)))))))
		(define (read-token prefix)
		  (let loop ((lst prefix)) ; prefix must be in reverse order
		    (let ((c (peek-char port)))
		      (if (or (eof-object? c) 
			      (memv c '(#\{ #\} #\( #\) #\[ #\] #\; #\"))
			      (char-whitespace? c))
			  (list->string (reverse lst))
			  (loop (cons (read-char port) lst))))))
		(read1)))))

	))


    (define-library-section loading
      
      (default

	(define (load file k)
	  (%inline 
	   "SPOCK.load"
	   (%string->jstring file) 
	   (and (not (%void? k))
		(callback k))))

	))


    (define-library-section error-handling

      (default
	
	;; (%error MESSAGE ARGUMENTS ...)
	(define %error
	  (%native-lambda
	   "SPOCK.error.apply(this, Array.prototype.slice.call(arguments, 1));"))

	(define error %error)

	))


    (define-library-section miscellaneous

      (default

	(define (exit code)
	  (%inline "SPOCK.exit" (if (void? code) 0 (%check "number" code))))

	(define (milliseconds thunk)
	  (let ((t0 (%inline "(new Date()).getTime")))
	    (if (void? thunk)
		t0
		(let* ((r (thunk)) ;XXX will not handle multiple values
		       (t1 (%inline "(new Date()).getTime")))
		  (%inline (1 "-" 2) t1 t0))))) 

	(define-inline (callback proc)
	  (%inline "SPOCK.callback" proc))

	(define-inline (callback-method proc)
	  (%inline "SPOCK.callbackMethod" proc))

	(define (print . args)
	  (for-each display args)
	  (newline))

	(define-inline (id x) x)
	(define-inline (const x) (lambda _ x))
	(define-inline (compl f) (lambda (x) (not (f x))))
	
	(define (o . fns)		;XXX optimize this
	  (if (null? fns)
	      id
	      (let loop ((fns fns))
		(let ((h (%car fns))
		      (t (%cdr fns)) )
		  (if (null? t)
		      h
		      (lambda (x) (h ((loop t) x))))))))

	(define %
	  (%native-lambda
	   "var o = {};"
	   "for(var i = 1; i < arguments.length; i += 2) {"
	   " var x = arguments[ i ];"
	   " if(typeof x === 'string') o[ x ] = arguments[ i + 1 ];"
	   " else if(x instanceof SPOCK.String)"
	   "  o[ x.name ] = arguments[ i + 1 ];"
	   " else SPOCK.error('(%) object key not a string or symbol', x);}"
	   "return K(o);"))

	(define native 
	  (%native-lambda 
	   "var func = arguments[ 1 ];"
	   "return K(function(k) {"
	   " var args = Array.prototype.splice.call(arguments, 1);"
	   " return k(func.apply(this, args));});"))

	(define native-method
	  (%native-lambda
	   "var func = arguments[ 1 ];"
	   "return K(function(k) {"
	   " var args = Array.prototype.splice.call(arguments, 2);"
	   " return k(func.apply(arguments[ 1 ], args));});"))

	(define bind-method
	  (%native-lambda
	   "var func = arguments[ 1 ];"
	   "var that = arguments[ 2 ];"
	   "return K(function() { return func.apply(that, arguments); });"))

	(define-inline (file-exists? filename)
	  (%inline "SPOCK.fileExists" (%string->jstring filename)))

	(define jstring
	  (%native-lambda
	   "var x = arguments[ 1 ];"
	   "if(typeof x === 'string') return K(x);"
	   "else if(x instanceof SPOCK.String) return K(x.normalize());"
	   "else if(x instanceof SPOCK.Char) return K(x.character);"
	   "else return K(x);"))

	))

    ))
