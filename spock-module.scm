;;;; spock-module.scm - read-syntax and programming interface


(module spock (<spock-header>
	       spock-script
	       <spock>)

(import scheme chicken)
(use matchable ports)
(require-library spock-compiler)
(reexport spock-compiler)


;;
(define (<spock-header> #!key (minified #t) debug path)
  (string-append
   "<script type='text/javascript' src='"
   (if path (string-append path "/") "")
   "spock-runtime"
   (if debug "-debug" "")
   (if minified "-min" "")
   ".js'></script>\n"))   

;;
(define (spock-script x . options)
  (let ((state (current-spock-state)))
    (unless state
      (set! state (make-spock-state))
      (current-spock-state state))
    (with-output-to-string
      (lambda ()
	(display "<script type='text/javascript'>\n")
	(apply spock
	       'code (strip-syntax x)
	       'environment (spock-state-mstore state)
	       (append (spock-state-options state) options))
	(display "\n</script>\n")))))

;;
(define-syntax (<spock> x r c)
  (let ((%cons (r 'cons))
	(%append (r 'append))
	(%list->vector (r 'list->vector))
	(%spock-script (r 'spock-script)))
    (define (unq x)
      (cond ((pair? x)
	     (cond ((and (symbol? (car x)) 
			 (= (length x) 2)
			 (eq? '<unquote> (strip-syntax (car x))))
		    (cadr x))
		   ((and (pair? (car x))
			 (symbol? (caar x))
			 (= (length (car x)) 2)
			 (eq? '<unquote-splicing> (strip-syntax (caar x))))
		    `(,%append (cadar x) ,(unq (cdr x))))
		   (else `(,%cons ,(unq (car x)) ,(unq (cdr x)))))) ;XXX could be improved
	    ((vector? x)
	     `(,%list->vector ,(unq (vector->list x))))
	    (else `',x)))
    `(,%spock-script ,(unq (cadr x)))))


;; read-syntax

(set-sharp-read-syntax!
 #\`
 (lambda (port)
   `(<spock> ,(read port))))

(set-sharp-read-syntax!
 #\^
 (lambda (port)
   (cond ((eqv? (peek-char port) #\@)
	  (read-char port)
	  `(<unquote-splicing> ,(read port)))
	 (else `(<unquote> ,(read port))))))

)
