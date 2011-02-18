;;;; xref.scm


(define (report-undefined)
  (let loop ((old undefined) (new '()))
    (cond ((null? old)
	   (cond ((pair? new)
		  (let ((out (current-error-port)))
		    (display "\nError: access to undefined global variables:\n\n" out)
		    (for-each
		     (lambda (id)
		       (display "  " out)
		       (write id out)
		       (display "\n" out)) 
		     (sort new symbol<?))
		    #t))
		 (else #f)))
	  ((get (car old) 'defined) 
	   (loop (cdr old) new))
	  (else (loop (cdr old) (cons (car old) new))))))

(define (xref sections show)
  (when show
    (for-each
     (lambda (defd) (pp `(define ,defd)))
     (sort defined symbol<?))
    (for-each 
     (lambda (refd)
       (when (or (not (symbol? (get refd 'defined))) sections)
	 (pp refd)))
     (sort referenced symbol<?))
    (for-each
     (lambda (asgnd) (pp `(set! ,asgnd)))
     (sort assigned symbol<?)))
  (and sections
       (let loop ((us used-sections) (done '()))
	 (cond ((null? us)
		(when show
		  (for-each
		   (lambda (s) (pp `(section ,s)))
		   (sort done symbol<?)))
		done)
	       (else
		(let* ((s (car us))
		       (done (cons s done)))
		  (let loop2 ((deps (or (get s 'depends) '()))
			      (us (cdr us)))
		    (cond ((null? deps)
			   (loop us done))
			  ((memq (car deps) done)
			   (loop2 (cdr deps) us))
			  (else
			   (loop2 (cdr deps) (cons (car deps) us)))))))))))
