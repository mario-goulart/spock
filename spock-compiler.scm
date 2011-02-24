;;;; spock-compiler.scm


(module spock-compiler (make-spock-state
			spock-state-mstore
			spock-state-options
			spock-state?
			current-spock-state
			spock
			spock-initialize)

(import scheme (except chicken expand))
(use matchable files extras ports 
     (except data-structures butlast join))

(include "config")
(include "misc")
(include "expand")
(include "core")
(include "opt")
(include "xref")
(include "honu")
(include "bind")
(include "sections")
(include "spock/library")
(include "driver")
(include "codegen")

(define-record spock-state
  mstore
  options)

;;
(set! make-spock-state
  (let ((make-spock-state make-spock-state)
	(spock spock))
    (lambda options
      (make-spock-state (apply spock 'prepare options) options))))

;;
(define current-spock-state (make-parameter #f))

;;
(define (spock-initialize . options)
  (current-spock-state
   (apply make-spock-state options)))

)
