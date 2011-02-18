;;;; chicken-spock.scm - command-line compiler


(module main ()

(import scheme chicken)
(use spock-compiler matchable)

(define fail error)

(include "top.scm")

(run (command-line-arguments))

)
