(define-library (tests write-array)
  (export run-tests write-array-to-string)
  (import (scheme base)
          (srfi 64)
          (srfi 231)
          (srfi 268)
          )
  (begin
    (define (write-array-to-string array)
      (call-with-port
       (open-output-string)
       (lambda (port)
	 (write-array array port)
	 (get-output-string port))))

    (define (run-tests)
      (test-group "write-array"
        (test-equal "empty 1d array, both bounds given"
          "#a((0 3)) (0 0 0)"
          (write-array-to-string
           (list*->array 1 '(0 0 0))))
      ))
  ))
