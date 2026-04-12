(define-library (srfi 268)
  (export read-array)
  (import (scheme base)
	  (scheme case-lambda)
	  (scheme read)
          (srfi 231)
          )
  (begin
    (define (check-arg who pred x)
      (unless (pred x)
	(error (string-append who ": invalid argument")
	       x)))

    (define (consume-tag-prefix port)
      (let ((c (read-char port)))
        (unless (eqv? c #\#)
          (parsing-error "invalid tag character (expected #)" c)))
      (let ((c (read-char port)))
	(unless (memv c '(#\a #\A))
	  (parsing-error "invalid tag character (expected a or A)" c))))

    (define (parsing-error msg . irritants)
      (apply error
	     (string-append "read-array: " msg)
	     irritants))

    (define (read-array-from-port port)
      (check-arg "read-array" input-port? port)
      (let* ((class (parse-tag port))
	     (bounds (parse-bounds port))
	     (contents (read port)))
	(unless (list? contents)
	  (error "read-array: invalid array contents"
		 contents))
	(build-array (apply make-interval bounds)
		     class
		     contents)))

    ;; Parse an array tag from *port* and return an appropriate
    ;; storage class.
    (define (parse-tag port)
      (consume-tag-prefix port)
      (let ((class-sym (read)))
        (unless (symbol? class-sym)
	  (parsing-error "invalid array tag" class-sym))
        (class-symbol->storage-class class-sym)))

    ;; TODO
    (define (parse-bounds port)
      #f)

    ;; TODO
    (define (class-symbol->storage-class sym)
      #f)

    ;; It would be easier to use list*->array to build the new
    ;; array here, but we need finer-grained control over the
    ;; interval.
    (define (build-array interval storage-class contents)
      (let* ((A (make-specialized-array interval storage-class))
             (set-entry! (array-setter A)))
	;; TODO: set all entries from *contents*
	A))

    (define read-array
      (case-lambda
        (() (read-array-from-port (current-input-port)))
	((port) (read-array-from-port port))))

  ))
