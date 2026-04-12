(define-library (srfi 268)
  (export read-array)
  (import (scheme base)
	  (scheme case-lambda)
	  (scheme read)
          (srfi 231)
          )
  (begin
    (define storage-class-symbols
      `((char . ,char-storage-class)
        (s8   . ,s8-storage-class)
	(s16  . ,s16-storage-class)
	(s32  . ,s32-storage-class)
	(s64  . ,s64-storage-class)
	(u1   . ,u1-storage-class)
	(u8   . ,u8-storage-class)
	(u16  . ,u16-storage-class)
	(u32  . ,u32-storage-class)
	(u64  . ,u64-storage-class)
	(f8   . ,f8-storage-class)
	(f16  . ,f16-storage-class)
	(f32  . ,f32-storage-class)
	(f64  . ,f64-storage-class)
	(c64  . ,c64-storage-class)
	(c128 . ,c128-storage-class)))

    (define (check-arg who pred x)
      (unless (pred x)
	(error (string-append who ": invalid argument")
	       x)))

    ;; Read the next char & raise an error if it's not in
    ;; valid-chars.
    (define (consume valid-chars)
      (let ((x (read-char)))
	(cond ((eof-object? x)
	       (parsing-error "unexpected EOF"))
	      ((not (memv x valid-chars))
	       (parsing-error "invalid character" x)))))

    (define (consume-tag-prefix)
      (consume '(#\#))
      (consume '(#\a #\A)))

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
    (define (class-symbol->storage-class sym)
      (cond ((assv sym storage-class-symbols) => cdr)
	    (else generic-storage-class)))

    (define (parse-tag)
      (consume-tag-prefix)
      (let ((class-sym (read)))
        (unless (symbol? class-sym)
	  (parsing-error "invalid array tag" class-sym))
        (class-symbol->storage-class class-sym)))

    (define (parse-bounds)
      (let ((bounds (read)))
	(unless (list? bounds)
	  (parsing-error "invalid bounds spec" bounds))
	(map list->vector bounds)))

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
	((port)
         (check-arg "read-array" input-port? port)
         (parameterize ((current-input-port port))
           (read-array)))
        (() (read-array-from-port (current-input-port)))))

  ))
