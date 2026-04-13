(import (scheme base)
	(srfi 64)
	(prefix (tests read-array) r:)
	(prefix (tests write-array) w:)
	)

(test-group "srfi-268"
  (r:run-tests)
  (w:run-tests))
