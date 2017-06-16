;; -*- scheme -*-

;; Copyright (c) 2017 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(use-modules (srfi srfi-41)
             (srfi srfi-60)
             (test tap)
             (test setup)
             (communication ieee802-15-4a-preamble))

;; Precalculated from spec
(define code-31-chips
  '(( -1  0  0  0  0 +1  0 -1  0 +1 +1 +1  0 +1 -1  0  0  0 +1 -1 +1 +1 +1  0  0 -1 +1  0 -1  0  0)
    (  0 +1  0 +1 -1  0 +1  0 +1  0  0  0 -1 +1 +1  0 -1 +1 -1 -1 -1  0  0 +1  0  0 +1 +1  0  0  0)
    ( -1 +1  0 +1 +1  0  0  0 -1 +1 -1 +1 +1  0  0 +1 +1  0 +1  0  0 -1  0  0  0  0 -1  0 +1  0 -1)
    (  0  0  0  0 +1 -1  0  0 -1  0  0 -1 +1 +1 +1 +1  0 +1 -1 +1  0  0  0 +1  0 -1  0 +1 +1  0 -1)
    ( -1  0 +1 -1  0  0 +1 +1 +1 -1 +1  0  0  0 -1 +1  0 +1 +1 +1  0 -1  0 +1  0  0  0  0 -1  0  0)
    ( +1 +1  0  0 +1  0  0 -1 -1 -1 +1 -1  0 +1 +1 -1  0  0  0 +1  0 +1  0 -1 +1  0 +1  0  0  0  0)
    ( +1  0  0  0  0 +1 -1  0 +1  0 +1  0  0 +1  0  0  0 +1  0 +1 +1 -1 -1 -1  0 -1 +1  0  0 -1 +1)
    (  0 +1  0  0 -1  0 -1  0 +1 +1  0  0  0  0 -1 -1 +1  0  0 -1 +1  0 +1 +1 -1 +1 +1  0 +1  0  0)))

(with-test-bundle (guile communication gps-ca)
  (plan (length code-31-chips))
  (let loop ((rest code-31-chips) (n 1))
    (if (null? rest)
        #t
        (let ((code-31 (ternary-code-31-chips n)))
          (define-test (format #f "ieee-802.15.4a preamble sequence ~a is correct" n)
            (pass-if-equal? (car rest)
                            code-31))
          (loop (cdr rest) (+ n 1))))))
