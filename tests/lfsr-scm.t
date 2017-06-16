;; -*- scheme -*-

;; Copyright (c) 2017 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(use-modules (srfi srfi-41)
             (test tap)
             (test setup)
             (communication lfsr))

(with-test-bundle (guile communication lfsr)
  (plan 2)
  ;; These are examples from the GPS satellite system:
  (let ((rng-a (word->bit-lfsr (make-lfsr-stream-fibonacci #b10010000001
                                                           #b1111111111)))
        (first-14-a '(1 1 1 1 1 1 1 1 1 1 0 0 0 1))
        (rng-b (word->bit-lfsr (make-lfsr-stream-fibonacci #b10110010111
                                                           #b1111111111)))
        (first-14-b '(1 1 1 1 1 1 1 1 1 1 0 0 1 0)))
    (define-test "First 14 chips from GPS-G1 look good"
      (pass-if-equal? (stream->list 14 rng-a)
                      first-14-a))
    (define-test "First 14 chips from GPS-G2 look good"
      (pass-if-equal? (stream->list 14 rng-b)
                      first-14-b))))
