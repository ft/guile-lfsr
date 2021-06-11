;; -*- scheme -*-

;; Copyright (c) 2017 Frank Terbeck <ft@bewatermyfriend.org>
;; All rights reserved.

(use-modules (srfi srfi-41)
             ((srfi srfi-60) #:prefix srfi60:)
             (test tap)
             (test setup)
             (communication gps-ca))

;; Here are the first 10 bits for each C/A sequence:
(define first-10-ca-bits (map (lambda (x)
                                (map (lambda (y) (if y 1 0))
                                     (srfi60:integer->list x)))
                              '(#o1440 #o1620 #o1710 #o1744 #o1133
                                #o1455 #o1131 #o1454 #o1626 #o1504
                                #o1642 #o1750 #o1764 #o1772 #o1775
                                #o1776 #o1156 #o1467 #o1633 #o1715
                                #o1746 #o1763 #o1063 #o1706 #o1743
                                #o1761 #o1770 #o1774 #o1127 #o1453
                                #o1625 #o1712 #o1745 #o1713 #o1134
                                #o1456 #o1713)))

(with-test-bundle (guile communication gps-ca)
  (plan (length first-10-ca-bits))
  (let loop ((rest first-10-ca-bits) (n 1))
    (if (null? rest)
        #t
        (let ((rng (make-gps-ca-stream n)))
          (define-test (format #f "First 10 bits of C/A seq #~a are correct" n)
            (pass-if-equal? (car rest)
                            (stream->list 10 rng)))
          (loop (cdr rest) (+ n 1))))))
