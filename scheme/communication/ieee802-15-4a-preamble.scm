;; Copyright © 2017 Frank Terbeck <ft@bewatermyfriend.org>
;;
;; This library  is free  software; you  can redistribute  it and/or  modify it
;; under the terms of the GNU Lesser General Public License as published by the
;; Free  Software Foundation;  either version  3 of  the License,  or (at  your
;; option) any later version.
;;
;; This library is distributed in the hope  that it will be useful, but WITHOUT
;; ANY  WARRANTY;  without even  the  implied  warranty of  MERCHANTABILITY  or
;; FITNESS FOR A PARTICULAR PURPOSE. See  the GNU Lesser General Public License
;; for more details.
;;
;; You should  have received a  copy of the  GNU Lesser General  Public License
;; along with  this library;  if not,  write to  the Free  Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
;;
;; Commentary:
;;
;; This module  implements ternary  sequences as  used in  ieee-802.15.4a ultra
;; wideband  beacon  preambles.  The  module uses  the  linear  feedback  shift
;; register module (communication lfsr) with LFSRs in Galois configuration.

(define-module (communication ieee802-15-4a-preamble)
  #:use-module (srfi srfi-41)
  #:use-module (communication lfsr)
  #:export (ternary-code-chips
            ternary-code-31-chips
            ternary-code-127-chips))

(define (ternary-mapper n)
  (if (zero? n) 1 -1))

(define ternary-code-31-cfg
  (vector (vector 41 20    61 20)
          (vector 47  8    59  1)
          (vector 41 29    59 29)
          (vector 61  1    41 27)
          (vector 37 20    61 19)
          (vector 61 16    59  3)
          (vector 59 29    61  6)
          (vector 55 12    41 18)))

(define (fetch-31 k n)
  (vector-ref (vector-ref ternary-code-31-cfg (- n 1)) k))

(define (lfsr-a-31-gp n)   (fetch-31 0 n))
(define (lfsr-a-31-init n) (fetch-31 1 n))
(define (lfsr-b-31-gp n)   (fetch-31 2 n))
(define (lfsr-b-31-init n) (fetch-31 3 n))

(define (make-code-31-a-stream n)
  (make-lfsr-stream-galois (lfsr-a-31-gp n) (lfsr-a-31-init n)))

(define (make-code-31-b-stream n)
  (make-lfsr-stream-galois (lfsr-b-31-gp n) (lfsr-b-31-init n)))

(define (ternary-code-31-chip-stream n)
  (let* ((stream-a (word->bit-lfsr (make-code-31-a-stream n)))
         (stream-b (word->bit-lfsr (make-code-31-b-stream n))))
    (stream-map (lambda (a b)
                  (* a (ternary-mapper b)))
                stream-a
                stream-b)))

(define (ternary-code-31-chips n)
  (reverse (stream->list 31 (ternary-code-31-chip-stream n))))


(define ternary-code-127-cfg
  (vector (vector 131  73    143  76    185 122)
          (vector 137  99    143 106    157 101)
          (vector 241  90    247 123    253  31)
          (vector 167 117    143  42    137 109)
          (vector 157  72    203 100    239  70)
          (vector 203  96    143   1    191  50)
          (vector 247  32    131 106    145 124)
          (vector 171 126    185 115    191 122)
          (vector 193  73    157  47    241  25)
          (vector 145  99    185  83    241  43)
          (vector 143  90    191  71    239  49)
          (vector 229  87    241  42    145  91)
          (vector 185  72    211  75    247 102)
          (vector 211  48    241  76    253 110)
          (vector 239   8    137 124    193 109)
          (vector 213 126    157  83    253  94)))

(define (fetch-127 k n)
  (vector-ref (vector-ref ternary-code-127-cfg (- n 1)) k))

(define (lfsr-a-127-gp n)   (fetch-127 0 n))
(define (lfsr-a-127-init n) (fetch-127 1 n))
(define (lfsr-b-127-gp n)   (fetch-127 2 n))
(define (lfsr-b-127-init n) (fetch-127 3 n))
(define (lfsr-c-127-gp n)   (fetch-127 4 n))
(define (lfsr-c-127-init n) (fetch-127 5 n))

(define (make-code-127-a-stream n)
  (make-lfsr-stream-galois (lfsr-a-127-gp n) (lfsr-a-127-init n)))

(define (make-code-127-b-stream n)
  (make-lfsr-stream-galois (lfsr-b-127-gp n) (lfsr-b-127-init n)))

(define (make-code-127-c-stream n)
  (make-lfsr-stream-galois (lfsr-c-127-gp n) (lfsr-c-127-init n)))

(define (ternary-code-127-chip-stream n)
  (let* ((stream-a (word->bit-lfsr (make-code-127-a-stream n)))
         (stream-b (word->bit-lfsr (make-code-127-b-stream n)))
         (stream-c (word->bit-lfsr (make-code-127-c-stream n))))
    (stream-map (lambda (a b c)
                  (* a (ternary-mapper (logxor b c))))
                stream-a
                stream-b
                stream-c)))

(define (ternary-code-127-chips n)
  (reverse (stream->list 127 (ternary-code-127-chip-stream n))))

(define (ternary-code-chips n)
  (let* ((short? (< n 9))
         (size (or (and short? 31) 127))
         (stream (or (and short? ternary-code-31-chip-stream)
                     ternary-code-127-chip-stream))
         (index (or (and short? n) (- n 8))))
    (reverse (stream->list size (stream index)))))
