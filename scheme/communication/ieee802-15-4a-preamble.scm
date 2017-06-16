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
  #:export (ternary-code-31-chips))

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

(define (ternary-mapper n)
  (if (zero? n) 1 -1))

(define (ternary-code-31-chip-stream n)
  (let* ((gp-a (lfsr-a-31-gp n))
         (init-a (lfsr-a-31-init n))
         (stream-a (word->bit-lfsr (make-lfsr-stream-galois gp-a init-a)))
         (gp-b (lfsr-b-31-gp n))
         (init-b (lfsr-b-31-init n))
         (stream-b (word->bit-lfsr (make-lfsr-stream-galois gp-b init-b))))
    (stream-map (lambda (a b)
                  (* a (ternary-mapper b)))
                stream-a
                stream-b)))

(define (ternary-code-31-chips n)
  (reverse (stream->list 31 (ternary-code-31-chip-stream n))))
