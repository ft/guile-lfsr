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
;; This module implements  linear feedback shift registers  as infinite streams
;; of bits.  LFSRs are used,  for example, to  generate chip sequences  in CDMA
;; systems, for synchronising preambles or as pseudo random number generators.

(define-module (communication lfsr)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-60)
  #:export (make-lfsr-stream-fibonacci
            make-lfsr-stream-galois
            word->bit-lfsr
            feedback-xor))

(define (feedback-xor x)
  (modulo (bit-count x) 2))

(define lfsr-iterator
  (stream-lambda (fnc head) (stream-cons head (lfsr-iterator fnc (fnc head)))))

(define (minimum-word-width word)
  (let loop ((x word) (steps 0))
    (if (zero? x)
        (- steps 1)
        (loop (ash x -1) (+ 1 steps)))))

(define (lfsr-step-fibonacci gp)
  (let* ((width (minimum-word-width gp))
         (msb (ash 1 (- width 1))))
    (lambda (x)
      (let ((new (feedback-xor (logand x gp)))
            (shifted (ash x -1)))
        (if (zero? new)
            shifted
            (logior shifted msb))))))

(define (lfsr-step-galois gp)
  (let* ((width (minimum-word-width gp)))
    (lambda (x)
      (let* ((new (logand x 1)))
        (if (zero? new)
            (ash x -1)
            (ash (logxor x gp) -1))))))

(define (make-lfsr-stream gp init stepper)
  (lfsr-iterator (stepper gp) init))

(define (make-lfsr-stream-fibonacci gp init)
  (make-lfsr-stream gp init lfsr-step-fibonacci))

(define (make-lfsr-stream-galois gp init)
  (make-lfsr-stream gp init lfsr-step-galois))

(define (word->bit-lfsr strm)
  (stream-map (lambda (x) (logand x 1)) strm))
