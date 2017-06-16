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
;; This module implements streams that  produce pseudo random gold chip sequen-
;; ces as used in  C/A of the GPS satellite system. The  module uses the linear
;; feedback shift register module (communication lfsr)

(define-module (communication gps-ca)
  #:use-module (srfi srfi-41)
  #:use-module (communication lfsr)
  #:export (make-gps-ca-stream
            gps-prn-id->code-delay))

(define stream-g1-parallel (make-lfsr-stream-fibonacci #b10010000001 #b1111111111))
(define stream-g1-serial (word->bit-lfsr stream-g1-parallel))
(define stream-g2-parallel (make-lfsr-stream-fibonacci #b10110010111 #b1111111111))

(define (gen-selector a b)
  (logior (ash 1 (- 9 a))
          (ash 1 (- 9 b))))

(define gps-prn-id-config
  (vector (cons (gen-selector 1 5) 5)
          (cons (gen-selector 2 6) 6)
          (cons (gen-selector 3 7) 7)
          (cons (gen-selector 4 8) 8)
          (cons (gen-selector 0 8) 17)
          (cons (gen-selector 1 9) 18)
          (cons (gen-selector 0 7) 139)
          (cons (gen-selector 1 8) 140)
          (cons (gen-selector 2 9) 141)
          (cons (gen-selector 1 2) 251)
          (cons (gen-selector 2 3) 252)
          (cons (gen-selector 4 5) 254)
          (cons (gen-selector 5 6) 255)
          (cons (gen-selector 6 7) 256)
          (cons (gen-selector 7 8) 257)
          (cons (gen-selector 8 9) 258)
          (cons (gen-selector 0 3) 469)
          (cons (gen-selector 1 4) 470)
          (cons (gen-selector 2 5) 471)
          (cons (gen-selector 3 6) 472)
          (cons (gen-selector 4 7) 473)
          (cons (gen-selector 5 8) 474)
          (cons (gen-selector 0 2) 509)
          (cons (gen-selector 3 5) 512)
          (cons (gen-selector 4 6) 513)
          (cons (gen-selector 5 7) 514)
          (cons (gen-selector 6 8) 515)
          (cons (gen-selector 7 9) 516)
          (cons (gen-selector 0 5) 859)
          (cons (gen-selector 1 6) 860)
          (cons (gen-selector 2 7) 861)
          (cons (gen-selector 3 8) 862)
          (cons (gen-selector 4 9) 863)
          (cons (gen-selector 3 9) 950)
          (cons (gen-selector 0 6) 947)
          (cons (gen-selector 1 7) 948)
          (cons (gen-selector 3 9) 950)))

(define (gps-prn-id->phase-selector n)
  (car (vector-ref gps-prn-id-config (- n 1))))

(define (gps-prn-id->code-delay n)
  (cdr (vector-ref gps-prn-id-config (- n 1))))

(define (make-g2-serial-stream gps-prn-id)
  (let ((phase-select (gps-prn-id->phase-selector gps-prn-id)))
    (stream-map (lambda (x) (feedback-xor (logand x phase-select)))
                stream-g2-parallel)))

(define (make-gps-ca-stream gps-prn-id)
  (stream-map (lambda (g1 g2) (logxor g1 g2))
              stream-g1-serial
              (make-g2-serial-stream gps-prn-id)))
