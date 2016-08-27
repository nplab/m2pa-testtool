;;;
;;; Copyright (c) 2011 Michael Tuexen
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;
;;; $Id: mtp.scm,v 1.4 2011/03/20 10:10:16 tuexen Exp $

(define (mtp-make-label dpc opc slc)
  (if (and (exact? dpc) (integer? dpc) (<= 0 dpc 2^14-1)
	   (exact? opc) (integer? opc) (<= 0 opc 2^14-1)
	   (exact? slc) (integer? slc) (<= 0 slc 2^4-1))
      (+ dpc (* 2^14 opc) (* 2^28 slc))
      (error "Arguments invalid" dpc opc slc)))
;;; (mtp-make-label 1 2 3)

(define (mtp-get-dpc label)
  (if (and (exact? label) (integer? label) (<= 0 label 2^32-1))
      (remainder label 2^14)
      (error "label not uint32" label)))
;;; (mtp-get-dpc (mtp-make-label 1 2 3))

(define (mtp-get-opc label)
  (if (and (exact? label) (integer? label) (<= 0 label 2^32-1))
      (remainder (quotient label 2^14) 2^14)
      (error "label not uint32" label)))
;;; (mtp-get-opc (mtp-make-label 1 2 3))

(define (mtp-get-slc label)
  (if (and (exact? label) (integer? label) (<= 0 label 2^32-1))
      (remainder (quotient label 2^28) 2^4)
      (error "label not uint32" label)))
;;; (mtp-get-slc (mtp-make-label 1 2 3))

(define mtp-h0-h1-offset 4)
(define (mtp-get-h0-h1 message)
  (little-endian-bytes->uint8 (list-tail message mtp-h0-h1-offset)))

;;; Servide Indicator as specified in Q.704
(define mtp-si-snm   #b0000)
(define mtp-si-sntm  #b0001)
(define mtp-si-sccp  #b0011)
(define mtp-si-tup   #b0100)
(define mtp-si-isup  #b0101)
(define mtp-si-dup0  #b0110)
(define mtp-si-dup1  #b0111)
(define mtp-si-bisup #b1001)
(define mtp-si-sisup #b1010)

(define mtp-ssf-int       #b0000)
(define mtp-ssf-int-spare #b0100)
(define mtp-ssf-nat       #b1000)
(define mtp-ssf-nat-space #b1100)

;;; H0/H1 for singnalling network management messages according to Table 1/Q.704
(define mtp-h0-chm #b0001)
(define mtp-h1-coo #b0001)
(define mtp-h1-coa #b0010)
(define mtp-h1-xco #b0011)
(define mtp-h1-xca #b0100)
(define mtp-h1-cbd #b0101)
(define mtp-h1-cba #b0110)

(define mtp-h0-ecm #b0010)
(define mtp-h1-eco #b0001)
(define mto-h1-eca #b0010)

(define mtp-h0-fcm #b0011)
(define mtp-h1-rct #b0001)
(define mtp-h1-tfc #b0010)

(define mtp-h0-tfm #b0100)
(define mtp-h1-tfp #b0001)
(define mtp-h1-tfr #b0011)
(define mtp-h1-tfa #b0101)

(define mtp-h0-rsm #b0101)
(define mtp-h1-rst #b0001)
(define mtp-h1-rsr #b0010)

(define mtp-h0-mim #b0110)
(define mtp-h1-lin #b0001)
(define mtp-h1-lun #b0010)
(define mtp-h1-lia #b0011)
(define mtp-h1-lua #b0100)
(define mtp-h1-lid #b0101)
(define mtp-h1-lfu #b0110)
(define mtp-h1-llt #b0111)
(define mtp-h1-lrt #b1000)

(define mtp-h0-trm #b0111)
(define mtp-h1-tra #b0001)

(define mtp-h0-dlm #b1000)
(define mtp-h1-dlc #b0001)
(define mtp-h1-css #b0010)
(define mtp-h1-cns #b0011)
(define mtp-h1-cnp #b0100)

(define mtp-h0-ufc #b1010)
(define mtp-h1-upu #b0001)

;;; H0/H1 for singnalling link test messages according to Q.707
(define mtp-h0-tst  #b0001)
(define mtp-h1-sltm #b0001)
(define mtp-h1-slta #b0010)

(define (mtp-make-sltm dpc opc slc pattern)
  (append (uint32->little-endian-bytes (mtp-make-label dpc opc slc))
	  (uint8->little-endian-bytes (+ mtp-h0-tst (* mtp-h1-sltm 2^4)))
	  (uint8->little-endian-bytes (* (length pattern) 2^4))
	  pattern))
;;; (mtp-make-sltm 1 2 3 (list 1 2 3 4))

(define (mtp-make-slta dpc opc slc pattern)
  (append (uint32->little-endian-bytes (mtp-make-label dpc opc slc))
	  (uint8->little-endian-bytes (+ mtp-h0-tst (* mtp-h1-slta 2^4)))
	  (uint8->little-endian-bytes (* (length pattern) 2^4))
	  pattern))
;;; (mtp-make-slta 1 2 3 (list 1 2 3 4))

(define (mtp-make-slta-from-sltm sltm)
  (let* ((label (little-endian-bytes->uint32 sltm))
	 (dpc (mtp-get-dpc label))
	 (opc (mtp-get-opc label))
	 (slc (mtp-get-slc label))
	 (pattern (list-tail sltm 6)))
    (mtp-make-slta opc dpc slc pattern)))
;;; (mtp-make-slta-from-sltm (mtp-make-sltm 1 1 3 (list 1 2 3 4)))

(define (mtp-sltm-message? message)
  (= (mtp-get-h0-h1 message) (+ mtp-h0-tst (* mtp-h1-sltm 2^4))))

(define (mtp-slta-message? message)
  (= (mtp-get-h0-h1 message) (+ mtp-h0-tst (* mtp-h1-slta 2^4))))
