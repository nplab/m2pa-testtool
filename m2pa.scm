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
;;; $Id: m2pa.scm,v 1.10 2011/03/22 19:06:45 tuexen Exp $

(define m2pa-test-result-passed           0)
(define m2pa-test-result-failed           1)
(define m2pa-test-result-unknown          2)
(define m2pa-test-result-not-applicable 253)

;;; This is the IANA registered PPID for M2PA in host byte order
(define m2pa-ppid                         5)

;;; This is the IANA registered port for M2PA
(define m2pa-port                      3565)

;;; Constants for the message classes
(define m2pa-message-class               11)
(define m2pa-reserved-message-class      99)

;;; Constants for the message types
(define m2pa-user-data-type               1)
(define m2pa-link-status-type             2)

;;; Constant for the protocol version
(define m2pa-version                      1)

;;; Constant for spare field
(define m2pa-spare                        0)

;;;
;;; Creator functions for messages
;;;

(define (m2pa-make-common-header version spare class type length)
  (append (uint8->bytes version)
	  (uint8->bytes spare)
	  (uint8->bytes class)
	  (uint8->bytes type)
	  (uint32->bytes length)))

;;;(m2pa-make-common-header 1 2 3 4 5)
;;;(m2pa-make-common-header m2pa-version m2pa-spare m2pa-message-class m2pa-user-data-type 16)

(define m2pa-unused                       0)

(define (m2pa-make-m2pa-header unused-1 bsn unused-2 fsn)
  (append (uint8->bytes unused-1)
	  (uint24->bytes bsn)
	  (uint8->bytes unused-2)
	  (uint24->bytes fsn)))

;;;(m2pa-make-m2pa-header 1 2 3 4)
;;;(m2pa-make-m2pa-header m2pa-unused 1024 m2pa-unused 16)

(define m2pa-common-header-length         8)
(define m2pa-m2pa-header-length           8)

(define (m2pa-make-message type bsn fsn data)
  (append (m2pa-make-common-header m2pa-version
				   m2pa-spare
				   m2pa-message-class
				   type
				   (+ m2pa-common-header-length
				      m2pa-m2pa-header-length
				      (length data)))
	  (m2pa-make-m2pa-header m2pa-unused bsn m2pa-unused fsn)
	  data))

(define m2pa-priority                     0)
(define (m2pa-make-user-data-message bsn fsn data)
  (m2pa-make-message m2pa-user-data-type bsn fsn data))
;;; (m2pa-make-user-data-message 1 2 0 (list 2 3 4))

(define (m2pa-make-sltm-message bsn fsn sltm)
  (m2pa-make-user-data-message bsn
			       fsn
			       (cons m2pa-priority
				     (cons (+ mtp-si-sntm (* mtp-ssf 2^4))
					   sltm))))
(define m2pa-make-slta-message m2pa-make-sltm-message)

(define m2pa-alignment-state              1)
(define m2pa-proving-normal-state         2)
(define m2pa-proving-emergency-state      3)
(define m2pa-ready-state                  4)
(define m2pa-processor-outage-state       5)
(define m2pa-processor-recovered-state    6)
(define m2pa-busy-state                   7)
(define m2pa-busy-ended-state             8)
(define m2pa-out-of-service-state         9)

(define (m2pa-make-link-status-message bsn fsn state filler)
  (m2pa-make-message m2pa-link-status-type bsn fsn (append (uint32->bytes state) filler)))
;;; (m2pa-make-link-status-message 2 3 m2pa-out-of-service-state (list))

(define (m2pa-increment-version l)
  (if (positive? (length l))
      (cons (+ (car l) 1) (cdr l))
      (list)))
;;;(m2pa-increment-version (list 1 2 3))
;;;(m2pa-increment-version (list))

;;;
;;; General accessor functions for messages
;;;

(define m2pa-version-offset        0)
(define m2pa-spare-offset          1)
(define m2pa-message-class-offset  2)
(define m2pa-message-type-offset   3)
(define m2pa-message-length-offset 4)
(define m2pa-bsn-offset            9)
(define m2pa-fsn-offset           13)
(define m2pa-state-offset         16)
(define m2pa-filler-offset        20)
(define m2pa-user-data-offset     16)

(define (m2pa-get-version message)
  (bytes->uint8 (list-tail message m2pa-version-offset)))
;;; (m2pa-get-version  (m2pa-make-link-status-message 2 3 m2pa-out-of-service-state (list)))

(define (m2pa-get-spare message)
  (bytes->uint8 (list-tail message m2pa-spare-offset)))
;;; (m2pa-get-spare (m2pa-make-link-status-message 2 3 m2pa-out-of-service-state (list)))

(define (m2pa-get-message-class message)
  (bytes->uint8 (list-tail message m2pa-message-class-offset)))
;;; (m2pa-get-message-class (m2pa-make-link-status-message 2 3 m2pa-out-of-service-state (list)))

(define (m2pa-get-message-type message)
  (bytes->uint8 (list-tail message m2pa-message-type-offset)))
;;; (m2pa-get-message-type (m2pa-make-link-status-message 2 3 m2pa-out-of-service-state (list)))

(define (m2pa-get-message-length message)
  (bytes->uint32 (list-tail message m2pa-message-length-offset)))
;;; (m2pa-get-message-length (m2pa-make-link-status-message 2 3 m2pa-out-of-service-state (list)))

(define (m2pa-get-message-fsn message)
  (bytes->uint24 (list-tail message m2pa-fsn-offset)))

(define (m2pa-get-message-bsn message)
  (bytes->uint24 (list-tail message m2pa-bsn-offset)))

(define (m2pa-get-message-state message)
  (bytes->uint32 (list-tail message m2pa-state-offset)))
;;; (m2pa-get-message-state (list 1 2 3))

(define (m2pa-get-message-filler message)
  (list-tail message m2pa-filler-offset))

(define (m2pa-get-user-data message)
  (list-tail message m2pa-user-data-offset))

(define (m2pa-get-sio message)
  (let ((data (m2pa-get-user-data message)))
    (cadr data)))

(define (m2pa-get-sif message)
  (let ((data (m2pa-get-user-data message)))
    (cddr data)))

(define (m2pa-user-data-message? message)
  (= (m2pa-get-message-type message) m2pa-user-data-type))

(define (m2pa-sltm-message? message)
  (and (m2pa-user-data-message? message)
       (>= (m2pa-get-message-length message) (+ 8 8 2 8)) 
       (= (m2pa-get-sio message) (+ mtp-si-sntm (* mtp-ssf 2^4)))
       (mtp-sltm-message? (m2pa-get-sif message))))

(define (m2pa-slta-message? message)
  (and (m2pa-user-data-message? message)
       (>= (m2pa-get-message-length message) (+ 8 8 2 8)) 
       (= (m2pa-get-sio message) (+ mtp-si-sntm (* mtp-ssf 2^4)))
       (mtp-slta-message? (m2pa-get-sif message))))

(define (m2pa-link-status-message? message)
  (= (m2pa-get-message-type message) m2pa-link-status-type))

(define (m2pa-alignment-message? message)
  (and (m2pa-link-status-message? message)
       (= (m2pa-get-message-state message) m2pa-alignment-state)))

(define (m2pa-proving-normal-message? message)
  (and (m2pa-link-status-message? message)
       (= (m2pa-get-message-state message) m2pa-proving-normal-state)))

(define (m2pa-proving-emergency-message? message)
  (and (m2pa-link-status-message? message)
       (= (m2pa-get-message-state message) m2pa-proving-emergency-state)))

(define (m2pa-ready-message? message)
  (and (m2pa-link-status-message? message)
       (= (m2pa-get-message-state message) m2pa-ready-state)))

(define (m2pa-processor-outage-message? message)
  (and (m2pa-link-status-message? message)
       (= (m2pa-get-message-state message) m2pa-processor-outage-state)))

(define (m2pa-processor-recovered-message? message)
  (and (m2pa-link-status-message? message)
       (= (m2pa-get-message-state message) m2pa-processor-recovered-state)))

(define (m2pa-busy-message? message)
  (and (m2pa-link-status-message? message)
       (= (m2pa-get-message-state message) m2pa-busy-state)))

(define (m2pa-busy-ended-message? message)
  (and (m2pa-link-status-message? message)
       (= (m2pa-get-message-state message) m2pa-busy-ended-state)))

(define (m2pa-out-of-service-message? message)
  (and (m2pa-link-status-message? message)
       (= (m2pa-get-message-state message) m2pa-out-of-service-state)))

;;;
;;;  M2PA helper routines
;;;

(define m2pa-maximum-message-length (expt 2 16))

(define (m2pa-connect local-addr local-port remote-addr remote-port)
  (let ((s (socket AF_INET SOCK_STREAM IPPROTO_SCTP)))
    (catch 'system-error
	   (lambda ()
	     (bind s AF_INET (inet-aton local-addr) local-port)
	     (connect s AF_INET (inet-aton remote-addr) remote-port)
	     (if (defined? 'SCTP_NODELAY)
		 (setsockopt s IPPROTO_SCTP SCTP_NODELAY 1))
	     s)
	   (lambda (key . args)
	     (close s)))))

;;; (m2pa-connect "127.0.0.1" 0 "127.0.0.1" m2pa-port)

(define (m2pa-accept local-addr local-port)
  (let ((s (socket AF_INET SOCK_STREAM IPPROTO_SCTP)))
    (catch 'system-error  
	   (lambda ()
	     (bind s AF_INET (inet-aton local-addr) local-port)
	     (listen s 1)
	     (let ((ss (car (accept s))))
	       (close s)
	       (if (defined? 'SCTP_NODELAY)
		   (setsockopt ss IPPROTO_SCTP SCTP_NODELAY 1))
	       ss))
	   (lambda (key . args)
	     (close s)))))
    

;;;(m2pa-accept "127.0.0.1" m2pa-port)

(define (m2pa-send-message socket stream message)
  (catch 'system-error
	 (lambda()
	   (sctp-sendmsg socket (bytes->string message) (htonl m2pa-ppid) stream 0 0 AF_INET INADDR_ANY 0))
	 (lambda (key . args)
	   0)))

(define (m2pa-recv-message socket)
  (let ((buffer (make-string m2pa-maximum-message-length)))
    (catch 'system-error
	   (lambda ()
	     (let ((n (recv! socket buffer)))
	       (string->bytes (substring buffer 0 n))))
	   (lambda (key . args)
	     (list)))))
;;; (m2pa-recv-message s)

(define (m2pa-recv-message-on-stream socket stream)
  (let ((buffer (make-string m2pa-maximum-message-length)))
    (catch 'system-error
	   (lambda ()
	     (let ((result (sctp-recvmsg! socket buffer)))
	       (if (= stream (car (cadddr result)))
		   (string->bytes (substring buffer 0 (car result)))
		   (m2pa-recv-message-on-stream socket stream))))
	   (lambda (key . args)
	     (list)))))

(define (m2pa-recv-message-with-timeout socket seconds)
  (let ((buffer (make-string m2pa-maximum-message-length)))
    (catch 'system-error
	   (lambda ()
	     (let ((result (select (list socket) (list) (list) seconds)))
	       (if (null? (car result))
		   (list)
		   (let ((n (recv! socket buffer)))
		     (string->bytes (substring buffer 0 n))))))
	   (lambda (key . args)
	     (list)))))

;;; (m2pa-recv-message-with-timeout s 2)

(define (m2pa-wait-for-message socket predicate)
  (let ((m (m2pa-recv-message socket)))
    (if (or (zero? (length m)) (predicate m))
	m
	(m2pa-wait-for-message socket predicate))))

(define (m2pa-wait-for-message-on-stream socket predicate stream)
  (let ((m (m2pa-recv-message-on-stream socket stream)))
    (if (or (zero? (length m)) (predicate m))
	m
	(m2pa-wait-for-message-on-stream socket predicate stream))))

(define (m2pa-wait-for-message-with-timeout socket predicate seconds)
  (let ((m (m2pa-recv-message-with-timeout socket seconds)))
    (if (or (zero? (length m)) (predicate m))
	m
	(m2pa-wait-for-message-with-timeout socket predicate seconds))))

(define (m2pa-version-ok? message)
  (= (m2pa-get-version message) m2pa-version))
;;; (m2pa-version-ok? (m2pa-make-link-status-message 2 3 m2pa-out-of-service-state (list)))

(define (m2pa-message-class-ok? message)
  (= (m2pa-get-message-class message) m2pa-message-class))
;;; (m2pa-message-class-ok? (m2pa-make-link-status-message 2 3 m2pa-out-of-service-state (list)))

(define (m2pa-message-type-ok? message)
  (let ((type (m2pa-get-message-type message)))
    (or (= type m2pa-user-data-type)
	(= type m2pa-link-status-type))))
;;; (m2pa-message-type-ok? (m2pa-make-link-status-message 2 3 m2pa-out-of-service-state (list)))

(define (m2pa-message-length-ok? message)
  (= (m2pa-get-message-length message) (length message)))
;;; (m2pa-message-length-ok? (m2pa-make-link-status-message 2 3 m2pa-out-of-service-state (list)))

(define (m2pa-check-common-header message)
  (and (m2pa-version-ok? message)
       (m2pa-message-class-ok? message)
       (m2pa-message-type-ok? message)
       (m2pa-message-length-ok? message)))
;;; (m2pa-check-common-header (m2pa-make-link-status-message 2 3 m2pa-out-of-service-state (list)))

(define m2pa-sut-state-out-of-service    0) ;;; Have sent a m2pa-out-of-service-state, waiting for one.
(define m2pa-sut-state-not-aligned       1) ;;; Have sent a m2pa-alignment-state, waiting for one.
(define m2pa-sut-state-aligned           2) ;;; Have sent a m2pa-proving-normal-state, waiting for one.
(define m2pa-sut-state-proving-normal    3) ;;; Have sent a m2pa-ready-state after receiving a m2pa-proving-normal-state,
                                            ;;; waiting for m2pa-ready-state.
(define m2pa-sut-state-proving-emergency 4) ;;; Have sent a m2pa-ready-state after receiving a m2pa-emergency-normal-state,
                                            ;;; waiting for m2pa-ready-state.
(define m2pa-sut-state-aligned-ready     5) ;;; Have received a m2pa-ready-state.

(define (m2pa-perform-sut fd state bsn fsn)
  (let ((message (m2pa-recv-message fd)))
    (if (positive? (length message))
	(if (m2pa-check-common-header message)
	    (cond
	      ((= state m2pa-sut-state-out-of-service)
	       (cond
		((m2pa-out-of-service-message? message)
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message bsn fsn m2pa-alignment-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-not-aligned bsn fsn))
		(else
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message #xffffff 0 m2pa-out-of-service-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-out-of-service #xffffff 0))))
	      ((= state m2pa-sut-state-not-aligned)
	       (cond 
		((m2pa-out-of-service-message? message)
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message bsn fsn m2pa-alignment-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-not-aligned bsn fsn))
		((m2pa-alignment-message? message)
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message bsn fsn m2pa-proving-normal-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-aligned bsn fsn))
		(else
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message #xffffff 0 m2pa-out-of-service-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-out-of-service #xffffff 0))))
	      ((= state m2pa-sut-state-aligned)
	       (cond 
		((m2pa-alignment-message? message)
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message bsn fsn m2pa-proving-normal-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-aligned bsn fsn))
		((m2pa-proving-normal-message? message)
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message bsn fsn m2pa-ready-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-proving-normal bsn fsn))
		((m2pa-proving-emergency-message? message)
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message bsn fsn m2pa-ready-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-proving-emergency bsn fsn))
		((m2pa-out-of-service-message? message)
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message #xffffff 0 m2pa-out-of-service-state (list)))
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message bsn fsn m2pa-alignment-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-not-aligned bsn fsn))
		(else
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message #xffffff 0 m2pa-out-of-service-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-out-of-service #xffffff 0))))
	      ((or (= state m2pa-sut-state-proving-normal) (= state m2pa-sut-state-proving-emergency))
	       (cond
		((m2pa-ready-message? message)
		 (m2pa-send-message fd 1 (m2pa-make-sltm-message bsn fsn (mtp-make-sltm tester-pc sut-pc slc
											(string->bytes "M2PA testtool"))))
		 (m2pa-perform-sut fd m2pa-sut-state-aligned-ready bsn (+mod2^24 fsn 1)))
		((m2pa-out-of-service-message? message)
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message #xffffff 0 m2pa-out-of-service-state (list)))
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message bsn fsn m2pa-alignment-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-not-aligned bsn fsn))
		(else
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message #xffffff 0 m2pa-out-of-service-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-out-of-service #xffffff 0))))
	      ((= state m2pa-sut-state-aligned-ready)
	       (cond
		((m2pa-user-data-message? message)
		 (if (or (and (> (m2pa-get-message-length message) 16)
			      (= (+mod2^24 bsn 1) (m2pa-get-message-fsn message)))
			 (and (= (m2pa-get-message-length message) 16)
			      (= bsn (m2pa-get-message-fsn message))))
		     (if (> (m2pa-get-message-length message) 16)
			 (begin
			   (if (m2pa-sltm-message? message)
			       (begin
				 (m2pa-send-message fd 1 (m2pa-make-slta-message (+mod2^24 bsn 1) fsn (mtp-make-slta-from-sltm sltm)))
				 (m2pa-perform-sut fd m2pa-sut-state-aligned-ready (+mod2^24 bsn 1) (+mod2^24 fsn 1)))
			       (begin
				 (m2pa-send-message fd 1 (m2pa-make-user-data-message (+mod2^24 bsn 1) (-mod2^24 fsn 1) (list)))
				 (m2pa-perform-sut fd m2pa-sut-state-aligned-ready (+mod2^24 bsn 1) fsn))))
			 (m2pa-perform-sut fd m2pa-sut-state-aligned-ready bsn fsn))
		     (begin
		       (m2pa-send-message fd 0 (m2pa-make-link-status-message #xffffff 0 m2pa-out-of-service-state (list)))
		       (m2pa-perform-sut fd m2pa-sut-state-out-of-service #xffffff 0))))
		((m2pa-out-of-service-message? message)
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message #xffffff 0 m2pa-out-of-service-state (list)))
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message bsn fsn m2pa-alignment-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-not-aligned bsn fsn))
		(else
		 (m2pa-send-message fd 0 (m2pa-make-link-status-message #xffffff 0 m2pa-out-of-service-state (list)))
		 (m2pa-perform-sut fd m2pa-sut-state-out-of-service #xffffff 0)))))
	    (begin
	      (m2pa-perform-sut fd state bsn fsn))))))

(define (m2pa-run-sut)
  (let ((fd (if sut-is-server
		(m2pa-accept sut-addr sut-port)
		(m2pa-connect sut-addr sut-port tester-addr tester-port))))
    (m2pa-send-message fd 0 (m2pa-make-link-status-message #xffffff 0 m2pa-out-of-service-state (list)))
    (m2pa-perform-sut fd m2pa-sut-state-out-of-service #xffffff 0)
    (close fd)))
