#| rep.net.rpc -- simple RPC mechanisms for inter-host communication

   $Id$

   Copyright (C) 2001 John Harper <jsh@pixelslut.com>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;; Commentary:

;; This module implements a very simple RPC mechanism over TCP/IP
;; sockets.

;; Servers register functions that may be called by remote systems,
;; producing an id that can be used by the remote system (together with
;; the host name and chosen port number) to create a proxy function.

;; Calling the proxy function is then exactly the same as calling the
;; real function (with the exception that all data must be able to be
;; printed and re-read)

;; Using this module to create proxies for functions returned by the
;; `object' macro defined by rep.data.objects gives an object-oriented
;; rpc mechanism, somewhat like a dynamically-typed version of CORBA!


;; Example:

;; =Server=

;; (rpc-create-server 10000)		-- create listener on port 10000

;; (define (foo x) (+ x 42))

;; (define foo-id (make-rpc-servant foo))

;; `foo-id' is now a symbol that uniquely identifies the `foo' function
;; on this server. E.g. it may be something like `=9s72fdln00=61vxd7='


;; =Client=

;; (define proxy (make-rpc-proxy "localhost" 10000 '=9s72fdln00=61vxd7=))

;; Assuming both client and server are on the same machine in this
;; case. Now `proxy' is a function that when called marshals all its
;; arguments, sends them to the server, along with the unique id, and
;; waits for a result to be returned, which it then unmarshals and
;; returns


;; Todo:

;; - add a mechanism for async (`oneway') requests


(define-structure rep.net.rpc

    (export rpc-socket-listener
	    rpc-output-handler
	    rpc-create-server
	    rpc-destroy-server
	    register-rpc-server
	    deregister-rpc-server
	    make-rpc-servant
	    destroy-rpc-servant
	    make-rpc-proxy
	    servant-id->global-id
	    global-id->rpc-proxy)

    (open rep
	  rep.io.sockets
	  rep.io.processes
	  rep.system
	  rep.regexp
	  rep.data.tables
	  rep.data.records)

  (define-record-type :socket-data
    (make-socket-data closable)
    ;; no predicate
    (pending-data socket-pending-data socket-pending-data-set!)
    (result-pending socket-result-pending socket-result-pending-set!)
    (closable socket-closable-p))

  (define listener-socket nil)

;;; connection cache

  ;; maps from (SERVER-NAME . PORT-NUMBER) -> SOCKET
  (define socket-cache (make-table equal-hash equal))

  ;; maps from SOCKET -> SOCKET-DATA
  (define socket-data-table (make-weak-table eq-hash eq))

  (define (server-socket server port)
    (or (table-ref socket-cache (cons server port))
	(open-server server port)
	(error ("No connection with server %s:%d" server port))))

  (define (register-rpc-server socket #!key closable)
    (let ((server (socket-peer-address socket))
	  (port (socket-peer-port socket)))
      (table-set socket-cache (cons server port) socket)
      (table-set socket-data-table socket (make-socket-data closable))))

  (define (deregister-rpc-server socket)
    (let ((server (socket-peer-address socket))
	  (port (socket-peer-port socket)))
      (when (eq (server-socket server port) socket)
	(let ((data (socket-data socket)))
	  (when (socket-closable-p data)
	    (close-socket socket))
	  (table-unset socket-cache (cons server port))
	  (table-unset socket-data-table socket)))))

  (define (socket-data socket) (table-ref socket-data-table socket))

;;; socket I/O

  (define (rpc-socket-listener master-socket)
    (let (socket)
      (setq socket (socket-accept master-socket
				  (lambda (output)
				    (rpc-output-handler socket output))
				  (lambda ()
				    (deregister-rpc-server socket))))
      (register-rpc-server socket #:closable nil)
      socket))

  (define (open-server host port)
    (let (socket)
      (setq socket (socket-client host port
				  (lambda (x)
				    (rpc-output-handler socket x))
				  (lambda ()
				    (deregister-rpc-server socket))))
      (register-rpc-server socket #:closable t)
      socket))

  (define (rpc-output-handler socket output)
    ;;(format standard-error "Read: %S\n" output)
    (let ((sock-data (socket-data socket)))
      (when (socket-pending-data sock-data)
	(setq output (concat (socket-pending-data sock-data) output))
	(socket-pending-data-set! sock-data nil))
      (let ((stream (make-string-input-stream output))
	    (point 0)
	    form)
	(catch 'out
	  (while t
	    (condition-case nil
		(setq form (read stream))
	      ((premature-end-of-stream end-of-stream)
	       (throw 'out))
	      ((invalid-read-syntax)
	       (error "Can't parse rpc message: %S" (substring output point))))

	    ;;(format standard-error "Parsed: %S\n" form)
	    (case (car form)
	      ((#t #f)
	       ;; Response
	       (unless (socket-result-pending sock-data)
		 (error "Spurious result on %s" socket))
	       ((socket-result-pending sock-data) form))
		
	      (t ;; Request
	       (let ((result (call-with-exception-handler
			      (lambda ()
				(let ((id (car form))
				      (args (cdr form)))
				  (cons '#t (apply (servant-ref id) args))))
			      (lambda (data)
				(cons '#f data)))))
		 ;;(format standard-error "Wrote: %S\n" result)
		 (write socket (prin1-to-string result)))))
	    (setq point (car stream))))
	(when (< point (length output))
	  (socket-pending-data-set! sock-data (substring output point))))))

  (define (wait-for-reponse socket)
    (let ((old-vector (socket-result-pending (socket-data socket)))
	  (result '()))
      (define (result-callback value)
	;;(format standard-error "Result: %S\n" value)
	(setq result value))
      (socket-result-pending-set! (socket-data socket) result-callback)
      (unwind-protect
	  (while (not result)
	    (accept-process-output))
	(socket-result-pending-set! (socket-data socket) old-vector))
      (if (eq (car result) '#t)
	  ;; success
	  (cdr result)
	;; exception raised
	(raise-exception (cdr result)))))

  (define (rpc-create-server)
    (unless listener-socket
      (setq listener-socket (socket-server nil nil rpc-socket-listener))))

  (define (rpc-destroy-server)
    (when listener-socket
      (close-socket listener-socket)
      (setq listener-socket nil)))

;;; servants

  (define (make-servant-id)
    (intern (concat #\= (number->string (current-utime) 36)
		    #\= (number->string (random) 36) #\=)))

  ;; map from ID->RPC-IMPL
  (define servant-table (make-table eq-hash eq))

  (define (servant-ref id) (table-ref servant-table id))

  (define (make-rpc-servant impl)
    (let ((id (make-servant-id)))
      (table-set servant-table id impl)
      id))

  (define (destroy-rpc-servant id)
    (table-unset servant-table id))

;;; proxies

  (define (make-rpc-proxy server port servant-id)
    (lambda args
      (let ((socket (server-socket server port)))
	;;(format standard-error "Wrote: %S\n" (cons servant-id args))
	(write socket (prin1-to-string (cons servant-id args)))
	(wait-for-reponse socket))))

;;; globally referenceable ids

  ;; returns a string
  (define (servant-id->global-id id)
    (or listener-socket (error "Need an opened RPC server"))
    (format nil "%s:%s:%s"
	    (socket-address listener-socket)
	    (socket-port listener-socket) id))

  (define (global-id->rpc-proxy id)
    (let ((bits (string-split ":" id)))
      (let ((server (nth 0 bits))
	    (port (string->number (nth 1 bits)))
	    (servant-id (intern (nth 2 bits))))
	(make-rpc-proxy server port servant-id))))

;;; initialization

  ;; ensure that the random numbers are random..
  (random t))
