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

;; (rpc-create-server)		-- create rpc listener on a random port

;; (define (foo x) (+ x 42))

;; (define foo-id (make-rpc-servant foo))

;; `foo-id' is now a symbol that uniquely identifies the `foo' function
;; on this server. E.g. it may be something like `9s72fdln00-61vxd7'

;; To turn this into a globally valid id, use the servant-id->global-id
;; function:

;; (define foo-global-id (servant-id->global-id foo-id))

;; this creates a string, e.g.: "9s72fdln00-61vxd7@1.2.3.4:2000"


;; =Client=

;; (define proxy (global-id->rpc-proxy "9s72fdln00-61vxd7@1.2.3.4:2000"))

;; Now `proxy' is a function that when called marshals all its
;; arguments, sends them to the server, along with the unique id, and
;; waits for a result to be returned, which it then unmarshals and
;; returns


;; Unlike CORBA it's not possible to transparently pass object
;; references (proxies) over an RPC call, and have them work at the
;; other end. 

;; The solution is to convert the proxy to a global id, then pass that
;; over the RPC call, so that the other side can convert it into a new
;; proxy.

;; It's also possible to pass references from servers behind firewalls
;; (and thus can't create usable global ids). The solution here is to
;; pass the local servant-id to the remote server, which can then use
;; the remote-servant-id->rpc-proxy function. This knows that the given
;; servant id refers to the connection used to invoke the currently
;; executing rpc call


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
	    call-with-rpc-servant
	    rpc-proxy->global-id
	    rpc-proxy->servant-id
	    servant-id->global-id
	    remote-servant-id->global-id
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

  ;; The socket used to listen for connections to this server (or false)
  (define listener-socket nil)

  ;; The socket that was used to invoke the innermost called servant
  ;; implementation
  (define active-socket (make-fluid))

;;; connection cache

  ;; maps from (SERVER-NAME . PORT-NUMBER) -> SOCKET
  (define socket-cache (make-table equal-hash equal))

  ;; maps from SOCKET -> SOCKET-DATA
  (define socket-data-table (make-weak-table eq-hash eq))

  ;; Return the socket associated with SERVER:PORT. If there isn't one,
  ;; try to connect to the server
  (define (server-socket server port)
    (or (table-ref socket-cache (cons server port))
	(open-server server port)
	(error ("No connection with server %s:%d" server port))))

  (define (register-rpc-server socket #!key closable)
    "Add the connection SOCKET to the table of known rpc connections. If
CLOSABLE is true, then the socket could be closed and reopened simply
by knowing its address and port number."
    (let ((server (socket-peer-address socket))
	  (port (socket-peer-port socket)))
      (table-set socket-cache (cons server port) socket)
      (table-set socket-data-table socket (make-socket-data closable))))

  (define (deregister-rpc-server socket)
    "Remove SOCKET from the table of rpc connections."
    (let ((server (socket-peer-address socket))
	  (port (socket-peer-port socket)))
      (when (eq (server-socket server port) socket)
	(let ((data (socket-data socket)))
	  (when (socket-closable-p data)
	    (close-socket socket))
	  (table-unset socket-cache (cons server port))
	  (table-unset socket-data-table socket)))))

  ;; Return the data structure associated with SOCKET
  (define (socket-data socket) (table-ref socket-data-table socket))

;;; socket I/O

  (define (rpc-socket-listener master-socket)
    "The function that should be used to listen for connections on rpc
server sockets."
    (let (socket)
      (setq socket (socket-accept master-socket
				  (lambda (output)
				    (rpc-output-handler socket output))
				  (lambda ()
				    (deregister-rpc-server socket))))
      (register-rpc-server socket #:closable nil)
      socket))

  ;; Open an rpc connection to HOST:PORT
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
    "The function used to handle any OUTPUT from SOCKET."
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
				(let ((impl (servant-ref (car form)))
				      (args (cdr form)))
				  (unless impl
				    (error "No such RPC servant: %s"
					   (car form)))
				  (let-fluids ((active-socket socket))
				    (cons '#t (apply impl args)))))
			      (lambda (data)
				(cons '#f data)))))
		 ;;(format standard-error "Wrote: %S\n" result)
		 (write socket (prin1-to-string result)))))
	    (setq point (car stream))))
	(when (< point (length output))
	  (socket-pending-data-set! sock-data (substring output point))))))

  ;; Wait for an rpc response on SOCKET. Parse it and either return the
  ;; value or raise the exception
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
    "Start listening for rpc connections on the current machine"
    (unless listener-socket
      (setq listener-socket (socket-server nil nil rpc-socket-listener))))

  (define (rpc-destroy-server)
    "Stop listening for rpc connections on the current machine"
    (when listener-socket
      (close-socket listener-socket)
      (setq listener-socket nil)))

;;; servants

  ;; map from ID->RPC-IMPL
  (define servant-table (make-table eq-hash eq))

  ;; Create a new (unique) servant id
  (define (make-servant-id)
    (intern (concat (number->string (current-utime) 36)
		    #\- (number->string (random) 36))))

  ;; Return the servant implementation associated with ID
  (define (servant-ref id) (table-ref servant-table id))

  (define (make-rpc-servant impl)
    "Register the function IMPL as an rpc servant, and return the created
servant-id."
    (let ((id (make-servant-id)))
      (table-set servant-table id impl)
      id))

  (define (destroy-rpc-servant id)
    "Remove the servant with servant-id ID from the table of servants."
    (table-unset servant-table id))

  (define (call-with-rpc-servant impl callback)
    "Call the function CALLBACK with a single argument, the servant-id that
can be used to call the function IMPL. Once CALLBACK returns, the servant-id
becomes invalid."
    (let ((id (make-rpc-servant impl)))
      (unwind-protect
	  (callback id)
	(destroy-rpc-servant id))))

;;; proxies

  ;; magic object used to get information from proxies
  (define proxy-token (cons))

  ;; XXX shouldn't keep consing new proxies..
  (define (make-proxy server port servant-id)
    (let ((global-id (make-global-id server port servant-id)))
      (lambda args
	(if (eq (car args) proxy-token)
	    ;; when called like this, do special things
	    (case (cadr args)
	      ((global-id) global-id)
	      ((servant-id) servant-id))

	  ;; otherwise, just forward to the server
	  (let ((socket (server-socket server port)))
	    (write socket (prin1-to-string (cons servant-id args)))
	    (wait-for-reponse socket))))))

  (define (rpc-proxy->global-id proxy)
    "Return the globally-valid servant-id (a string) that can be used to
reference the RPC proxy function PROXY."
    (proxy proxy-token 'global-id))

  (define (rpc-proxy->servant-id proxy)
    "Return the unqualified servant-id (a symbol) that can be used to
reference the RPC proxy function PROXY."
    (proxy proxy-token 'servant-id))

;;; globally referenceable ids

  ;; Create the global servant id for ID@SERVER:PORT
  (define (make-global-id server port id)
    (format nil "%s@%s:%s" id server port))

  (define (servant-id->global-id id)
    "Return the globally referenceable RPC servant id for local servant id ID."
    (unless listener-socket
      (error "Need an opened RPC server"))
    (make-global-id (socket-address listener-socket)
		    (socket-port listener-socket) id))

  (define (remote-servant-id->global-id id)
    "Return the globally referenceable RPC servant id for the local servant
id ID (a symbol) associated with the invoker of the currently active RPC
request."
    (unless (fluid active-socket)
      (error "Not called from an RPC servant"))
    (make-global-id (socket-peer-address (fluid active-socket))
		    (socket-peer-port (fluid active-socket)) id))

  (define (global-id->rpc-proxy id)
    "Return a function that can be used to call the RPC associated with the
global servant id ID (a string)."
    (unless (string-looking-at "(.+)@(.+):(.+)" id)
      (error "Badly formed global RPC servant id: %s" id))
    (let ((servant-id (intern (expand-last-match "\\1")))
	  (server (expand-last-match "\\2"))
	  (port (string->number (expand-last-match "\\3"))))
      (make-proxy server port servant-id)))

;;; initialization

  ;; ensure that the random numbers are random..
  (random t))
