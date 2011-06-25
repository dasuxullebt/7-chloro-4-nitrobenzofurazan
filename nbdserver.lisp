#|
Copyright 2011 Christoph-Simon Senjak

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
|#

;;; requires: usocket

;; we could use constants here, but I do not like constants
(defparameter *nbd-init-magic* #(#x0 #x0 #x42 #x2 #x81 #x86 #x12 #x53))
(defparameter *nbd-request-magic* #(#x25 #x60 #x95 #x13))
(defparameter *nbd-reply-magic* #(#x67 #x44 #x66 #x98))

;; the ram-pool for experiments
(defvar *pool*)
(defun readfrompool (position length)
  (make-array (list length)
	      :element-type '(unsigned-byte 8)
	      :displaced-to *pool*
	      :displaced-index-offset position))
(defun writetopool (position length bytes)
  (replace *pool* bytes :start1 position :end2 length) T)
(defun run-rampool (port size &optional (pool (make-array (list size)
							  :element-type '(unsigned-byte 8) :initial-element 0
							  :adjustable nil)))
  (let
      ((*pool* pool))
    (start-single-server port size #'readfrompool #'writetopool)
    *pool*))

;; lowlevel stuff
(defun int-to-uint64-be (int)
  (map 'simple-vector
       #'(lambda (i) (mod (ash int (* -8 i)) 256))
       #(7 6 5 4 3 2 1 0)))

(defun int-to-uint32-be (int)
  (map 'simple-vector
       #'(lambda (i) (mod (ash int (* -4 i)) 256))
       #(3 2 1 0)))

(defun be-to-int (args)
  (let ((ret 0))
    (dolist (arg args)
      (setf ret (+ (* 256 ret) arg)))
    ret))

;; convenience
(defun write-sequences (str &rest seq)
  (write-sequence (apply #'concatenate 'simple-vector seq) str)
  (finish-output str))

(defun process-connection (stream size read-callback write-callback
			   buffer-size)
  ;; write initial header
  (write-sequences stream
		   (map 'vector #'char-code "NBDMAGIC")
		   *nbd-init-magic*
		   (int-to-uint64-be size)
		   #128(0))
  ;; event loop
  (let ((proceed t))
    (loop while proceed do
	 (let
	     ((buffer (make-array (list buffer-size)
				  :element-type '(unsigned-byte 8)
				  :initial-element 0))
	      (magic (loop for i to 3 collect (read-byte stream)))
	      (type (be-to-int (loop for i to 3 collect (read-byte stream))))
	      (handle (loop for i to 7 collect (read-byte stream)))
	      (from (be-to-int (loop for i to 7 collect (read-byte stream))))
	      (length (be-to-int (loop for i to 3 collect (read-byte stream)))))
	   (declare (ignore magic buffer))
	   (cond
	     ((> (+ length from) size) ; sanatize	      
	      (write-sequences stream
			       *nbd-reply-magic*
			       (int-to-uint32-be 1) ; Error
			       handle))
	     ((= type 0) ; read
	      (let ((bytes (funcall read-callback from length)))
		(cond
		  (bytes ; bytes were read
		   (write-sequences stream
				    *nbd-reply-magic*
				    (int-to-uint32-be 0) ; Ok
				    handle
				    bytes))
		  (t ; error
		   (write-sequences stream
				    *nbd-reply-magic*
				    (int-to-uint32-be 1) ; Error
				    handle)
		   #| RETURN HERE! |#
		   (setf proceed nil)))))
	     ((= type 1) ; write
	      (let ((write-buffer
		     (make-array (list length)
				 :element-type '(unsigned-byte 8)
				 :adjustable nil)))
		(assert (= (read-sequence write-buffer stream) length))
		(cond
		  ((funcall write-callback from length write-buffer)
		   ;; success
		   (write-sequences stream
				    *nbd-reply-magic*
				    (int-to-uint32-be 0) ; Ok
				    handle))
		  (t ;; fail
		   (write-sequences stream
				    *nbd-reply-magic*
				    (int-to-uint32-be 1) ; Error
				    handle)
		   #| RETURN HERE |#
		   (setf proceed nil)
		   ))))
	     (t ; end
	      #| RETURN HERE |#
	      (setf proceed nil)))))))

(defun start-single-server (port size read-callback write-callback
			    &optional (buffer-size 8192))
  (let*
      ((listen-socket (usocket:socket-listen "localhost" port :element-type '(unsigned-byte 8)))
       (socket (usocket:socket-accept listen-socket))
       (stream (usocket:socket-stream socket)))
    (block handler-ret
      (handler-bind ((error #'(lambda (&rest args)
				(return-from handler-ret args))))
	(process-connection stream size read-callback write-callback buffer-size)))
    (usocket:socket-close socket)
    (usocket:socket-close listen-socket)))

(defun start-server (port size read-callback write-callback
		     &optional (buffer-size 8192))
  "Documentation:
port: tcp-port to listen
size: bytesize of the device
read-callback: position -> length -> overwritable octet vector -> [octet vector | NIL]
called on read. should return NIL on error.
write-callback: position -> length -> octet vector -> generalized boolean
called on write. should return NIL on error, and true on success."
  (usocket:socket-server
   "localhost" port
   #'(lambda (stream) (process-connection stream size read-callback write-callback buffer-size))
   nil :element-type '(unsigned-byte 8)))