;; -*- Mode:Lisp; Package:USER; Base:10 -*-
;;
;; charsets.lsp -- sets of characters
;;
;; DM/HMSC 10/97
;; -------------------------------------------------------------------

(in-package "CHARSETS")

(defclass charset ()
  ((bits :accessor charset-bits
	 :initarg :bits
	 :initform (make-array 256
			       :element-type 'bit
			       :initial-element 0))))

(defun make-charset (&optional str)
  (let ((cs (make-instance 'charset)))
    (if str
	(charset-add-chars cs str)
      cs)))

(defmethod print-object ((cs charset) stream)
  (format stream "~&#<~S ~A>"
	  (type-of cs)
	  (charset-printform cs))
  cs)

(defmacro sbit-zerop (bits ix)
  `(zerop (sbit ,bits ,ix)))

(defun number-of-ones (cs)
  (let ((bits (charset-bits cs)))
    (- 256 (loop for i from 0 below 256 count (sbit-zerop bits i)))))

(defun number-of-zeros (cs)
  (let ((bits (charset-bits cs)))
    (loop for i from 0 below 256 count (sbit-zerop bits i))))

(defun charset-empty-p (cs)
  (zerop (number-of-ones cs)))

(defun charset-p (obj)
  (eq (class-of obj) (find-class 'charset)))

(defun charset-char-printform (ch)
  (case ch
	( #\newline   "\\\\r")
	( #\linefeed  "\\\\n")
	( #\tab       "\\\\t")
	( #\page      "\\\\f")
	( #\backspace "\\\\b")
	( #\escape    "\\\\e")
	( #\]         "\\]"  )
	( #\\         "\\\\" )
	( #\-         "\\-"  )
	(otherwise
	 (if (graphic-char-p ch)
	     ch
	   (let ((*print-base* 16))
	     (format nil "\\\\x~A" (char-int ch)))))
	))

#+LISPWORKS
(defun concatenate-strings (&rest strs)
  (apply #'um:mkstr strs))

(defun charset-printform (cs)
  (let* ((n1 (number-of-ones cs))
	 (n0 (- 256 n1))
	 (chars '("[") )
	 (notform (> n1 n0))
	 (start 0)
	 (state :start)
	 (bits  (charset-bits cs)))
    (if notform
	(push "^" chars))
    (loop for i from 0 below 256 do
	 (let ((have (or
		      (and notform
			   (sbit-zerop bits i))
		      (and (not notform)
			   (not (sbit-zerop bits i))))))
	   (case state
		 ( :start
		   (when have
			 (setf state :collecting
			       start i)
			 (push (charset-char-printform
				(code-char i))
			       chars)))
		 
		 ( :collecting
		   (unless have
			   (setf state :start)
			   (unless (= 1 (- i start))
				   (if (> (- i start) 2)
				       (push "-" chars))
				   (push (charset-char-printform
					  (code-char (1- i)))
					 chars))))
		 )))
    (push "]" chars)
    (apply 'concatenate-strings (reverse chars))
    ))

(defmethod charset-add-chars ((cs charset) (ch character))
  (setf (sbit (charset-bits cs) (char-int ch)) 1)
  cs)

(defmethod charset-remove-chars ((cs charset) (ch character))
  (setf (sbit (charset-bits cs) (char-int ch)) 0)
  cs)

(defun make-ccls (str)
  ;;
  ;; Start of a range operation.
  ;; Generate a bit-vector that has one bit per possible character
  ;; and then on each character or range, set the possible bits.
  ;;
  ;; If the first character is carat then invert the set.
  ;;
  (let ((slen (length str))
	(pos  1))

    (labels

     ((escaped ()
	 (if (>= pos slen)
	     #\\
	   (let ((ch (char str pos)))
	     (incf pos)
	     (case ch
		   (#\n #\Linefeed)
		   (#\r #\Newline)
		   (#\f #\Page)
		   (#\t #\Tab)
		   (#\e #.(code-char #o33))
		   (#\b #\Backspace)
		   (t
		    (if (digit-char-p ch)
			(do ((start pos))
			    ((or (>= pos slen)
				 (not (digit-char-p (char str pos))))
			     (code-char (read-from-string
					(subseq str (1- start) pos))))
			    (incf pos))
		      ch))
		   )))))
      
;;     (unless (and (char= (char str (1- slen)) #\])
;;		  (char/= (char str (- slen 2)) #\\))
;;	     (error "Invalid charset syntax"))
    
     (let* ((invert (char= (char str pos) #\^))
	    (bitstring (make-array 256
				   :element-type 'bit
				   :initial-element
				   (if invert 1 0)))
	    (set-char (if invert 0 1)))
       (if invert (incf pos))
       (do ()
	   ((or (and (>= pos slen)
		     (error "Invalid charset syntax"))
		(char= (char str pos) #\]))
	    bitstring)
	   (cond ((and (char=  (char str (1+ pos)) #\-)
		       (char/= (char str (+ pos 2)) #\]))
		  (if (>= (char-int (char str pos))
			  (char-int (char str (+ 2 pos))))
		      (error "Invalid range \"~A-~A\".
                                  Ranges must be in acending order"
			     (char str pos) (char str (+ 2 pos))))
		  (do ((j (char-int (char str pos)) (1+ j)))
		      ((> j (char-int (char str (+ 2 pos))))
		       (incf pos 2))
		      (setf (sbit bitstring j) set-char)))
		 (t
		  (let ((ch (char str pos)))
		    ;;
		    ;; If the character is quoted then find out what
		    ;; it should have been
		    ;;
		    (incf pos)
		    (if (char= ch #\\ )
			(setf ch (escaped)))
		    (setf (sbit bitstring (char-int ch)) set-char)))
		 ))
       ))))
       
(defmethod charset-add-chars ((cs charset) (str string))
  (if (char= (char str 0) #\[)
      (charset-add-chars cs (make-ccls str))
    (loop for ch across str do
	 (charset-add-chars cs ch)))
  cs)

(defmethod charset-remove-chars ((cs charset) (str string))
  (if (char= (char str 0) #\[)
      (charset-remove-chars cs (make-ccls str))
    (loop for ch across str do
	 (charset-remove-chars cs ch)))
  cs)

(defmethod charset-add-chars ((cs charset) (bits bit-vector))
  (bit-ior (charset-bits cs) bits t)
  cs)

(defmethod charset-remove-chars ((cs charset) (bits bit-vector))
  (bit-andc2 (charset-bits cs) bits t)
  cs)

(defmethod charset-add-chars ((cs charset) (cs2 charset))
  (bit-ior (charset-bits cs) (charset-bits cs2) t)
  cs)

(defmethod charset-remove-chars ((cs charset) (cs2 charset))
  (bit-andc2 (charset-bits cs) (charset-bits cs2) t)
  cs)

(defun charset-contains-char-p (cs ch)
  (not (sbit-zerop (charset-bits cs) (char-int ch))))

(provide "CHARSETS")

;; --- end of charsets.lisp --- ;;

