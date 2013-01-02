;;; -*- Mode:Lisp; Package:USER; Base:10 -*-
;;;
;;; This code was written by:
;;;
;;;    David B. McClain <dbmcclain@acm.org>
;;;    MCFA
;;;    Tucson, AZ  85750
;;;
;;; If you modify this code, please comment your modifications
;;; clearly and inform the author of any improvements so they
;;; can be incorporated in future releases.
;;;
;;; nregexp.lisp - 10/97 DM/MCFA -- revised to avoid GOTO's,
;;;                Cleaned up GROUP behavior considerably,
;;;                Simplified & added DESCRIBE.
;;;
;;;                My 10/96 attempt at a Lisp based regular expression
;;;                parser.
;;;
;;; This regular expression parser operates by taking a
;;; regular expression and breaking it down into a vector
;;; NFA state machine.
;;;
;;; Each NFA state begins with a keyword or a character or string
;;; or bitvector representing a character class.
;;;
;;; The second element of each NFA state is the index of the
;;; follow state for the current match.
;;;
;;; Some state types may have a third or fourth element depending on
;;; their specific needs.
;;; ---------------------------------------------------------------------
;;; First we create macros to help debug the beast

;(require "CHARSETS" #F"/lisp-lib/regex/charsets.lsp")

(in-package "NREGEX")

(defvar *regex-debug* nil)		; Set to nil for no debugging code

(defmacro info (message &rest args)
  (if *regex-debug*
      `(format *standard-output* ,message ,@args)))

;; --------------------------------------------------------------------

(defstruct nregex			;basic compiled regex structure
  (ngroups)				;nbr of paren groups + 1 for whole
  (start)				;index of start state
  (states))				;vector of states

;;
;; Define the substates of a compiled regex
;; (NOTE: the use of :include in a structure spec
;;  implies a class heritage... (BE CAREFUL!) )
;;
(defstruct simple-state
  (follow))

  (defstruct (bos-state ( :include simple-state)))

  (defstruct eos-state)

  (defstruct (any-state ( :include simple-state)))

  (defstruct (abstract-alt-state ( :include simple-state ))
    (alt))

      (defstruct (alt-state ( :include abstract-alt-state)))

      (defstruct (plus-state ( :include abstract-alt-state)))

      (defstruct (star-state ( :include abstract-alt-state)))

      (defstruct (question-state ( :include abstract-alt-state)))

      (defstruct (group-state ( :include abstract-alt-state))
	(group-index))

  (defstruct (iter-state ( :include simple-state))
    (alt)
    (count))

  (defstruct (ref-state ( :include simple-state))
    (refname))

  (defstruct (str-state ( :include simple-state))
    (str))

  (defstruct (char-state ( :include simple-state))
    (ch))

  (defstruct (ccls-state ( :include simple-state))
    (ccls))

;;
;; Printers -- a printer method accepts an object, a stream,
;; and returns the object.
;;
(defmethod describe-object ((re nregex) stream)
  (format stream "~&#<~A> is a Compiled Regular Expression (CRE)"
	  (type-of re))
  (format stream "~&  ~A Groups" (nregex-ngroups re))
  (format stream "~&  ----------")
  (let ((states (nregex-states re))
	(start  (nregex-start  re)))
    (dotimes (i (length states))
	     (format stream "~& ~A~A: "
		     (if (= i start) "*" " ")
		     i)
	     (describe-state (aref states i) stream)))
  (values))

(defmethod describe-state ((state simple-state) stream)
  (if (simple-state-follow state)
      (format stream "next -> ~A"
	  (simple-state-follow state))
    (format stream "Terminate")))

(defmethod describe-state :before ((state ref-state) stream)
  (format stream "Ref -> ~S, "
	  (ref-state-refname state)))

(defmethod describe-state :before ((state alt-state) stream)
  (format stream "Alt -> ~A, "
	  (alt-state-alt state)))

(defmethod describe-state :before ((state char-state) stream)
  (format stream "Char= ~S, "
	  (char-state-ch state)))

(defmethod describe-state :before ((state str-state) stream)
  (format stream "String= ~S, "
	  (str-state-str state)))

(defmethod describe-state :before ((state ccls-state) stream)
  (format stream "CCLS ~A, "
	  (charset-printform (ccls-state-ccls state))))

(defmethod describe-state :before ((state bos-state) stream)
  (format stream "BOS, "))

(defmethod describe-state ((state eos-state) stream)
  (format stream "EOS, "))

(defmethod describe-state :before ((state any-state) stream)
  (format stream "ANY, "))

(defmethod describe-state :before ((state group-state) stream)
  (format stream "Group #~A, call -> ~A, "
	  (group-state-group-index state)
	  (group-state-alt state)))

(defmethod describe-state :before ((state plus-state) stream)
  (format stream "Kleene(+) -> ~A, "
	  (plus-state-alt state)))

(defmethod describe-state :before ((state star-state) stream)
  (format stream "Kleene(*) -> ~A, "
	  (star-state-alt state)))

(defmethod describe-state :before ((state question-state) stream)
  (format stream "Kleene(?) -> ~A, "
	  (question-state-alt state)))

(defmethod describe-state :before ((state iter-state) stream)
  (format stream "Iterate(~A) -> ~A, "
	  (iter-state-count state)
	  (iter-state-alt   state)))

;;
;; REGEX -- compiles or looks up a compiled regex
;;
(defvar *regex-dictionary* (make-hash-table))   ;holds named regex's

(defun regex (re)
   ;;
   ;; Compile to a regexp list if a string,
   ;; else check that it is a regexp list.
   ;; If it is a symbol then look it up in the *regexp-dictionary*
   ;;
   (cond ((null re)
          nil)

         ((symbolp re)
          (regex (gethash re *regex-dictionary* nil)))

         ((stringp re)
          (compile-regex re))

         ((characterp re)
          (compile-regex (string re)))

         ((nregex-p re)
          re)

         (t (error "Invalid regex pattern: ~S" re))))


;;
;; MATCH-REGEX -- matches a regex against a string
;;
(defun match-regex (re str
                     &key (start 0)
                     (end (length str))
                     (anchored nil))
   ;;
   ;; Apply the regex pattern against the string.
   ;; The pattern may be precompiled, or it may also be a string.
   ;; The returned value is NIL on no match, or a vector of
   ;; (start end) pairs for each subgroup in the pattern.
   ;; The zeroth element is the entire matched range of string chars.
   ;;
   (let ((cre    (regex re))
         slen	 ;cached string length
         groups  ;vector of group indices
         states  ;vector of subpattern states
         pat)
      (labels
       (
;	(match-iter (subpat pos)
;		     (format t "~&enter: ~A ~A" subpat pos)
;		     (let ((rslt (xmatch-iter subpat pos)))
;		       (format t "~&exit: ~A" rslt)
;		       rslt))

        (match-iter (subpat pos)
           ;;
           ;; Internal processor for regex matching
           ;;
           (do ((subpat subpat (let ((follower (simple-state-follow subpat)))
                                 (if follower
                                     (aref states follower)))))
               ((or (null subpat) (null pos)) pos)

           (labels
             ((follow-alt (pos)
		 (match-iter (aref states
				   (abstract-alt-state-alt subpat))
			     pos)))

	     ;;(format t "~&pos: ~A  state: ~A" pos subpat)
             (cond

	      ((group-state-p subpat)
	       (let ((newpos (follow-alt pos)))
		 (when newpos
		       (setf (aref groups
				   (group-state-group-index subpat))
			     (list pos newpos)))
                 (setf pos newpos)))

	      ((abstract-alt-state-p subpat)
	       (let ((newpos (follow-alt pos)))
                 (if newpos
                     (return-from nil newpos))))

	      ((bos-state-p subpat)
	       (unless (= pos start)
                 (return-from nil nil)))

	      ((eos-state-p subpat)
               (return-from nil (and (= pos slen)
		                             pos)))

	      ((any-state-p subpat)
	       (unless (< pos slen)
                 (return-from nil nil))
               (incf pos))

	      ((iter-state-p subpat)
	       (do ((i (iter-state-count subpat) (1- i))
		    (newpos pos (follow-alt newpos)))
		   ((or (zerop i)
			(null newpos))
		    (progn 
                      (unless newpos
                        (return-from nil nil))
                      (setf pos newpos)))))
			   
	      ((ref-state-p subpat)
	       (let ((rslt (match-regex (ref-state-refname subpat) str
					:start    pos
					:end      slen
					:anchored t)))
		 (unless rslt
                   (return-from nil nil))
                 (setf pos (second (aref rslt 0)))))

	      ((str-state-p subpat)
	       (let* ((pat  (str-state-str subpat))
		      (lpat (length pat))
		      (end  (+ pos lpat)))
		 (unless (and (<= end slen)
		              (string= pat str
			               :start2 pos
			               :end2   end))
                   (return-from nil nil))
                 (setf pos end)))

	      ((char-state-p subpat)
	       (unless (and (< pos slen)
		            (char= (char-state-ch subpat)
                                   (char str pos)))
                 (return-from nil nil))
               (incf pos))

	      ((ccls-state-p subpat)
               (unless (and (< pos slen)
		            (charset-contains-char-p (ccls-state-ccls subpat)
					             (char str pos)))
                 (return-from nil nil))
               (incf pos))

	      (t
	       (error "Unrecognized subpattern type: ~S" subpat))
	      ))))

         (scan-iter (pos)
	   (if (<= pos slen)
	       (let ((offset (match-iter pat pos)))
		 (if offset
		     (list pos offset)
		   (scan-iter (1+ pos)))
		 ))))

        (when cre
           (setf groups (make-array (nregex-ngroups cre))
                 states (nregex-states cre)
                 pat    (aref states (nregex-start cre))
                 slen   (min (length str) end))   ;cached string length
           ;;
           ;; A little optimization for anchored matching...
           ;; don't bother scanning entire length after first failure.
           ;;
           (if anchored
              (let (offset)
                 (if (and (<= start slen)
                          (setf offset (match-iter pat start)))
                    (progn
                      (setf (aref groups 0) (list start offset))
                      groups)))
              ;;
              ;; Non-anchored matching begins here...
              ;;
              (if (setf (aref groups 0) (scan-iter start))
                 groups)))
        )))

;; ----------------------------------------------------------------------
;;
;; Define the compiler representation of connected states
;;
(defstruct state-rep
  (first)
  (last))

;;
;; COMPILE-REGEX -- compiles a string into a regex NFA pattern structure
;;
(defun compile-regex (str)
   (let ((slen        (length str))	;cached pattern string length
	 start				;start pos of subpat
         (pos         0)		;current scan position
         (group-index 0)		;highest group encountered
         (state-index 0)
         (state-array (make-array (* 2 (length str))))
         available-states)
      (labels
        (
	 ;; -------------------------------------------------
	 ;; State management...
	 ;;

         (get-node (state)
           (aref state-array state))

         (get-first-node (state-rep)
           (get-node (state-rep-first state-rep)))

         (get-last-node (state-rep)
           (get-node (state-rep-last state-rep)))

         (set-state-node (state node)
           (setf (aref state-array state) node))

         (free-state (state)
	   (if (= state (1- state-index))
	       (decf state-index)
	     (push state available-states)))

	 (next-state ()
	   (prog1
	       state-index
	       (incf state-index)))

         (assign-state (node)
           ;;
           ;; get or re-use a state array slot
           ;; fill it with the new state info
           ;; return a new state pair
           ;;
           (let ((new-state (or (pop available-states)
                                (next-state))))
	     (set-state-node new-state node)
	     (make-state-rep :first new-state
			     :last  new-state)))

	 (simple-node-value (node)
	   ;;
	   ;; return the single char or str held by
	   ;; a simple state.
	   ;;
	   (if (char-state-p node)
	       (char-state-ch node)
	     (str-state-str node)))

	 (char-or-str-state-p (node)
           (or (str-state-p node)
               (char-state-p node)))

         (attach-expr (expr1 expr2)
	   (cond
	    ((and (char-or-str-state-p (get-last-node  expr1))
		  (char-or-str-state-p (get-first-node expr2))
                  (eq (get-first-node expr2) 
                      (get-last-node expr2)))
	     ;;
	     ;; Optimization: if the last state of expr1
	     ;; and the first state of expr2 are both simple
	     ;; (i.e., char or str states) then coalesce them
	     ;; into one string state at the tail of expr1,
	     ;; and discard expr2.
	     ;;
	     (set-state-node
	      (state-rep-last expr1)
	      (make-str-state
	       :str (charsets:concatenate-strings
		     (simple-node-value (get-last-node  expr1))
		     (simple-node-value (get-first-node expr2)))
	       :follow (simple-state-follow (get-last-node expr1))
	       ))
	     (free-state (state-rep-first expr2)))

	    ((question-state-p (get-last-node expr1))
	     ;;
	     ;; Question states need to have both the follow of the
	     ;; question state and the follow of the tail of its
	     ;; alt expression chain fixed up.
	     ;;
	     (let* ((qnode (get-last-node expr1))
		    (lnode (get-node (question-state-follow qnode))))
	       (setf (question-state-follow qnode) (state-rep-first expr2)
		     (simple-state-follow   lnode) (state-rep-first expr2)
		     (state-rep-last        expr1) (state-rep-last  expr2))))

	    (t
	     ;;
	     ;; Otherwise, just attach the head of expr2 to the
	     ;; tail of expr1. Abosorb the total chain into expr1.
	     ;;
	     (setf (simple-state-follow (get-last-node expr1))
		   (state-rep-first expr2)
		   (state-rep-last expr1) (state-rep-last expr2))))

	   expr1)

         (append-state (expr node)
	   (attach-expr expr (assign-state node)))

	 (terminate (term)
	     ;;
	     ;; If the last node of the term is a QUESTION closure,
	     ;; then its follow has to be nil'd, as it now points
	     ;; to the final state of its alt path. Return the term.
	     ;;
	     (let ((last-node (get-last-node term)))
	       (if (question-state-p last-node)
		   (setf (simple-state-follow last-node) nil))
	       term))

	 ;; -------------------------------------------------
	 ;; Character scanning...
	 ;;

	 (eos ()
	    (>= pos slen))

	 (this-char ()
	   (char str pos))

	 (get-char ()
	   (prog1
	       (this-char)
	       (incf pos)))

	 (unget-char ()
	   (decf pos))

	 (have-char (ch)
	   (and (not (eos))
		(char= (this-char) ch)))

         (expect (ch)
           (if (have-char ch)
	       (get-char)
	     (error "Invalid Regex: '~C' expected at position ~A" ch pos)))

	 (end-of-subpat-p ()
	   (or (eos)
	       (have-char #\| )
	       (have-char #\) )))

	 ;; -------------------------------------------------
	 ;; Composite tokens...
	 ;;

         (get-count ()
	    ;;
	    ;; We are just past the opening "^" for a pattern count
	    ;; We should be looking at a digit character.
	    ;;
	    ;; (This routine is also used to parse an octal encoded char)
	    ;;
	    (let ((start pos))
	      (until (or (eos)
			 (not (digit-char-p (this-char))))
		     (get-char))
	      (if (= start pos)
		  (error "Pattern count expected at position ~A" pos))
              (read-from-string (subseq str start pos))))

         (escaped ()
           (if (eos)
	       #\\
	     (let ((ch (get-char)))
	       (case ch
		     (#\n #\Linefeed)
		     (#\r #\Newline)
		     (#\f #\Page)
		     (#\t #\Tab)
		     (#\e #.(code-char #o33))
		     (#\b #\Backspace)
		     (t
		      (if (digit-char-p ch)
			  (let ((*read-base* 7))
			    (unget-char)
			    (code-char (get-count)))
			ch))
		     ))))

         (advance-to-unescaped-char (ch)
	    (until (or (eos)
		       (have-char ch))
		   (if (have-char #\\)
		       (get-char))
		   (unless (eos)
			   (get-char))))

         (make-ccls ()
	    ;; at this point pos points just past "[" within CCLS spec.
	    (let ((start (1- pos)))
	      (advance-to-unescaped-char #\] )
	      (expect #\] )
	      (make-charset (subseq str start pos))))


	 (get-refname ()
	    ;; at this point we are just past the opening "{"
	    ;; scan to the closing "}" and use the resulting
	    ;; symbol as a refname
            (let ((start pos))
	      (advance-to-unescaped-char #\} )
	      (expect #\} )
	      (read-from-string (subseq str start (1- pos)))))

	 ;; -------------------------------------------------
	 ;; Factors, Terms, and Expressions...
	 ;;

         (syntax-error ()
           (error "Invalid regex syntax at position ~A" pos))

         (get-suffix (factor)
	   (if (eos)
	       factor
	     (case (get-char)
		   
		   ;;
		   ;;  ---[ITER(n)]---
		   ;;         |
		   ;;        [RE]
		   ;;
		   (#\^		;simple repeat factor N times
		    (assign-state
		     (make-iter-state
		      :alt   (state-rep-first factor)
		      :count (get-count))))
		   
		   ;;
		   ;;  ---x--[RE]--[ALT (+)]---
		   ;;     |            |
		   ;;     +------------+
		   ;;
		   (#\+		;one or more times
		    (append-state
		     factor
		     (make-plus-state
		      :alt (state-rep-first factor))))
		   
		   ;;
		   ;;  ---x----[ALT (*)]---
		   ;;     |        |
		   ;;     +--[RE]--+
		   ;;
		   (#\*		;zero or more times (Kleene closure)
		    (append-state
		     factor
		     (make-star-state
		      :alt (state-rep-first factor)))
		    (setf (state-rep-first factor)
			  (state-rep-last  factor))
		    factor)
		   
		   ;;
		   ;;  ---[ALT (?)]----x--
		   ;;         |        |
		   ;;         +--[RE]--+
		   ;;
		   (#\?		;zero or one times
		    (assign-state
		     (make-question-state
		      :follow (state-rep-last  factor)
		      :alt    (state-rep-first factor))))
		   
		   (t
		    (unget-char)
		    factor)
		   )))

         (get-factor ()
           ;;
           ;; A regex factor is a single matching element with a possible
           ;; repeat indication.
           ;;
	   (let ((ch (get-char)))
	     (case ch

	       ((#\| #\) #\] #\})	; invalid start chars
		(unget-char)
		(syntax-error))

	       (#\(			;group start
		(let* ((gix (incf group-index)) ;in case subexpr has groups
		       (factor (assign-state
				(make-group-state
				 :group-index gix
				 :alt   (state-rep-first (get-expression))))))
		  (expect #\) )
		  (get-suffix factor)))

	       (#\^			;left anchor
		(if (= start (1- pos))	;only if at front of pattern
		    (assign-state (make-bos-state))
		  (assign-state (make-char-state :ch #\^))))

	       (#\$			;right anchor
		(if (end-of-subpat-p)	;only if at end of pattern
		    (assign-state (make-eos-state))
		  (assign-state (make-char-state :ch #\$))))

	       (#\.			;match any character
		(get-suffix (assign-state (make-any-state))))

	       (#\{			;start of an indirect pattern ref
		(get-suffix (assign-state
			     (make-ref-state
			      :refname (get-refname))
			     )))

	       (#\[			;start of a character class (CCLS)
		(get-suffix (assign-state
			     (make-ccls-state
                              :ccls (make-ccls))
			     )))

	       (#\\			;escaped character
		(get-suffix (assign-state
			     (make-char-state
                              :ch (escaped))
			     )))

	       (t			;else just the character
		(get-suffix (assign-state
			     (make-char-state
			      :ch ch))))
	     ))) 

         (get-term ()
           ;;
           ;; A term is a sequence of regex factors
           ;;
	   (setf start pos)
	   (let ((term (get-factor)))
	     (until (end-of-subpat-p)
		    (attach-expr term (get-factor)))
	     (terminate term)))

         (get-expression ()
           ;;
           ;; An expression is a term or an alternation of terms
           ;;
           (let ((expr (get-term)))
	     (while (have-char #\| )
	       (get-char)
	       (setf expr (assign-state
			   (make-alt-state
			    :alt    (state-rep-first expr)
			    :follow (state-rep-first (get-term)))
			   )))
	     expr)))

	;; ------------------------------------------------------
	;; Entry point processing
	;;

        (let ((rslt (get-expression)))
	  (unless (eos)
		  (syntax-error))
	  (make-nregex
	   :ngroups (1+ group-index)
	   :start   (state-rep-first rslt)
	   :states  (subseq state-array 0 state-index)))
        )))

;; ------------------------------------------------------------------
;; Translator for simple regex's to canonical regex's.
;; These are the simple regex's used to describe filename patterns.
;;
;; "*" --> any number of any chars (0 or more)
;; "?" --> one of any char
;; character classes match against one character
;; anything else matches literally
;;
(defun simple-regex (str)
  (let ((re "")
	(pos 0)
	(slen (length str)))

    (labels
     ((append-re (&rest strs)
	  (setf re (apply #'charsets:concatenate-strings re strs)))

      (get-char ()
	  (prog1
	      (char str pos)
	      (incf pos))))

     (while (< pos slen)
       (let ((ch (get-char)))
	 (case ch
	       (#\\
		(if (< pos slen)
		    (append-re #\\ (get-char))
		  (append-re #\\ )))

	       (#\?
		(append-re #\.))

	       (#\*
		(append-re ".*"))

	       ((#\. #\+ #\( #\) #\{ #\})
		(append-re #\\ ch))

	       (t
		(append-re ch))
	       ))))
    (compile-regex re)))

;; --------------------------------------------------------------------
;; Enable the #M"xxx" syntax for compiled regex's
;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |reader-for-#M| (inp schar scnt)
    (declare (ignore schar scnt))
    `(nregex:regex ,(read inp)))
    
  (set-dispatch-macro-character #\# #\M #'|reader-for-#M|))


;; ----------------------------------------------------------------
;; (defregex sym pat) to add a named pattern to the system list
;; Parameter pat can legitimately be any of character, string,
;; compiled regex, or another symbol for further indirection.
;;
(defun defregex (sym pat)
   (setf (gethash sym *regex-dictionary*) pat))

;;
;; Some starter patterns -- NOTE: ordering is significant within
;; a regexp with alternatives.
;;
(defregex :digit   #M"[0-9]")
(defregex :odigit  #M"[0-7]")
(defregex :xdigit  #M"[0-9a-fA-F]")
(defregex :bdigit  #M"[01]")
(defregex :alpha   #M"[a-zA-Z_]")
(defregex :alnum   #M"{:alpha}|{:digit}")
(defregex :word    #M"{:alpha}{:alnum}*")
(defregex :integer #M"{:digit}+")
(defregex :onum    #M"0{:odigit}*")
(defregex :xnum    #M"0(x|X){:xdigit}*")
(defregex :bnum    #M"0(b|B){:bdigit}*")
(defregex :frac    #M"{:integer}\\.{:integer}?|{:integer}?\\.{:integer}")
(defregex :expon   #M"(e|E|d|D)(+|-)?{:integer}")
(defregex :fnum    #M"{:frac}{:expon}|{:frac}")
(defregex :number  #M"{:xnum}|{:bnum}|{:fnum}|{:onum}|{:integer}")
(defregex :ws      #M"[\\n\\r\\t\\f ]")
(defregex :nws     #M"[\\t\\f ]")
(defregex :not-ws  #M"[^\\n\\r\\t\\f ]")
(defregex :cr      #M"\\r")
(defregex :lf      #M"\\n")
(defregex :eol     #M"{:cr}*{:lf}{:cr}*")
(defregex :line    #M"[^\\r\\n]*")
(defregex :char    #M"'[^\\\\]'|'\\\\.'")
(defregex :string  #M"\"([^\\\\]|\\\\.)*\"")

(provide "NREGEX")

;; -- end of nregex.lisp -- ;;
