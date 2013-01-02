
;;
;; Computation of RE Firsts
;;

(defun flatten-firsts (lst)
  (let ((result nil)
	(cs (make-charset)))
    (labels

     ((do-flatten (lst)
		  (cond ((null lst))
			((or (charset-p lst)
			     (characterp lst))
			 (charset-add-chars cs lst))
			((keywordp (car lst))
			 (pushnew lst result :test #'equal))
			(t
			 (do-flatten (car lst))
			 (do-flatten (cdr lst))))))
     (do-flatten lst)
     (cond ((and result
		 (not (charset-empty-p cs)))
	    (list result cs))
	   (result result)
	   (t cs)))))

(defun charset-of-firsts (re)
  (flatten-firsts
   (get-firsts re (re-state re (nregex-start re)))))

(defun re-state (re ix)
  (aref (nregex-states re) ix))

(defmethod get-firsts ((re nregex) (state alt-state))
  (list (get-firsts re (re-state re (alt-state-alt state)))
	(get-firsts re (re-state re (alt-state-follow state)))))

(defmethod get-firsts ((re nregex) (state ref-state))
  (charset-of-firsts (regex (ref-state-refname state))))

(defmethod get-firsts ((re nregex) (state char-state))
  (list (char-state-ch state)))

(defmethod get-firsts ((re nregex) (state str-state))
  (list (char (str-state-str state) 0)))

(defmethod get-firsts ((re nregex) (state ccls-state))
  (list (ccls-state-ccls state)))

(defmethod get-firsts ((re nregex) (state iter-state))
  (get-firsts re (re-state re (iter-state-alt state))))

(defmethod get-firsts ((re nregex) (state plus-state))
  (get-firsts re (re-state re (plus-state-alt state))))

(defmethod get-firsts ((re nregex) (state simple-state))
  (get-firsts re (re-state re (simple-state-follow state))))

(defmethod get-firsts ((re nregex) (state any-state))
  (list :any))

;; ------------------------------------------------------

(when nil
      (define-lexer
	'((:number                    :number)
	  (:word                      :word)
	  (:string                    :string)
	  (:char                      :char)
	  ("/"                        :div)
	  ("*"                        :times)
	  ("+"                        :plus)
	  ("-"                        :minus)
	  ("%"                        :mod)
	  (("^" "^^" "**")            :expt)
	  ("&"                        :bit-and)
	  ("|"                        :bit-or)
	  ("^|"                       :bit-xor)
	  ("&&"                       :and)
	  ("||"                       :or)
	  ("++"                       :incf)
	  ("--"                       :decf)
	  (">>"                       :shr)
	  ("<<"                       :shl)
	  ((":" "->")                 :colon)
	  ("<"                        :lt)
	  ("<="                       :le)
	  (">="                       :ge)
	  (">"                        :gt)
	  ("=="                       :eq)
	  (("!=" "<>")                :ne)
	  ("."                        :dot)
	  (("=" ":=")                 :assign)
	  ("+="                       :plus-eq)
	  ("-="                       :minus-eq)
	  ("*="                       :times-eq)
	  ("/="                       :div-eq)
	  ("%="                       :mod-eq)
	  ("&="                       :and-eq)
	  ("|="                       :or-eq)
	  ("^="                       :xor-eq)
	  (","                        :comma)
	  ("("                        :lparen)
	  (")"                        :rparen)
	  ("["                        :lbrack)
	  ("]"                        :rbrack)
	  ("{"                        :lbrace)
	  ("}"                        :rbrace)
	  ("[["                       :aref)
	  ("]]"                       :endaref)
	  ("$"                        :lisp-esc)
	  ("~"                        :bit-not)
	  (:ws                        )
	  (:eol                       :eol)
	  ))
)

(defun define-parser (&key start states)
  (let ((ht (make-hash-table)))
    (dolist (state states)
	    (let ((nonterm (car state))
		  (reds (cdr state)))
	      (dolist (redux reds)
		      ;;(print redux)
		      (let ((key (if (consp redux)
				     (if (consp (car redux))
					 (caar redux)
				       (car redux))
				   redux)))
			(let ((old (gethash key ht nil)))
			  (setf (gethash key ht)
				(cons (cons nonterm (mklist redux)) old)))))))
    (loop for key being each hash-key of ht using (hash-value val) collect
	 (cons key (sort val #'(lambda (a b)
				 (cond ((and
					 (consp (cadr a))
					 (consp (cadr b)))
					(> (length (cadr a))
					   (length (cadr b))))
				       ((consp (cadr a))
					t)
				       ((consp (cadr b))
					nil)
				       (t t))))
	       ))))

(defun mklist (arg)
  (if (listp arg)
      arg
    (list arg)))

(let ((ht (make-hash-table)))
  
  (defun set-firsts (key val)
    (setf (gethash key ht) val))
  
  (defun get-firsts (key)
    (gethash key ht nil))
  
  (defun fillin-firsts ()
    (maphash #'fillin ht))

  (defun fillin (key val)
    (let ((lst nil))
      (set-firsts key :invalid-recursion)
      (dolist (item val)
	      (if (keywordp val)
		  (pushnew val lst)
		(let ((vals (fillin val (get-firsts val))))
		  (dolist (item vals)
			  (if (keywordp val)
			      (if (eq val :invalid-recursion)
				  (error "firsts recursion between ~A and ~A"
					 key vals)
				(pushnew val lst))
			    (error "can't happen in FILLIN"))))
		))
      (set-firsts key lst)))
  )

(defun define-parser (&key start states)
  (let ((lst nil)
	(ht  (make-hash-table)))
    (dolist (state states)
	    (set-firsts (car state) (gather-firsts (cdr state))))
    (fillin-firsts)
    (dolist (state states)
	    (push (define-reductions (car state) (cdr state)) lst))
    (nreverse lst)))

(defun gather-firsts (reductions)
  (let ((lst nil))
    (dolist (redux reductions)
	    (let ((firsts (get-reduction-first redux)))
	      (if (consp firsts)
		  (dolist (f firsts)
			  (pushnew f lst))
		(pushnew firsts lst))))
    lst))

(defun get-reduction-first (reduction)
  (if (consp reduction)
      (if (consp (car reduction))
	  (caar reduction)
	(car reduction))
    reduction))

(defun define-reductions (non-terminal reductions)
  (let ((lst nil))
    (dolist (redux reductions)
	    (push (define-reduction non-terminal redux) lst))
    (nreverse lst)))

(defun define-reduction (non-terminal reduction)
  (let ((redux (mklist (if (consp reduction)
		   (car reduction)
		 reduction)))
	(action (if (consp reduction)
		    (cadr reduction))))
    (let ((head nil)
	  (tail redux)
	  (lst nil))
      (while tail
	(push (list (append head (list '$) tail)
		    (let ((el (pop tail)))
		      (if tail
			  (list el
				'shift
				(append (setf head (append head (list el)))
					(list '$)
					tail))
			(list el 'reduce non-terminal action))))
	      lst))
      (nreverse lst))))
				  
(setf x
(define-parser
  :start '<expr>
  :states
  '((<if>        ((:if <expr> :then <progn-block>
		       :else <progn-block> :end)
		  `(if ,$2 ,$4 ,$6))
		 
		 ((:if <expr> :then <block> :end)
		  `(when ,$2 ,@$4))
		 
		 ((:if <expr> :else <block> :end)
		  `(unless ,$2 ,@$4)))
    
    (<progn-block> (<block>
		    (if $1
			`(progn ,@$1))))
    
    (<block>     nil
		 
		 ((<expr> <block>)
		 `(,$1 ,@$2)))
    
    (<while>     ((:while <expr> :do <block> :end)
		  `(while ,$2 ,@$4)))
    
    (<until>     ((:until <expr> :do <block> :end)
		  `(until ,$2 ,@$4)))
    
    (<expr>      <hier1>
		 
		 ((<expr> <addop> <term>)
		  `(,$2 ,$1 ,$3)) 	;left assoc
		 
		 <if>
		 
		 <while>
		 
		 <until>
		 )
    
    
    (<expr-list> (<expr>
		  `(,$1))
		 
		 ((<expr-list> :comma <expr>)
		  `(,@$1 ,$3)))
    
    (<hier1>     <hier2>
		 
		 (<expr-list>
		  `(progn ,@$1)))
    
    (<hier2>     <hier3>
		 
		 )
    
    (<term>      <factor>
		 
		 ((<term> <mulop> <factor>)
		  `(,$2 ,$1 ,$3))) ;left assoc

    (<expt>      ((<primary> :expt <expt>)
		  `(expt ,$1 ,$3)) ;right assoc
		 <primary>
		 )
    
    (<primary>   :word
		 :number
		 :char
		 :string
		 
		 ((:lbrack <expr-list> :rbrack)
		  `(vector ,@$2))
		 
		 ((:lbrace <expr-list> :rbrace)
		  `(list ,@$2))
		 
		 ((<primary> :lbrack <expr-list> :rbrack)
		  `(aref ,$1 ,@$3))
		 
		 ((<primary> :lparen <expr-list> :rparen)
		  `(,$1 ,@$3))
		 
		 ((<primary> :lparen :rparen)
		  `(,$1))
		 
		 ((:incf <primary>)
		  `(incf ,$2))
		 
		 ((:decf <primary>)
		  `(decf ,$2))
		 
		 ((<primary> :incf)
		  `(incf-after ,$1))
		 
		 ((<primary> :decf)
		  `(decf-after ,$1))
		 
		 ((<primary> :dot :word)
		  `(,$3 ,$1))
		 
		 ((:lparen <progn-block> :rparen)
		  $2))
    
    ))
)

(when nil
(defun parse (states start inp)
  (let ((state (assoc start states)))
    (multiple-value-bind (reduced-p result-form inp-remainder)
			 (parse-reduce states (cdr state) inp nil)
       (if (and reduced-p
		(null inp-remainder))
	   (values (car state) result-form)))))


(defun parse-reduce (states reductions inp form)  
  (do ((reds reductions (cdr reds))
       (rslt (parse-reduce-one states (car reds) inp form)
	     (parse-reduce-one states (car reds) inp form)))
      (rslt rslt)))

(defun get-input-token (inp-item)
  (if (consp inp-item)
      (car inp-item)
    inp-item))

(defun matches-p (states reduction inp form)
  (let ((cursym  (get-input-token (car inp)))
	(pattern (mklist (if (consp reduction)
			     (car reduction)
			   reduction)))
	(action  (if (consp reduction)
		     (cadr reduction))))

    (cond ((null pattern)
	   ;; we have an epsilon match
	   )

	   ((and (keywordp (car pattern))
		 (eq (car pattern) cursym)
		 (matches-p (cdr pattern) (cdr inp)))
	    ;; we have a match
	    )

	    ((and (parse-reduce (lookup-reduction (car pattern) states) inp)
		  (matches-p (cdr pattern) (inp-remainder)))
	     ;;we have a match
	     )

	    (t ;no match
	     nil))))


(defun reduce-input (inp)
  (if inp
      (let* ((token (pop inp))
	     (cursym (get-token-type token)))
	(tagbody
	 lookup
	 (let ((reductions (gethash cursym state-table nil)))
	   (cond ((null reductions)
		  (return-from reduce-input nil))
		 ((consp reductions)
		  (dolist (red reductions)
			  (cond ((null red)
				 (
				  )
				 (t
				  (setf cursym reductions)
				  (go lookup)))
				))
		  )))
	 ))))
)

(when nil
      ;; ------------------------------------------------------
      ;; Fast Compiled Closure Representation of Regexp's
      ;;
      (defparameter *qcregex-dictionary* (make-hash-table))

      (defun defqcregex (key)
	(setf (gethash key *qcregex-dictionary*) (qcr key)))

      (defun qcre (key str)
	(funcall (gethash key *qcregex-dictionary*
			  #'(lambda (str)
			      (error "Invalid QCRE key")))
		 str))

      (defun debug (fmt &rest args)
	(apply 'format t fmt args))

      (let (groups
	    slen
	    str
	    str-start
	    closures)

      (defun qcr-compile-group-state (gix alt follow)
	#'(lambda (pos)
	    (debug "~&Group ~A Pos: ~A" gix pos)
	    (let ((newpos (funcall (aref closures alt) pos)))
	      (when newpos
		    (setf (aref groups gix) (list pos newpos))
		    (or (and follow
			     (funcall (aref closures follow) newpos))
			newpos)))))

      (defun qcr-compile-alt-state (alt follow)
	#'(lambda (pos)
	    (debug "~&Alt Pos: ~A" pos)
	    (or (funcall (aref closures alt) pos)
		(if follow
		    (funcall (aref closures follow) pos)
		  pos))))

      (defun qcr-compile-bos-state (follow)
	#'(lambda (pos)
	    (debug "~&BOS Pos: ~A" pos)
	    (and (= pos str-start)
		 (or (and follow
			  (funcall (aref closures follow) pos))
		     pos))))

      (defun qcr-compile-eos-state ()
	#'(lambda (pos)
	    (debug "~&EOS Pos: ~A" pos)
	    (and (= pos slen)
		 pos)))

      (defun qcr-compile-any-state (follow)
	#'(lambda (pos)
	    (debug "~&ANY Pos: ~A" pos)
	    (and (< pos slen)
		 (or (and follow
			  (funcall (aref closures follow) (1+ pos)))
		     (1+ pos)))))

      (defun qcr-compile-iter-state (ntimes alt follow)
	#'(lambda (pos)
	    (debug "~&Iter (~A) Pos: ~A" pos)
	    (do ((newpos pos (funcall (aref closures alt) newpos))
		 (count  ntimes (1- count)))
		((or (zerop count)
		     (null newpos))
		 (and newpos
		      (or (and follow
			       (funcall (aref closures follow) newpos))
			  newpos))))))

      (defun qcr-compile-ref-state (refname follow)
	#'(lambda (pos)
	    (debug "~&Ref (~S) Pos: ~A" refname pos)
	    (let* ((ref  (gethash refname *qcregex-dictionary* nil))
		   (rslt (if ref
			     (funcall ref str
				      :start    pos
				      :end      slen
				      :anchored t)
			   (match-regex refname str
					:start    pos
					:end      slen
					:anchored t))))
	      (debug "~&rslt = ~S" rslt)
	      (and rslt
		   (let ((newpos (second (aref rslt 0))))
		     (or (and follow
			      (funcall (aref closures follow) newpos))
			 newpos))))))

      (defun qcr-compile-str-state (pat lpat follow)
	#'(lambda (pos)
	    (debug "~&STR ~S Pos: ~A" pat pos)
	    (let ((end (+ pos lpat)))
	      (and (<= end slen)
		   (string= pat str
			    :start2 pos
			    :end2   end)
		   (or (and follow
			    (funcall (aref closures follow) end))
		       end)))))

      (defun qcr-compile-char-state (ch follow)
	#'(lambda (pos)
	    (debug "~&Char (~S) Pos: ~A" ch pos)
	    (and (< pos slen)
		 (char= ch (char str pos))
		 (or (and follow
			  (funcall (aref closures follow) (1+ pos)))
		     (1+ pos)))))

      (defun qcr-compile-ccls-state (ccls follow)
	#'(lambda (pos)
	    (debug "~&CCLS ~A Pos: ~A" ccls pos)
	    (and (< pos slen)
		 (charset-contains-char-p ccls (char str pos))
		 (or (and follow
			  (funcall (aref closures follow) (1+ pos)))
		     (1+ pos)))))


      (defun qcr-compile-cre (ngroups code follow)
	#'(lambda (string &key (start 0) (end (length str)) anchored)
	    (labels
	     ((scan-iter (pos)
			 (if (<= pos slen)
			     (let ((offset (funcall (aref closures follow) pos)))
			       (if offset
				   (list pos offset)
				 (scan-iter (1+ pos)))
			       ))))

	     (setf groups (make-array ngroups)
		   str    string
		   slen   (min (length str) end)
		   str-start start
		   closures code)

	     (if anchored
		 (let (offset)
		   (if (and (<= start slen)
			    (setf offset (funcall (aref closures follow) start)))
		       (progn
			 (setf (aref groups 0) (list start offset))
			 groups)))
	       (if (setf (aref groups 0) (scan-iter start))
		   groups)))))

      (defun qcr (re)
	(let ((cre    (regex re)))
	  (if cre
	      (let* ((states (nregex-states cre))
		     (code   (make-array (length states))))

		(loop for ix from 0 below (length states) do
		     (setf (aref code ix)
			   (let ((node (aref states ix)))
			     (cond

			      ((group-state-p node)
			       (qcr-compile-group-state
				(group-state-ngroups node)
				(group-state-alt node)
				(group-state-follow node)))

			      ((abstract-alt-state-p node)
			       (qcr-compile-alt-state
				(abstract-alt-state-alt node)
				(abstract-alt-state-follow node)))

			      ((bos-state-p node)
			       (qcr-compile-bos-state
				(bos-state-follow node)))

			      ((eos-state-p node)
			       (qcr-compile-eos-state))

			      ((any-state-p node)
			       (qcr-compile-any-state
				(any-state-follow node)))

			      ((iter-state-p node)
			       (qcr-compile-iter-state
				(iter-state-count node)
				(iter-state-alt node)
				(iter-state-follow node)))

			      ((ref-state-p node)
			       (qcr-compile-ref-state
				(ref-state-refname node)
				(ref-state-follow node)))

			      ((str-state-p node)
			       (qcr-compile-str-state
				(str-state-str node)
				(length (str-state-str node))
				(str-state-follow node)))

			      ((char-state-p node)
			       (qcr-compile-char-state
				(char-state-ch node)
				(char-state-follow node)))

			      ((ccls-state-p node)
			       (qcr-compile-ccls-state
				(ccls-state-ccls node)
				(ccls-state-follow node)))

			      (t
			       (error "Can't happen in QCR: unknown state ~S"
				      node)))
			     )))

		(qcr-compile-cre (nregex-ngroups cre) code (nregex-start cre)))
	    (error "Invalid RE: ~S" re))
	  )))

)

