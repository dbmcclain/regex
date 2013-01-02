;; -*- Mode:Lisp; Package:USER; Base:10 -*-
;;
;; csv.lsp -- Comma Separated Textfile (CSV) File Reader
;;
;; This module reads CSV files line by line and returns a list of CSV groups
;; with each line split into separate items within each group.
;; A CSV group is identified as all those lines between lines containing 
;; only '(' and ')'. Empty lines are elided.
;;
;; The routine attempts to guess whether to use CSV (comma) or
;; TSV (tab) splitting.
;;
;; The returned result from READ-CSV is an association list of groups,
;; with each group containing an association list of columns identified by
;; column heading strings. Each column heading is accompanied by the
;; column vector of strings or numbers, depending on the data type declared
;; in the line following the column headers.
;;
;; DM  10/96
;; Updated DM/RAL 02/07
;; ----------------------------------------------------------------------------------

;(require "NREGEX" #F"/lisp-lib/regex/nregex.lsp")

(in-package "CSV")

(unless (regex :csv)
	;;
	;; A regular expression that matches either:
	;;   1. A run of characters not containing commas, 
	;;      up to the next comma or the end of the line.
	;;   2. A single or double quoted string containing any characters
	;;      up to a matching close-quote. Any embedded quotes must be 
	;;      doubled up. These quoted fields are expected to be terminated
	;;      by either a comma or the end of the line.
	;;
	;; Remember that when a match is successful, the 0th pair in the
	;; returned vector contains the span of the entire match.
	;; This pattern is set up so that the second pair matches
	;; everything except the trailing comma.
	;; Quoted strings are left that way so that a "dequoter" can
	;; determine the type of operation to perform.
	;;
	(defregex :dquote-field #M"\"(([^\"]|\"\")*)\"")
	(defregex :squote-field #M"'(([^']|'')*)'")
	(defregex :csv
	  #M"{ws}*({:dquote-field}|{:squote-field}|([^,]*)){ws}*(,|$)")
        #|
	(defregex :tsv
	  #M"({:dquote-field}|{:squote-field}|([^\\t]*))(\\t|$)")
        |#
        (defregex :tsv
                  #M"{:ws}*({:dquote-field}|{:squote-field}|{:not-ws}+)"))


(defun read-lines (&optional fname)
  ;;
  ;; Return a list of text lines from the file
  ;;
  (lw:when-let (fnm (or fname 
	             (capi:prompt-for-file
	              "Select File to Read"
	              :filters '("CSV Files" "*.csv;*.tsv"
                                             "All Files" "*.*"))
	             ))
    (with-open-file (fp fnm)
      (loop for line = (read-line fp nil :eof)
            until (eq line :eof)
            while line
            collect line))
    ))


(defun dequote (str
                &aux
                (slen (length str))
                delim)
  ;;
  ;; Remove opening and trailing quote chars
  ;; and change doubled-up interior quotes to single ones.
  ;; Quoted strings are assumed to have a matching trailing quote.
  ;;
  (if (and (>= slen 2) ; string must be at least 2 chars long
           ;; and first char must be single or double quote
           (member (setf delim (char str 0)) '(#\' #\")))
      
      (let ((dbl-delim (case delim
                         (#\"  "\"\"")
                         (#\'  "''"))
                       ))

        (labels ((strip-double-delims (str start)
                   (um:if-let (pos (search dbl-delim str :start2 start))
                              (strip-double-delims
                               (remove-if (constantly t) str
                                          :start pos
                                          :count 1)
                               (1+ pos))
                              str)))
          
          (strip-double-delims
           (string-trim (list delim) str)
           0)
          ))
    ;; else - just return the black trimmed string
    (string-trim (list #\Tab #\Space) str)
    ))

;; Use hashed string lookup to minimize the creation of garbage?
(defparameter *unique-string-table*
  (make-hash-table :test #'string=))

(defun uniqueify (str)
  (let ((unique-str (gethash str *unique-string-table* nil)))
    (unless unique-str
      (setf (gethash str *unique-string-table*)
            (setf unique-str str)))
    unique-str))

(defun split-line (pat str
                       &aux (slen (length str)))
  ;;
  ;; Split a string into separate fields using the 'CSV regular expression
  ;;
  (labels ((field-at (start)
             ;; returns a list (extracted-string next-position)
             (um:if-let (field (and (< start slen)
                                    (match-regex pat str
                                                 :start    start
                                                 :anchored t)))
                        ;; (s1 e1) from match-regex refer to the entire string
                        ;; (s2 e2) refer to an individual substring
                        (destructuring-bind ((s1 e1) (s2 e2) &rest others)
                            (coerce field 'list)
                          (declare (ignore s1 others))
                          (list (uniqueify (dequote (subseq str s2 e2)))
                                e1))
                        
                        (list nil slen))))
    
    (loop for (f e) = (field-at 0) then (field-at e)
          until (null f)
          collect f)
    ))

(defun split-lines (lines &key (type :csv))
  ;;
  ;; Split each line into separate CSV fields,
  ;; discarding lines that are devoid of any data.
  ;;
  (let ((pat (case type
               (:csv (regex :csv))
               (:tsv (regex :tsv))
               (t    (regex (decide-csv-or-tsv lines)))
               )))
    
    (loop for line in lines
          for fields = (split-line pat line)
          unless (every (um:curry #'string= "") fields)
          collect fields)
    ))

(defun decide-csv-or-tsv (lines)
  ;;
  ;; count the number of commas and tabs in the list of lines.
  ;; If commas outnumber tabs then we probably have CSV. Else we
  ;; probably have TSV.
  ;;
  (let ((n-comma 0)
	(n-tab   0))
    (dolist (line lines)
      (incf n-comma (count-if (um:curry  #'char=  #\, #|c|#) line))
      (incf n-tab   (count-if (um:rcurry #'member #|c|# '(#\Space #\Tab)) line)))
    (if (> n-comma n-tab)
        :csv
      :tsv)
    ))
		  
(defun tableize (g hdr-lines
                   &aux (nrows (- (length g) hdr-lines)))
  ;;
  ;; Turn a group of split lines into a 2-D array of strings
  ;; This is not really a 2-D array, but rather a list of associations
  ;; between column heading strings and the column vectors of data.
  ;; Each column vector contains as many elements as there are 
  ;; lines in the group following the obligatory header and data type
  ;; strings..
  ;;
  (unless (plusp nrows)
    (error "Invalid CSV section"))
  (labels
      ((make-clean-vector (n l)
         (let ((v (make-array n :initial-element "")))
           (loop for ix of-type fixnum from 0 below n
                 for str in l
                 when str
                 do (setf (aref v ix) str))
           v))
       
       (trim-trailing-blank-entries (row)
         (um:if-let (pos (position-if (um:curry #'string/= "" #|str|#) row :from-end t))
                    (subseq row 0 (1+ pos))
                    row)))
    
    (let* ((g     (mapcar #'trim-trailing-blank-entries g))
           (ncols (reduce #'max (mapcar #'length g)))
           ;;
           ;; gv is an array of row vectors
           ;;
	   (gv    (make-array nrows))
           ;;
           ;; Grab the column headings and their data types.
           ;; (mapping missing elements to the empty string)
           ;;
	   (hdgs  (if (plusp hdr-lines)
                      (make-clean-vector ncols (pop g))
                    (coerce
                     (loop for ix of-type fixnum from 1 to ncols
                           collect (mkstr "col" ix))
                     'vector)))
           ;;
           ;; Second line should be names of data types, e.g., STRING or NUMBER
           ;;
	   (typs  (if (> hdr-lines 1)
                      (make-clean-vector ncols (pop g))
                    (map 'vector (constantly "STRING") hdgs))))

      ;; Discard extraneous header lines
      (loop for ix of-type fixnum from 2 below hdr-lines do (pop g))
      
      ;;
      ;; Now fill the initial gv vector with row vectors,
      ;; mapping missing elements to the empty string
      ;;         
      (dotimes (row nrows)
        (setf (aref gv row) (make-clean-vector ncols (pop g))))
      
      ;;
      ;; Next we reshape to a vector of columns
      ;; No point doing it for columns without headings
      ;; Convert the numeric columns to numbers and leave
      ;; the strings as they are. We end of with a list of associations
      ;; between the column heading strings and their columns of
      ;; data.
      ;;
      (loop for col of-type fixnum from 0 below ncols
            for hdg across hdgs
            when (string/= "" hdg)
            collect
            (let ((isnum (string-equal (aref typs col) "NUMBER"))
                  (v     (make-array nrows)))
              
              (dotimes (row nrows)
                (let ((item (aref (aref gv row) col)))
                  (setf (aref v row)
                        (if isnum
                            (let ((val (read-from-string item nil :eof)))
                              (if (numberp val) val 0))
                          item)
                        )))

              (cons hdg v)))
      )))

(defun groupize (l hdr-lines)
   ;;
   ;; Clump split lines into groups delimited by lines
   ;; containing only opening and closing parens.
   ;;
   (let* ((groups            (make-collector))
          (subgroup          (make-collector))
          group-name
          (section-separator "SECTION")
          (len-separator     (length section-separator))
          (sep-ignores       " -_:"))
      
      (labels ((collect-group ()
                 (if-let (items (collector-contents subgroup))
                         (collector-append-item groups (cons group-name items))
                         ))
               
               (prefixp (pref str
                              &aux
                              (plen (length pref))
                              (slen (length str)))
                 (and (>= slen plen)
                      (string-equal str pref
                                    :start1 0 :end1 plen))
                 ))
        
        (dolist (v l)
          
          (cond ((string= (first v) "(")
                 (collect-group)
                 (setf group-name (second v)))
                
                ((prefixp section-separator (first v))                 
                 (collect-group)
                 (setf group-name (or (second v)
                                      (string-left-trim sep-ignores
                                                        (subseq (first v) 
                                                                len-separator)))))
                
                ((string/= (first v) ")")
                 (collector-append-item subgroup v))
                ))
        
        (collect-group)
        
        (mapcar #'(lambda (g)
                    (cons (first g)
                          (tableize (rest g) hdr-lines)))
          (collector-contents groups))
        )))


(defun read-file (&key fname (hdr-lines 2) (skip-lines 0) (type :csv))
  ;;
  ;; Read and decipher a CSV file
  ;; Return the grouped, split lines, and dequoted fields
  ;;
  ;; The returned value is an association list of groups
  ;; whose values are lists of column a-lists.
  ;;
  (lw:when-let (lines (read-lines fname))
    (groupize (split-lines
               (um:drop skip-lines lines)
               :type type)
              hdr-lines)
    ))


(defun get-group (id glist)
  ;; either strings or symbols can be used for id's
  ;; courtesy of string-equal
  (rest (assoc id glist :test #'string-equal)))

(defun get-column (hdg clist)
  ;; either strings or symbols can be used for hdg's
  ;; courtesy of string-equal
  (rest (assoc hdg clist :test #'string-equal)))

;(provide "CSV")


#|
(defparameter x (read-csv-file #P"tst.tsv"))
(defparameter x1 (get-group "ffs" x))
(defparameter x2 (get-group "data" x))

(pprint (mapcar #'car x2))
("Filename" "File Type" "Azimuth" "Elevation" "Vis Dest. Directory" 
   "IR1 Dest. Directory" "IR2 Dest. Directory" "File Version" 
   "Attr Created" "Data Created" "Operator" "Sensor" "Chopper Freq." 
   "IR1 40001" "IR2 40001" "IR1 Voffset" "IR2 Voffset" "IR1 Int. Time" 
   "IR2 Int. Time" "Vis Int. Time" "IR1 Vramp" "IR2 Vramp" "IR1 Vgate" 
   "IR2 Vgate" "Source Temp." "Target Size" "Test Trans" "Time Recorded" 
   "Vis X Location" "Vis Y Location" "IR1 X Location" "IR1 Y Location" 
   "IR2 X Location" "IR2 Y Location" "Location Origin" "Rec Size" 
   "X Size" "Y Size" "N Frames" "HDAS Commands" "IR1" "IR2" "Vis")

(defun xx1 (&optional name)
   (if (null name)
      (setf name (ask-user-for-choice-from-list 
                  "Select a data column..."
                  (sort (mapcar #'car x1) #'string-lessp))))
   (print name)
   (pprint (get-column name x1)))

(defun xx2 (&optional name)
   (if (null name)
      (setf name (ask-user-for-choice-from-list 
                  "Select a data column..."
                  (sort (mapcar #'car x2) #'string-lessp))))
   (print name)
   (pprint (get-column name x2)))

|#
;; -- end of csv.lisp -- ;;
