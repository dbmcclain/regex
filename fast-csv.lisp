;; fast-csv.lisp -- Fast, Simple, CSV File Reading
;;
;; This version is very fast because it does the minimum possible work
;; in reading CSV files, and extracting information on demand.
;;
;; It is assumed that no fields are quoted fields that contain the primary delimiter.
;; Primary delimiters (comma for CSV, tab for TSV files) are heuristically determined
;; from examination of the first line of the file, which is assumed to be the headings
;; line. Subsequent lines are assumed to contain data. Empty lines are discarded.
;;
;; These conditions are satisfied by nearly 99%+ of all the CSV files ever seen.
;; If your CSV/TSV file violates any of these conditions, then use the more elaborate
;; and general-purpose routines in CSV.LISP.
;;
;; DM/RAL  06/07
;; ----------------------------------------------------------------------------

(in-package fast-csv)

(defun collect-delimiter-positions (s delim &key count (start 0))
  (loop with  pos = start
        with  end = start
        while (and end
                   (or (null count)
                       (plusp count)))
        do    (setq end (position delim s :start pos))
        collect (or end (length s))
        when  end
        do    (setq pos (1+ end))
        when  count
        do    (decf count)
        ))

(defun collect-headers (s delim)
  (let ((pos (collect-delimiter-positions s delim)))
    (loop with start = 0
          for  p in pos
          collect (subseq s start p)
          do      (setq start (1+ p))
          )))

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
            when  (plusp (length line)) ;; elide empty lines
            collect line))
    ))

(defun decide-csv-or-tsv (line)
  ;;
  ;; count the number of commas and tabs in the list of lines.
  ;; If commas outnumber tabs then we probably have CSV. Else we
  ;; probably have TSV.
  ;;
  (let ((n-comma (count-if (um:curry #'char= #\,)   line))
	(n-tab   (count-if (um:curry #'char= #\Tab) line)))
    (if (> n-comma n-tab)
        #\,
      #\Tab)
    ))

(defstruct csv-info
  hdrs
  delim
  row-infos
  nlines
  lines
  ends-cache)

(defun collect-additional-ends (info column row line)
  (with-accessors ((ends-cache  csv-info-ends-cache)
                   (row-infos   csv-info-row-infos )
                   (delim       csv-info-delim     )) info
    (let* ((ends     (aref row-infos row))
           (new-ends (if ends
                         ;; we have some partial information
                         (let* ((nends     (length ends))
                                (more-ends (collect-delimiter-positions
                                            line delim
                                            :count (1+ (- column nends))
                                            :start (1+ (aref ends (1- nends)))
                                            )))
                           (concatenate 'vector ends more-ends))
                       
                       ;; else we never had any information -- build from scratch
                       (coerce
                        (collect-delimiter-positions
                         line delim
                         :count (1+ column)
                         :start 0)
                        'vector)))
           
           (unique-ends (gethash new-ends ends-cache nil)))
      (unless unique-ends
        (setf (gethash new-ends ends-cache) new-ends))
      (setf (aref row-infos row) (or unique-ends new-ends))
      )))

(defun ensure-column-of-row-exists (info column row line)
  ;; at this point row-info cannot be nil, but its contents might be...
  (with-accessors ((row-infos  csv-info-row-infos)) info
    (let ((ends (aref row-infos row)))
      (unless (< column (length ends))
        (collect-additional-ends info column row line))
      )))
          
(defun extract-data-of-column (info column reverse op)
  (with-accessors ((nlines    csv-info-nlines)
                   (lines     csv-info-lines)
                   (row-infos csv-info-row-infos)) info
    (let* ((nelm1 (1- nlines))
           (data  (make-array nlines)))
      (loop for line in lines
            for row from 0
            for revrow = (- nelm1 row)
            do
            (ensure-column-of-row-exists info column row line)
            ;; at this point ends is a vector of delimiter positions
            ;; that is at least as long as one greater than the column index
            (let* ((ends  (aref row-infos row))
                   (start (if (zerop column)
                              0
                            (1+ (aref ends (1- column)))
                            ))
                   (end   (aref ends column))
                   (datum (funcall op line start end)))
              (setf (aref data (if reverse revrow row)) datum)
              ))
      data
      )))

(defun get-column-of-hdr (hdr info)
  (position (um:mkstr hdr) (csv-info-hdrs info)
            :test #'string-equal))

(defun ensure-row-infos-exist (info column all)
  (with-accessors ((row-infos  csv-info-row-infos)
                   (nlines     csv-info-nlines   )
                   (lines      csv-info-lines    )) info
    (unless row-infos
      (setf row-infos (make-array nlines))
      (when all
        (loop for row from 0
              for line in lines
              do
              (collect-additional-ends info column row line)))
      )))

(defun get-numeric-field-op (pre-op)
  (if pre-op
      ;; we need to extract a subseq for the user's function
      (lambda (line start end)
        (read-from-string
         (funcall pre-op (subseq line start end))
         nil 0))
    
    ;; else we can avoid creating a subseq
    (lambda (line start end)
      (read-from-string line nil 0
                        :start start :end end))
    ))

(defun get-string-field-op (pre-op post-op numeric-p)
  (if numeric-p
      (let ((op (get-numeric-field-op pre-op)))
        (if post-op
            (um:compose post-op op)
          op))
    
    (if pre-op
        (if post-op
            (um:compose post-op pre-op #'subseq)
          (um:compose pre-op #'subseq))
      (if post-op
          (um:compose post-op #'subseq)
        #'subseq))
    ))

;; --------------------------------------------------------------
;; user accessible routines...
;;
(defun read-file (fname
                  &key
                  (ndrop 0)
                  &allow-other-keys)
  (let* ((lines      (nthcdr ndrop (read-lines fname)))
         (delim      (decide-csv-or-tsv (first lines)))
         (hdrs       (collect-headers (first lines) delim))
         (data-lines (rest lines)))
    (make-csv-info
     :hdrs       hdrs
     :delim      delim
     :row-infos  nil ;; a vector of vectors of delimiter positions
     :nlines     (length data-lines)
     :lines      data-lines
     :ends-cache (make-hash-table :test #'equalp)
     )))


(defun get-column (hdr info
                       &key
                       reverse
                       scrubber
                       (pre-op scrubber)
                       post-op
                       numeric-p
                       &allow-other-keys)
  ;; user should utilize keys :reverse and :op
  ;; the :num-op key is for our private use
  (let* ((column (get-column-of-hdr hdr info)))
    (when column
      (ensure-row-infos-exist info column :all)
      (extract-data-of-column info column reverse
                              (get-string-field-op pre-op post-op numeric-p))
      )))

(defun get-numeric-column (hdr info
                               &key
                               reverse
                               scrubber
                               (pre-op scrubber)
                               post-op
                               &allow-other-keys)
  (get-column hdr info
              :reverse   reverse
              :numeric-p t
              :pre-op    pre-op
              :post-op   post-op))


(defun get-field (hdr row info
                      &key
                      reverse
                      scrubber
                      (pre-op scrubber)
                      post-op
                      numeric-p
                      &allow-other-keys)
  ;; user should utilize keys :reverse and :op
  ;; the :num-op key is for our private use
  (with-accessors ((lines     csv-info-lines    )
                   (row-infos csv-info-row-infos)
                   (nlines    csv-info-nlines   )) info
    (let ((column (get-column-of-hdr hdr info)))
      (when column
        (ensure-row-infos-exist info column nil)
        (let* ((actual-row (cond ((numberp row) (if reverse
                                                    (- nlines row 1)
                                                  row))
                                 
                                 ((eq row :last) (if reverse
                                                     0
                                                   (1- nlines)))
                                 
                                 ((eq row :first) (if reverse
                                                      (1- nlines)
                                                    0))
                                 ))
               (line (nth actual-row lines)))
          (ensure-column-of-row-exists info column actual-row line)
          (let* ((ends  (aref row-infos actual-row))
                 (start (if (zerop column)
                            0
                          (1+ (aref ends (1- column)))
                          ))
                 (end   (aref ends column)))
            (funcall (get-string-field-op pre-op post-op numeric-p)
                     line start end)
            ))
        ))
    ))

(defun get-numeric-field (hdr row info
                              &key
                              reverse
                              scrubber
                              (pre-op scrubber)
                              post-op
                              &allow-other-keys)
  (get-field hdr row info
             :numeric-p t
             :pre-op    pre-op
             :post-op   post-op
             :reverse reverse))

