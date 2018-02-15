;; Avisere.lisp -- Read in and evaluate the Feature Vector Sets
;; DM/Avisere  06/04
;;
(defpackage "AVISERE"
  (:nicknames "AVI")
  (:use "COMMON-LISP")
  (:export
   ))

(in-package "AVISERE")

;; (lw:load-system "csv")
(ql:quickload :csv)
(ql:quickload :vmath)

(defclass <channel-moments-group> ()
  ((its-frm    :accessor its-frm  :initarg :frm)
   (its-m1     :accessor its-m1   :initarg :m1)
   (its-m2     :accessor its-m2   :initarg :m2)
   (its-m3     :accessor its-m3   :initarg :m3)
   (its-m4     :accessor its-m4   :initarg :m4)
   (its-gnd    :accessor its-gnd  :initarg :gnd)))

(defun read-hh-file (&optional fname)
  (format t "~%Reading From: ~A" fname)
  (let ((raw (csv:read-csv :hdr-lines 1 :fname fname)))
    (when raw
      (let ((grp (csv:get-group nil raw)))
        (labels ((get-numeric-column (hdg)
                   (map 'vector #'read-from-string
                        (csv:get-column hdg grp))))
          (make-instance '<channel-moments-group>
                         :frm  (get-numeric-column "FRME")
                         :m1   (get-numeric-column "M1")
                         :m2   (get-numeric-column "M2")
                         :m3   (get-numeric-column "M3")
                         :m4   (get-numeric-column "M4")
                         :gnd  (get-numeric-column "Ground Truth"))
          ))
      )))

;; NOTE: All CSV files must have the same identically spelled 
;; column headings. Use Emacs to ensure that they do...
(defvar *hh-path* "~/Avisere/CSV\ Data/")
(progn
  (setf grp-HH  (read-hh-file (um:mkstr *hh-path* "HH.csv")))
  (setf grp-HH2 (read-hh-file (um:mkstr *hh-path* "HH2.csv")))
  (setf grp-HL  (read-hh-file (um:mkstr *hh-path* "HL.csv")))
  (setf grp-HL2 (read-hh-file (um:mkstr *hh-path* "HL2.csv")))
  (setf grp-LH  (read-hh-file (um:mkstr *hh-path* "LH.csv")))
  (setf grp-LH2 (read-hh-file (um:mkstr *hh-path* "LH2.csv")))
  (setf grp-LL  (read-hh-file (um:mkstr *hh-path* "LL.csv")))
  (setf grp-LL2 (read-hh-file (um:mkstr *hh-path* "LL2.csv"))))
|#

;; (lw:compile-system "vmath" :load t)

(defun combine (truth &rest args)
  (apply #'map 'vector #'list truth args))

(defun select (grp score)
  (remove-if-not #'(lambda (key)
                     (= key score))
                 grp
                 :key #'first))

(defun showme (v1 v2 truth 
                  &rest other-args
                  &key 
                  (wid    0)
                  (xtitle "Var #1")
                  (ytitle "Var #2")
                  (title  "Var #1 vs Var #2")
                  &allow-other-keys)
  (let* ((all   (combine truth v1 v2))
         (pres  (select all 1))
         (npres (select all 0))
         (plotter #'sg:plot))
    (labels ((xs (grp)
               (map 'vector #'second grp))
             (ys (grp)
               (map 'vector #'third grp))
             (show (grp color)
               (apply plotter (xs grp) (ys grp)
                        :color color
                        :symbol sg:$sym-dot
                        :xtitle xtitle
                        :ytitle ytitle
                        :title  title
                        other-args)
               (setf plotter #'sg:oplot))
             (show-pres ()
               (show pres sg:$darkgreen))
             (show-npres ()
               (show npres sg:$red)))
      (sg:wset wid)
      (if (> (length pres) (length npres))
          (progn
            (show-pres)
            (show-npres))
        (progn
          (show-npres)
          (show-pres)))
      )))

(defun get-slot (grp sel)
  (ecase sel
    (:M1  (its-m1 grp))
    (:M2  (its-m2 grp))
    (:M3  (its-m3 grp))
    (:M4  (its-m4 grp))))

(defun get-slot-name (sel)
  (ecase sel
    (:M1 "M1")
    (:M2 "M2")
    (:M3 "M3")
    (:M4 "M4")))

(defun show-group (grp sel1 sel2 &key (wid 0))
  (let ((slot1 (get-slot grp sel1))
        (slot2 (get-slot grp sel2))
        (nam1  (get-slot-name sel1))
        (nam2  (get-slot-name sel2))
        (truth (its-gnd grp)))
    (showme slot1 slot2 truth
            :wid    wid
            :xtitle nam1
            :ytitle nam2
            :title  (um:mkstr nam1 " vs " nam2))))

(show-group grp-hh :m1 :m2 :wid 1)
(show-group grp-hh :m3 :m2 :wid 2)
(show-group grp-hh :m1 :m3 :wid 3)

;; ----------------------------------------------------
#|
(defun serialize-group (grp &optional (stream *standard-output*))
  (princ "(make-instance 'avi::<channel-moments-group>" stream)
  (princ "  :frm " stream)
  (princ (its-frm grp) stream)
  (princ "  :m1  " stream)
  (princ (its-m1 grp) stream)
  (princ "  :m2  " stream)
  (princ (its-m2 grp) stream)
  (princ "  :m3  " stream)
  (princ (its-m3 grp) stream)
  (princ "  :m4  " stream)
  (princ (its-m4 grp) stream)
  (princ "  :gnd " stream)
  (princ (its-gnd grp) stream)
  (princ ")" stream)
  (terpri stream))
|#

;; ----------------------------------------------------
#| code for test checkout
(loop for grp in (list grp-HH ;;grp-HH2 grp-HL grp-HL2
                       ;;grp-LH grp-LH2 grp-LL grp-LL2) 
                       ) do
      (sg:wset 0)
      (sg:plot (its-frm grp) (its-gnd grp)
               :color sg:$red  :symbol sg:$sym-dot
               :xtitle "Frame"
               :ytitle "Ground Truth"
               :title  "Frame & Ground Truth Overview")
      (showme (its-m1 grp) (its-m2 grp) (its-gnd grp)
              :wid   1
              :title "M1 vs M2"
              :xtitle "M1"
              :ytitle "M2")
      (showme (its-m3 grp) (its-m4 grp) (its-gnd grp)
              :wid    2
              :title "M3 vs M4"
              :xtitle "M3"
              :ytitle "M4"))
  
(setf x1 (its-m1 grp-HH))
(setf x2 (its-m2 grp-HH))
(setf tr (its-gnd grp-HH))
|#
