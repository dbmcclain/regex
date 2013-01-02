
(defpackage charsets
  (:use useful-macros common-lisp)
  (:export
   #:charset
   #:make-charset
   #:charset-add-chars
   #:charset-remove-chars
   #:charset-p
   #:charset-contains-char-p
   #:charset-empty-p
   #:charset-printform
   #:concatenate-strings))

(defpackage nregex
  (:use useful-macros common-lisp charsets)
  (:export
   #:defregex
   #:regex
   #:match-regex
   #:simple-regex
   #:*regex-dictionary*))

(defpackage csv
  (:use common-lisp useful-macros)
  (:import-from nregex
   #:defregex
   #:regex
   #:match-regex)
  (:export
   #:read-file
   #:get-group
   #:get-column))

(defpackage fast-csv
  (:use common-lisp)
  (:nicknames fcsv)
  (:export
   #:read-file
   #:get-column
   #:get-numeric-column
   #:get-field
   #:get-numeric-field))

;; -----------------------------------------------------------------
;;
(defsystem regex
  (:package CL-USER
   :default-pathname (translate-logical-pathname "PROJECTS:LISP;regex;"))
  :members (("useful-macros" :type :system)
            "charsets"
            "nregex")
  :rules
  ((:in-order-to :compile :all
    (:caused-by (:compile :previous))
    (:requires (:load :previous)))))

(defsystem csv
  (:package CL-USER
   :default-pathname (translate-logical-pathname "PROJECTS:LISP;regex;"))
  :members (("regex" :type :system)
            "csv"
            "fast-csv")
  :rules
  ((:in-order-to :compile :all
    (:caused-by (:compile :previous))
    (:requires (:load :previous)))))

