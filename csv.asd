
(asdf:defsystem "csv"
  :description "csv: a system for handling CSV (comma separated variables) data files"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "packages")
                (:file "csv")
                (:file "fast-csv"))
  :depends-on  ("regex"))
