
(asdf:defsystem "regex"
  :description "regex: a system for NDFA regular expression matching"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2008 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "packages")
                (:file "charsets")
                (:file "nregex"   :depends-on ("charsets")))
  :depends-on  ("useful-macros"))
