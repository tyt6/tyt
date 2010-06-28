;;;; -*- mode: lisp -*-
;;;;
;;;; This as an ASDF system for tyt
;;; -*- Lisp -*- mode

(defpackage #:tyt-system (:use #:cl #:asdf))
(in-package #:tyt-system)

(defclass acl-file (cl-source-file) ())
(defmethod source-file-type ((c acl-file) (s module)) "lisp")

(defsystem tyt
  :author "tyt"
  :licence "LLGPL"
  :default-component-class acl-file
  :components ((:file "tyt")
               )
  :version "0.0.1"
  :depends-on (scheme fare-utils fare-matcher
                      cl-ppcre split-sequence)
  :perform (load-op :after (op tyt)
                    (pushnew :tyt cl:*features*)))

