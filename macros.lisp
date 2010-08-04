(defmacro test-macro(x)
  `(if 1 ,x))

(defmacro defmacro* (name vars symbols &body body)
  `(defmacro ,name ,vars
     ,(macroexpand `(fare-matcher::with-gensyms ,symbols
                      ,@body))))

;; これを呼ぶ側で  (require 'cl-interpol)を必要とします
;; readerが始めに呼ばれます。無理
;;(defmacro with-interpol (&body body)
;;  `(progn (cl-interpol:enable-interpol-syntax)
;;          (prog1 (progn ,@body)
;;            (cl-interpol:disable-interpol-syntax))))