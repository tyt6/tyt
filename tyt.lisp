;; -*- mode:lisp; encoding:utf-8; -*-

(defpackage :tyt.base
  (:use :common-lisp :fare-matcher :scheme
        )
  (:import-from :fare-utils with-gensyms)
  (:export
   #:intern+ #:intern++ #:describe+

   ;; not-pairの時引数を取る
   #:car* #:cdr* #:cadr* #:cddr* #:caar* #:cdar* #:caddr* #:cadddr*
   ;; #:first* #:second* #:third* #:fourth*

   ;;; macros
   ;; #:defmacro*  複雑なので、loadしてもらう。
   #:load-macro
   #:!
   #:the-all
   
   #:let-s #:let-c #:let-h #:let-a
   
   #:dolist* #:fold* #:map*

   #:fold-plist
   #:map-tree #:map-list-tree

   ;; rxmatch
   #:rxbase #:rxscanner
   #:rxmatch-lite #:rxmatchs-lite
   #:rxmatchs
   #:rxmatchi #:rxmatchsi
   ;; #:rxmatch-values
   
   ;; #:hash-table
   #:nub-ht #:nub-ht1 #:nub-ht-cons #:nub-ht-cons1
   #:totalize #:totalize! #:make-compare
   #:take% #:take%-by-hash-table
   #:take-topn #:take-topn-assoc
   #:program->string-list

   ;; p*p like
   #:number-format 
   
   #:map-tree #:map-tree2 #:fold-tree #:fold-tree2
   #:map-list-tree
   
   #:sort-alist
   #:random-string
   ))

;; tyt-no-package.lisp に drop do版など

;; --------------------------------
(common-lisp:in-package :tyt.base)

;; (declaim (maybe-inline cadr* caar* cddr* cdar*))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (proclaim '(optimize (speed 0)(safety 0) (debug 3) (compilation-speed 3) (space 0)))
  (declaim (inline car* cdr*))
 
  ;;  :execute       インタプリタにロードするときに評価する
  ;;  :compile-toplevel バイトコンパイルするときに評価する
  ;;  :load-toplevel バイトコンパイル済みのファイルをロードするときに評価する 

  (defun intern+ (sym &optional package)
    (if package (intern (symbol-name sym) package) sym))
  (defun intern++ (sym &optional package)
    (if package (intern (symbol-name sym) package)
        (intern (symbol-name sym))))
  
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))
  
  (defun describe+ (symbol)
    (with-output-to-string (*standard-output*)
      (handler-bind
          ((error (lambda (e) (declare (ignorable e))(return-from describe+ ""))))
        (describe symbol))))

  ;; 拾いもん
  (defmacro print-form-and-results (form)
    `(format t "~&~A --> ~S~%" (write-to-string ',form) ,form))

  (defun car* (x &optional (def nil))
    (if (consp x) (car x) def))
  
  (defun cdr* (x &optional (def nil))
    (if (consp x) (cdr x) def))
  
  ;;letで一回一回bindするより速い
  (defun cadr* (x &optional (def nil))
    (if (and (consp x) (consp (cdr x)))
        (cadr x)
        def))

  (defun cddr* (x &optional (def nil))
    (if (and (consp x) (consp (cdr x)))
        (cddr x)
        def))

  (defun caar* (x &optional (def nil))
    (if (and (consp x) (consp (car x)))
        (caar x)
        def))

  (defun cdar* (x &optional (def nil))
    (if (and (consp x) (consp (car x)))
        (cdar x)
        def))

  (defun caddr* (x &optional (def nil))
    (car* (cddr* x nil) def))

  (defun cadddr* (x &optional (def nil))
    (cadr* (cddr* x nil) def))

  ;; あまり上手く動かないマクロをこちらへ。
  ;; 公開するが自分専用ライブラリなので勘弁してもらう。
  (defmacro load-macro ()
    `(load (merge-pathnames "share/lib/lisp/tyt/macros.lisp" (user-homedir-pathname))))

  (defmacro the-all ( type op &rest rest)
    `(the ,type (,op ,@(mapcar (^ (x) `(the ,type ,x)) rest))))

  ;; (defun-alias first* car*)
  ;; (defun-alias second* cadr*)
  ;; (defun-alias third* caddr*)
  ;; (defun-alias fourth* cadddr*)
 
   (defun let-ref-org (ref var-list ref-able body)
    (labels ((make-ref (ref obj var)
               (umatch var
                 ( (var key default) `(,var (,ref ,obj ,key ,default)))
                 ( (var key) `(,var (,ref ,obj ,key)))
                 ( (var) `(,var (,ref ,obj (quote ,var))))
                 (_  `(,var (,ref ,obj (quote ,var)))))))
      (with-gensyms (%ref-able)
        `(let ((,%ref-able ,ref-able))
           (let ,(mapcar (cut make-ref ref %ref-able <>)
                         (if (consp var-list) var-list (list var-list)))
             ,@body)))))

   (defun let-struct-org (struct-name var-list ref-able body)
     (labels ((make-ref (obj var)
                (umatch var
                  ( (var key) `(,var (,key  ,obj)))
                  ( (var) `(,var (,(symb struct-name "-" var) ,obj)))
                  (_  `(,var (,(symb struct-name "-" var) ,obj))))))
       (with-gensyms (%ref-able)
         `(let ((,%ref-able ,ref-able))
            (let ,(mapcar (cut make-ref %ref-able <>)
                          (if (consp var-list) var-list (list var-list)))
              ,@body)))))

   ;; match系に比べ 速度重視だと思う
   ;;class
  (defmacro let-c (var-list obj &rest body)
    (let-ref-org 'slot-ref var-list obj body))
  ;; hash-table
  (defmacro let-h (var-list obj &rest body)
    (let-ref-org 'hash-table-get  var-list obj body))
  ;; assoc
  (defmacro let-a (var-list obj &rest body)
    (let-ref-org 'assoc-ref  var-list obj body))
  ;; struct
  (defmacro let-s (struct-name var-list obj &rest body)
    (let-struct-org struct-name var-list obj body))

  ;; destructuring-bind版
  ;; it.base.arnesiにもある。
  (defmacro dolist*  (vars-lis-ret &rest body)
    (let ((%x (gensym)))
      (umatch vars-lis-ret
        ((vars lis . ret)
         `(dolist (,%x ,lis ,@ret)
            (letl ,vars ,%x
              ,@body)))
        (_ (error "malformed dolist*")))))
  
  (defmacro fold* (vars-lis-xs ret-val-proc &rest forms)
    (let ((%lp (gensym)) (%lis (gensym)) (%xs (gensym)))
      (letl (%vars lis . xs) vars-lis-xs
        (if (consp xs) (setf %xs (car xs)))
        (letl (%ret val . proc) ret-val-proc
          `(letp ,%lp ((,%ret ,val)  (,%lis ,lis))
             (umatch ,%lis
               ((,%vars . ,%xs)
                (,%lp ,@(if (>= (length forms) 2) `((progn ,@forms)) forms) ,%xs))
               (nil ,(if (null proc) %ret `(,(car proc) ,%ret)))
               (t  (error "fold*: mulformed list"))))
          ))))

  ;;(defun accum (end? term succ kons knil &rest args)
  ;;  (let (tmp)
  ;;    (while (not (any end? (setq tmp (mapcar term args))))
  ;;      (setq knil (apply kons knil tmp)
  ;;            args (mapcar succ args)))
  ;;    knil))

  ;;(defmacro fold*2 (var-knil+vars knil+list-of-list &rest body)
  ;;  (letl (var-knil . vars) var-knil+vars
  ;;    (letl (knil . list-of-list) knil+list-of-list
  ;;      `(accum 'null  'car 'cdr (lambda (,var-knil ,@vars) ,@body) ,knil ,@list-of-list))))

  (defmacro map* (var+list &rest body)
    `(fold* ,var+list (knil '() nreverse) (cons ,@body knil)))
  
  
;;  (define-macro* (doht  key+val+ht+ret . forms) ()
;;    (match key+val+ht+ret
;;      ((key val ht . ret)
;;       `(begin (hash-table-for-each ,ht (lambda (,key ,val) ,@forms))
;;               ,@ret))
;;      (_ (error "malformed doht !"))))
;;  
;;  (define-macro* (dovector  ix+val+vec+ret . forms) (%vec)
;;    (umatch ix+val+vec+ret
;;      ((ix val vec . ret)
;;       `(let1 ,%vec ,vec (dotimes (,ix (vector-length ,%vec) ,@ret)
;;                           (let ((,val (vector-ref ,%vec ,ix)))
;;                             ,@forms))))
;;      (_ (error "malformed dovector !"))))

  (defun rxscanner (reg)
    ;;interpolも文字列を作っている。
    (declare (string reg))
    (cl-ppcre:create-scanner reg :case-insensitive-mode nil :multi-line-mode nil))
  
  (defun rxbase (reg str)
    (declare (string str reg))
    (let ((scanner (cl-ppcre:create-scanner reg :case-insensitive-mode nil :multi-line-mode nil)))
      (cl-ppcre:scan scanner str)))

  ;; booleanを返す方が型が決まっていいかな
  (defun rxmatch-lite (reg str)
    (declare (string reg str))
    (if (rxbase reg str) t nil))
  
  (defun rxmatchs-lite (scn str)
    (declare (type string str))
    (if (cl-ppcre:scan scn str) t nil))

  ;; scanner version
  (defun rxmatchs (scan str)
    (declare (string str))
    (multiple-value-bind (start end sts ens) (cl-ppcre:scan scan str)
      (and start (lambda (i) (if (= i 0) (subseq str start end)(subseq str (aref sts (- i 1)) (aref ens (- i 1))))))))

  ;; string-> string -> fixnum -> string | nil
  (defun rxmatchi (reg str i)
    (declare (string str reg) (fixnum i))
    (multiple-value-bind (start end sts ens) (rxbase reg str)
      (and start (if (= i 0) (subseq str start end) (subseq str (aref sts (- i 1)) (aref ens (- i 1)))))))
  
  (defun rxmatchsi (scan str i)
    (declare (string str) (fixnum i))
    (multiple-value-bind (start end sts ens) (cl-ppcre::scan scan str)
      (and start (if (= i 0) (subseq str start end) (subseq str (aref sts (- i 1)) (aref ens (- i 1)))))))

  ;;(defmacro rxmatch-values (reg str ixs)
  ;;  (with-gensyms  (%str %st %en %sts %ens)
  ;;    `(let ((,%str ,str))
  ;;       (multiple-value-bind (,%st ,%en ,%sts ,%ens) (rxbase ,reg ,%str)
  ;;         ;; %st,%en, :: nil | fixnum?
  ;;         (and ,%st (values ,@(mapcar (cutr if (= <> 0) `(subseq ,%str ,%st ,%en) `(subseq ,%str (aref ,%sts ,(- <0> 1)) (aref ,%ens ,(- <0> 1)))) ixs)))))))
  ;;
  ;;(defmacro rxmatchs-values (scan str ixs)
  ;;  (with-gensyms (%str %st %en %sts %ens)
  ;;    `(let ((,%str ,str))
  ;;       (multiple-value-bind (,%st ,%en ,%sts ,%ens) (cl-ppcre:scan ,scan ,%str)
  ;;         (and ,%st (values ,@(mapcar (cutr if (= <> 0) `(subseq ,%str ,%st ,%en) `(subseq ,%str (aref ,%sts ,(- <0> 1)) (aref ,%ens ,(- <0> 1)))) ixs)))))))

  (defun nub-ht (lis &key (test 'eq))
    (declare (type list lis))
    (hash-table-keys (nub-ht1 lis (make-hash-table :test test))))

  (defun nub-ht1 (lis ht)
    (declare (type list lis))
    (dolist (x lis ht)
      (if (not (gethash x ht nil))
          (setf (gethash x ht) 1))))


  ;; 返り値の型は以後のversionで保証しません。
  (defun nub-ht-cons (lis &key (test 'eq))
    (declare (type list lis))
    (nub-ht-cons1 lis (make-hash-table :test test)))
  
  (defun nub-ht-cons1 (lis ht)
    (declare (type list lis))
    (dolist (x lis ht)
      ;; (declare (type symbol x))
      (if (not (gethash (car x) ht nil))
          (setf (gethash (car x) ht) (cdr x)))))

  ;; 返り値の型は以後のversionで保証しません。
  (defun totalize (seq &key (rehash-threshold 0.8) (test 'equal) (size 100) (rehash-size 2.0))
    (declare (type sequence seq))
    (totalize! (make-hash-table :rehash-threshold rehash-threshold :test test :size size :rehash-size rehash-size) seq))
  
  (defun totalize! (ht seq)
    (declare (type sequence seq))
    (dolist (el seq ht)
;;  sifが使えそうな予感
      (aif (gethash el ht nil)
          (setf (gethash el ht) (+ 1 it))
        (setf (gethash el ht) 1))))
  
  ;; sortには key option があるので余り必要なかった.
  (defmacro make-compare (base-function term)
    (if (and (symbolp base-function) (symbolp term ))
        (with-gensyms (%a %b)
          `(lambda (,%a ,%b) (,base-function (,term ,%a) (,term ,%b))))
        (with-gensyms (%a %b %base %term)
          `(let ((,%base  ,base-function)
                 (,%term ,term))
             (lambda (,%a ,%b) (funcall ,%base (funcall ,%term ,%a) (funcall ,%term ,%b)))))))

  (defun take%-by-hash-table (ht odds sum)
    (let* ((sum% (round (* (or sum (apply #'+ (hash-table-values ht))) odds))))
      (take-while (cutr >= (decf sum% (cdr <>)) 0)
                  (sort (hash-table->alist ht)
                        (make-compare > cdr)))))

  ;; 以後のversionで返り値の型は保証されません。
  ;; 法則名忘れた。
  (defun take% (seq &optional (odds 0.8))
    (let* ((sum (length seq)))
      (mapcar 'car  (take%-by-hash-table (totalize seq :test 'equal :size sum :rehash-size 2.0) odds sum))))

  (defun take-topn (seq n)
    (take n (take% seq 1.0)))

  (defun take-topn-assoc (seq n)
    (let* ((sum (length seq)))
      (take n (take%-by-hash-table (totalize seq :test 'equal :size sum :rehash-size 2.0) 1.0 sum))))


  ;; (program->string-list "/bin/ls")
  ;; pipeが使えたらcommand->string-listにする。
  (defun program->string-list (comm &rest args)
    (split-sequence:SPLIT-SEQUENCE
     #\Newline
     (with-output-to-string (out)
       (sb-ext::run-program  comm args :output out))))

  ;; p*p likeな変数名なのでやめたい。
  (defun number-format (x)
    (format nil "~:D" x))
  ;; ==> "1,000,000"

  (defun map-tree (proc tree)
    (cond ((consp tree)
           (cons (map-tree proc (car tree))
                 (map-tree proc (cdr tree))))
          ;; nilはどうしようか
          (t (funcall proc tree))
          ))

  (defun fold-tree (proc knil tree)
    (cond ((consp tree)
           (fold-tree proc (fold-tree proc knil (car tree)) (cdr tree)))
          ;; nilはどうしようか
          (t (funcall proc knil tree))
          ))
  
  (defun map-tree2 (proc tree tree2)
    (cond ((and (consp tree) (consp tree2))
           (cons (map-tree2 proc (car tree) (car tree2))
                 (map-tree2 proc (cdr tree) (cdr tree2))))
          ;; nilはどうしようか
          (t (funcall proc tree tree2))
          ))

  (defun fold-tree2 (proc knil tree tree2)
    (cond ((and (consp tree) (consp tree2))
           (fold-tree2 proc (fold-tree2 proc knil (car tree) (car tree2)) (cdr tree) (cdr tree2)))
          ;; nilはどうしようか
          (t (funcall proc knil tree tree2))
          ))
  
  (defun map-list-tree (proc tree)
    (cond ((listp tree)
           (cons (map-list-tree proc (car tree))
                 (mapcar (cut map-list-tree proc <>) (cdr tree))))
          (t (funcall proc tree))))

  (defun sort-alist (alist &key (test #'<))
    (sort alist (^ (a b) test) :key 'car))

  (defun random-string (n)
    (let* ((len (random n))
           (alpha "abcdefghijklmnopqrstuvwxyz")
           (strs (mapcar (^(_) (declare (ignorable _)) (aref alpha (random (length alpha)))) (iota len))))
      (concatenate 'string strs)))

  ;;  (define (file-null? file)
  ;;    (= (file-size file) 0))

  ;;関数名どっちがいい pfold とfold-plist
  (defun fold-plist (term knil plist)
    (loop for (a b) on plist by #'cddr
       do (setq knil (funcall term knil a b)))
    knil)
  
  )


;; ` gaucheこれが抜けているのでescapeが完全ではない。その内gaucheに報告する。
;;(defun shell-escape-string (str)
;;    (cond ((string-null? str) "''")
;;          ((string-index str #(\s\\\"\'*?$<>!\[\](){}`))
;;           (string-append "'" (regexp-replace-all #/'/ str "'\"'\"'") "'"))
;;        (t str)))
