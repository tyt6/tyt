;; -*- mode:lisp; encoding:utf-8; package: :scheme.test -*-
;;! /usr/bin/sbcl --noinform --load ~/share/lib/lisp/scheme/test.lisp

(require 'fare-utils)
(require 'fare-matcher)
(require 'cl-ppcre)
(require 'parse-number)
(require 'split-sequence)
(require 'scheme)
(require 'tyt)
(common-lisp:defpackage :tyt.test2
  (:use :common-lisp :tyt.base :scheme )
  (:export ))

;; --------------------------------
(common-lisp:in-package :tyt.test2)

(defmacro is (ans test)
  `(progn
     (princ (prin1-to-string ',test))
          (let ((tyt-is-a ,ans)
                (tyt-is-b
                 (block is-b
                   (handler-bind
                       ((error (lambda (e) (declare (ignorable e))(return-from is-b 'is-b-test-exception)) ))
                     ,test))))
            (if (equal tyt-is-a tyt-is-b)
                (progn
                  (format t "ok~%")
                  t)
                ;; format
                (progn
                  (mapcar #'princ
                          (list
                           "predict " (prin1-to-string tyt-is-a)
                           ", but got" (prin1-to-string tyt-is-b)
                           ))
                  (format t "~%")
                  nil)))))

(is 'it (intern+ 'it))
(is 'scheme::it (intern+ 'it :scheme))
(is t (and (rxmatch "LIST" (describe+ 'car)) t))
(is 123 (car* '() 123))
(is 1 (car* '(1) 123) )
(is 123 (cdr* '() 123))
(is 1 (cdr* '(nil . 1) 123))
(is 123 (cadr* '() 123))
(is 2 (cadr* '(1 2) 123))
(is 123 (cddr* '() 123))
(is '(3) (cddr* '(1 2 3) 123))
(is 123 (caar* '() 123))
(is 1 (caar* '((1) 1) 123))
(is 123 (cdar* '() 123))
(is '(2) (cdar* '((1 2)) 123))
(is 123 (caddr* '() 123))
(is 3 (caddr* '(1 2 3 4) 123))
(is 123 (cadddr* '() 123))
(is 4 (cadddr* '(1 2 3 4 ) 123))

(load-macro)

(defmacro* x (a b) (%a %b)
  `(let ((,%a ,a) (,%b ,b))
     (list ,%a ,%a ,%b ,%b)))


(is '(13 13 14 14)
    (let ((a 12) (b 13))
      (x (incf a) (incf b ))))

(let ((x 1.0)
      (y 2.0))
  (is 'is-b-test-exception (the-all fixnum + x y)))

(let ((x 1)
      (y 2))
  (is 'is-b-test-exception (the-all fixnum / x y)))

(let ((x 1)
      (y 2))
  (is 3 (the-all fixnum + x y)))

(defclass <x> () (x ))
(defvar test-x (make-instance '<x>))
(setf (slot-ref test-x 'x) 1)
(is 1 (let-c (x) test-x
             x))
(is 1 (let-c x test-x
             x))

(defvar test-ht (make-hash-table :test #'equal))
(setf (gethash "x" test-ht) 13)

(is '(13 20)
    (let-h ((x "x" 12) (y "y" 20)) test-ht
           (list x y)))

(let ((al '((a . 1) (b . 2))))
  (is '(1 2)
      (let-a (a b) al
             (list a b))))

(defstruct test-y a b c)
(defvar test-y1 (make-test-y :a 1 :b 2 :c 3))
(is '(1 2 3)
    (let-s test-y (a b c) test-y1
           (list a b c)))

(is "12-34-"
    (with-output-to-string (*standard-output*)
        (dolist* ( (x y) '((1 2) (3 4)))
          (format t "~d~d-" x y))))

(is '((2 6) (6 12))
    (map* ( (x y) '((1 2) (3 4)))
          (list (* 2 x) (* 3 y))))

(is t (rxmatch-lite "^test$" "test"))

(is t (rxmatchs-lite (rxscanner "^test$") "test"))

;; 
(is "123456" (funcall (rxmatchs (rxscanner "^test([0-9]+)$") "test123456") 1))

(is "01928737"
    (rxmatchi "^([0-9]+)$" "01928737" 1))

(is "01928737"
    (rxmatchsi  (rxscanner"^([0-9]+)$") "01928737" 1))

(is '((1 . 2) (2 . 1) (3 . 1))
    (sort-alist (hash-table->alist (totalize '(1 1 2 3 )))))

(is '(1 2 3 4 5 6)(sort (nub-ht '(1 2 3 1 2 3 4 4 5 6 2 2 1 3 )) #'<))

(is '((a . b) (b . d) (c . e))  (sort-alist (hash-table->alist (nub-ht-cons '((a . b) (a . c) (b . d) (c . e))))))

(defvar lis '())
(setq lis '())
(push '(1 . 2) lis)
(push '(122 . 4) lis)
(push '(1234 . 3) lis)

(is '((1234 . 3)(122 . 4)(1 . 2)) lis)
(is '((1 . 2)(1234 . 3) (122 . 4))  (sort lis (make-compare < cdr)))
(is '((1234 . 3) (122 . 4)) lis) ;; 破壊される

(setq lis '())
(push '(1 . 2) lis)
(push '(122 . 4) lis)
(push '(1234 . 3) lis)

(is '((1 . 2)(1234 . 3) (122 . 4))  (sort lis (make-compare (lambda (a b) (< a b)) (lambda (x) (cdr x)))))


(is '(3 1 4 2 5 65 26 7646 7 6) (take% '(1 1 11 1 2  3 4 756 287 367 92 73 6 2 7 7646  26 1 3 4  5 1 2 3 4 5 2 3 4 65 3)))
(is '(3 1)  (take-topn '(1 1 11 1 2  3 4 756 287 367 92 73 6 2 7 7646  26 1 3 4  5 1 2 3 4 5 2 3 4 65 3) 2))
(is '((3 . 5) (1 . 5))  (take-topn-assoc '(1 1 11 1 2  3 4 756 287 367 92 73 6 2 7 7646  26 1 3 4  5 1 2 3 4 5 2 3 4 65 3) 2))

;; echo って binか？
(is '("1" "")  (program->string-list "/bin/echo" "1"))


(is "123,456,789" (number-format 123456789))

(is '((((2 . 4) . 6) . 10) . 12) (map-tree (scheme::cut * 2 <>) '((((1 . 2) . 3) . 5) . 6)))
(is  34 (fold-tree (^ (knil x) (+ (* 2 x) knil)) 0 '((((1 . 2) . 3) . 5) . 6)))

(is '((((3 . 16) . 6) . 5) . 84)
    (map-tree2 (cut * <> <>) '((((1 . 2) . 3) . 5) . 6) '((((3 . 8) . 2) . 1) . 14)))

(is 114 (fold-tree2 (^(knil x y) (+ (* x y)knil)) 0  '((((1 . 2) . 3) . 5) . 6) '((((3 . 8) . 2) . 1) . 14)))

(is '(((4 6 ) 4 6 8) 4 6 8)
    (map-list-tree (cut * 2 <>) '(((2 3 ) 2 3 4) 2 3 4)))

(is t (< (length (random-string 12) )12))

(is '((3 . 4)(1 . 2))  (fold-plist (lambda (kn a b) (acons a b kn)) '() '(1 2 3 4)))
