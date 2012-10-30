
;;; requires and setup

(when load-file-name
  (setq package-enable-at-startup nil)
  (setq package-load-list '((list-utils t)))
  (when (fboundp 'package-initialize)
    (package-initialize)))

(require 'eieio)
(require 'list-utils)
(require 'truthy)

;;; truthy

(ert-deftest truthy-01 nil
  "t"
  (should
   (truthy t)))

(ert-deftest truthy-02 nil
  "nil"
  (should-not
   (truthy nil)))

(ert-deftest truthy-03 nil
  "Number"
  (should
   (truthy 123)))

(ert-deftest truthy-04 nil
  "Number"
  (should-not
   (truthy 0))
  (should-not
   (truthy 0.0)))

(ert-deftest truthy-05 nil
  "String"
  (should
   (truthy "content"))
  (should
   (truthy " "))

(ert-deftest truthy-06 nil
  "String"
  (should-not
   (truthy "")))

(ert-deftest truthy-07 nil
  "Symbol"
  (should
   (truthy 'truthy)))

(ert-deftest truthy-08 nil
  "Symbol"
  (should-not
   (truthy (gensym))))

(ert-deftest truthy-09 nil
  "Vector"
  (should
   (truthy (vector 1 2 3))))

(ert-deftest truthy-10 nil
  "Vector"
  (should-not
   (truthy '[]))
  (should-not
   (truthy '[ [] ]))
  (should-not
   (truthy '[ [] [] ]))
  (should-not
   (truthy '[0 0 0])))

(ert-deftest truthy-11 nil
  "List"
  (should
   (truthy '(a b c)))
  (should
   (truthy (list 1 2 3))))

(ert-deftest truthy-12 nil
  "List"
  (should-not
   (truthy nil))
  (should-not
   (truthy '()))
  (should-not
   (truthy '(nil)))
  (should-not
   (truthy '(())))
  (should-not
   (truthy '((nil)))))
  (should-not
   (truthy (list 0 0 0)))
  (should-not
   (truthy '((nil nil nil)))))

(ert-deftest truthy-13 nil
  "Vector with list"
  (should
   (truthy (vector 1 2 (list 3)))))

(ert-deftest truthy-14 nil
  "Vector with list"
  (should-not
   (truthy '[ () ]))
  (should-not
   (truthy '[ [] () ])))

(ert-deftest truthy-15 nil
  "List with vector"
  (should
   (truthy (list 1 2 (vector 3)))))

(ert-deftest truthy-16 nil
  "List with vector"
  (should-not
   (truthy '([])))
  (should-not
   (truthy '( [] () ))))

(ert-deftest truthy-17 nil
  "Improper List"
  (should
   (truthy (list* 1 2 3))))

(ert-deftest truthy-18 nil
  "Improper List"
  (should-not
   (truthy (cons nil '[]))))

(ert-deftest truthy-19 nil
  "Don't modify improper lists"
  (should (equal (list* 1 2 3)
                 (let ((improper (list* 1 2 3)))
                   (truthy improper)
                   improper))))

(ert-deftest truthy-20 nil
  "Cyclic list"
  (let ((cyclic '(a b c d e f g h)))
    (nconc cyclic cyclic)
    (should
     (truthy cyclic))))

(ert-deftest truthy-21 nil
  "Defstruct"
  (should
   (truthy (make-random-state))))

(ert-deftest truthy-22 nil
  "Defstruct"
  (should-not
   (truthy (make-tconc))))

(ert-deftest truthy-23 nil
  "Bool vector"
  (should
   (truthy (make-bool-vector 10 t))))

(ert-deftest truthy-24 nil
  "Bool vector"
  (should-not
   (truthy (make-bool-vector 10 nil))))

(ert-deftest truthy-25 nil
  "Hash table"
  (let ((value (make-hash-table :size 10)))
    (puthash "one" 1 value)
    (puthash "two" 2 value)
    (should
     (truthy value))))

(ert-deftest truthy-26 nil
  "Hash table"
  (let ((value (make-hash-table :size 10)))
    (should-not
     (truthy value))))

(ert-deftest truthy-27 nil
  "Byte code"
  (should
   (truthy (make-byte-code nil t nil nil))))

(ert-deftest truthy-28 nil
  "Byte code"
  (should-not
   (truthy (make-byte-code nil nil nil nil))))

(ert-deftest truthy-29 nil
  "Marker"
  (with-temp-buffer
    (let ((value (make-marker)))
      (move-marker value 1)
      (should
       (truthy value)))))

(ert-deftest truthy-30 nil
  "Marker"
  (with-temp-buffer
    (let ((value (make-marker)))
      (should-not
       (truthy value)))))

(ert-deftest truthy-31 nil
  "Marker"
  (with-temp-buffer
    (let ((value (make-marker)))
      (move-marker value 1)
      (kill-buffer (current-buffer))
      (should-not
       (truthy value)))))

(ert-deftest truthy-32 nil
  "Overlay"
  (with-temp-buffer
    (let ((value (make-overlay 1 1)))
      (should
       (truthy value)))))

(ert-deftest truthy-33 nil
  "Overlay"
  (with-temp-buffer
    (let ((value (make-overlay 1 1)))
      (delete-overlay value)
      (should-not
       (truthy value)))))

(ert-deftest truthy-34 nil
  "Overlay"
  (with-temp-buffer
    (let ((value (make-overlay 1 1)))
      (kill-buffer (current-buffer))
      (should-not
       (truthy value)))))

(ert-deftest truthy-35 nil
  "Buffer"
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (should-not
       (truthy buffer))
      (insert " ")
      (should
       (truthy buffer)))))

(ert-deftest truthy-36 nil
  "Buffer"
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (kill-buffer (current-buffer))
      (should-not
       (truthy buffer)))))

(ert-deftest truthy-37 nil
  "Keymap"
  (let ((value (make-sparse-keymap)))
    (define-key value (kbd "a") 'ignore)
    (should
     (truthy value))))

(ert-deftest truthy-38 nil
  "Keymap"
  (let ((value (make-sparse-keymap)))
    (should-not
     (truthy value))))

(ert-deftest truthy-39 nil
  "Process"
  (let ((value (start-process "sleeper" "*sleeper*" "sleep" "1")))
    (should
     (truthy value))
    (sleep-for 2)
    (should-not
     (truthy value))))

(ert-deftest truthy-40 nil
  "Frame configuration"
  (truthy (current-frame-configuration)))

(ert-deftest truthy-41 nil
  "Char Table"
  (let ((value (make-char-table 'testing)))
    (set-char-table-range value '(?a . ?b) 3)
    (should
     (truthy value))))

(ert-deftest truthy-42 nil
  "Char Table"
  (let ((value (make-char-table 'testing)))
    (should-not
     (truthy value))))

(ert-deftest truthy-43 nil
  "Font Spec"
  (should
   (truthy (font-spec :family "Monaco"))))

(ert-deftest truthy-44 nil
  "Font Spec"
  (should-not
   (truthy (font-spec))))

(ert-deftest truthy-45 nil
  "Function"
  (should
   (truthy (lambda () 1 2 3))))

(ert-deftest truthy-46 nil
  "Function"
  (should-not
   (truthy (lambda (arg))))
  (should-not
   (truthy (lambda ()))))

(ert-deftest truthy-47 nil
  "Defclass"
  (defclass tester nil
    ((uid :initarg :uid)))
  (let ((value (tester "object_name")))
    (oset value :uid "my_id")
    (should
     (truthy value))))

(ert-deftest truthy-48 nil
  "Defclass"
  (defclass tester nil
    ((uid :initarg :uid)))
  (let ((value (tester "object_name")))
    (should-not
     (truthy value))))

(ert-deftest truthy-49 nil
  "Window"
  (should
   (truthy (selected-window))))

(ert-deftest truthy-50 nil
  "Window"
  :tags '(:interactive)
  (let ((value (split-window)))
    (should
     (truthy value))
    (delete-window value)
    (should-not
     (truthy value))))

(ert-deftest truthy-51 nil
  "Frame"
  (should
   (truthy (selected-frame))))

(ert-deftest truthy-52 nil
  "Frame"
  :tags '(:interactive)
  (let ((value (make-frame)))
    (should
     (truthy value))
    (delete-frame value)
    (should-not
     (truthy value))))

;;; truthy-s -- todo @@@

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:
;;

;;; truthy-test.el ends here
