
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
   (truthy t))
  (should
   (eq t
       (truthy t))))

(ert-deftest truthy-02 nil
  "nil"
  (should-not
   (truthy nil)))

(ert-deftest truthy-03 nil
  "Number"
  (let ((value 123))
    (should
     (truthy value))
    (should
     (= value
        (truthy value)))))

(ert-deftest truthy-04 nil
  "Number"
  (should-not
   (truthy 0))
  (should-not
   (truthy 0.0)))

(ert-deftest truthy-05 nil
  "String"
  (let ((value "content"))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))
    (setq value " ")
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-06 nil
  "String"
  (should-not
   (truthy "")))

(ert-deftest truthy-07 nil
  "Symbol"
  (let ((value 'truthy))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-08 nil
  "Symbol"
  (let ((value (gensym)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-09 nil
  "Vector"
  (let ((value (vector 1 2 3)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

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
  (let ((value '(a b c)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))
    (setq value (list 1 2 3))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

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
   (truthy '((nil))))
  (should-not
   (truthy (list 0 0 0)))
  (should-not
   (truthy '((nil nil nil)))))

(ert-deftest truthy-13 nil
  "Vector with list"
  (let ((value (vector 1 2 (list 3))))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-14 nil
  "Vector with list"
  (should-not
   (truthy '[ () ]))
  (should-not
   (truthy '[ [] () ])))

(ert-deftest truthy-15 nil
  "List with vector"
  (let ((value (list 1 2 (vector 3))))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-16 nil
  "List with vector"
  (should-not
   (truthy '([])))
  (should-not
   (truthy '( [] () ))))

(ert-deftest truthy-17 nil
  "Improper List"
  (let ((value (list* 1 2 3)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-18 nil
  "Improper List"
  (should-not
   (truthy (cons nil '[]))))

(ert-deftest truthy-19 nil
  "Don't modify improper lists"
  (let* ((value (vector 1 2 3))
         (backup (copy-tree value)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))
    (should
     (equal backup value))))

(ert-deftest truthy-20 nil
  "Cyclic list"
  (let ((value '(a b c d e f g h)))
    (nconc value value)
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-21 nil
  "Defstruct"
  (let ((value (make-random-state)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-22 nil
  "Defstruct"
  (should-not
   (truthy (make-tconc))))

(ert-deftest truthy-23 nil
  "Bool vector"
  (let ((value (make-bool-vector 10 t)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

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
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-26 nil
  "Hash table"
  (let ((value (make-hash-table :size 10)))
    (should-not
     (truthy value))))

(ert-deftest truthy-27 nil
  "Byte code"
  (let ((value (make-byte-code nil t nil nil)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

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
       (truthy value))
      (should
       (eq value
           (truthy value))))))

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
       (truthy value))
      (should
       (eq value
           (truthy value))))))

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
       (truthy buffer))
      (should
       (eq buffer
           (truthy buffer))))))

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
     (truthy value))
    (should
     (eq value
         (truthy value)))))

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
    (should
     (eq value
         (truthy value)))
    (sleep-for 2)
    (should-not
     (truthy value))))

(ert-deftest truthy-40 nil
  "Frame configuration"
  (let ((value (current-frame-configuration)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-41 nil
  "Char Table"
  (let ((value (make-char-table 'testing)))
    (set-char-table-range value '(?a . ?b) 3)
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-42 nil
  "Char Table"
  (let ((value (make-char-table 'testing)))
    (should-not
     (truthy value))))

(ert-deftest truthy-43 nil
  "Font Spec"
  (let ((value (font-spec :family "Monaco")))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-44 nil
  "Font Spec"
  (should-not
   (truthy (font-spec))))

(ert-deftest truthy-45 nil
  "Function"
  (let ((value (lambda () 1 2 3)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-46 nil
  "Function"
  (should-not
   (truthy (lambda (arg))))
  (should-not
   (truthy (lambda ()))))

(ert-deftest truthy-47 nil
  "Defclass"
  (defclass truthy-tester-47 nil
    ((uid :initarg :uid)))
  (let ((value (truthy-tester-47 "object_name")))
    (oset value :uid "my_id")
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-48 nil
  "Defclass"
  (defclass truthy-tester-48 nil
    ((uid :initarg :uid)))
  (let ((value (truthy-tester-48 "object_name")))
    (should-not
     (truthy value))))

(ert-deftest truthy-49 nil
  "Window"
  (let ((value (selected-window)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-50 nil
  "Window"
  :tags '(:interactive)
  (let ((value (split-window)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))
    (delete-window value)
    (should-not
     (truthy value))))

(ert-deftest truthy-51 nil
  "Frame"
  (let ((value (selected-frame)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-52 nil
  "Frame"
  :tags '(:interactive)
  (let ((value (make-frame)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))
    (delete-frame value)
    (should-not
     (truthy value))))

(ert-deftest truthy-53 nil
  "Ring"
  (let ((value (make-ring 10)))
    (should-not
     (truthy value))
    (ring-insert value "")
    (should-not
     (truthy value))
    (ring-insert value 1)
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-54 nil
  "Abbrev table"
  (let ((value (make-abbrev-table)))
    (should-not
     (truthy value))
    (define-abbrev value "twoo" "two")
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-55 nil
  "Macro"
  (let ((value (defmacro truthy-tester () 1)))
    (should
     (truthy (symbol-function value)))
    (should
     (eq (symbol-function value)
         (truthy (symbol-function value))))))

(ert-deftest truthy-56 nil
  "Macro"
  (let ((value (defmacro truthy-tester ())))
    (should-not
     (truthy (symbol-function value))))
  (let ((value (defmacro truthy-tester (arg))))
    (should-not
     (truthy (symbol-function value)))))

(ert-deftest truthy-57 nil
  "Cons with nil"
  (let ((value '(nil . 4)))
    (should
     (truthy value))
    (should
     (eq value
         (truthy value)))))

(ert-deftest truthy-58 nil
  "Posn"
  :tags '(:interactive)
  :expected-result (if (and (>= emacs-major-version 24) (>= emacs-minor-version 3)) :passed :failed)
  (with-temp-buffer
    (should
     (truthy (posn-at-point)))
    (should (equal (posn-at-point)
                   (truthy (posn-at-point))))))

(ert-deftest truthy-59 nil
  "Posn"
  :tags '(:interactive)
  (with-temp-buffer
    (let ((value (posn-at-point)))
      (kill-buffer (current-buffer))
      (delete-window (posn-window value))
      (should-not
       (truthy value)))))

;;; truthy-s

(ert-deftest truthy-s-01 nil
  "Vector - empty"
  (should-not
   (truthy '[]))
  (should-not
   (truthy-s '[])))

(ert-deftest truthy-s-02 nil
  "Vector of nil"
  (should-not
   (truthy '[nil]))
  (should-not
   (truthy-s '[nil])))

(ert-deftest truthy-s-03 nil
  "Vector of empty vectors"
  (should-not
   (truthy '[ [] ]))
  (should
   (truthy-s '[ [] ])))

(ert-deftest truthy-s-04 nil
  "Vector of empty vectors"
  (should-not
   (truthy '[ [] [] ]))
  (should
   (truthy-s '[ [] [] ])))

(ert-deftest truthy-s-05 nil
  "Vector of zeros"
  (should-not
   (truthy '[0 0 0]))
  (should
   (truthy-s '[0 0 0])))

(ert-deftest truthy-s-06 nil
  "List - empty"
  (should-not
   (truthy '()))
  (should-not
   (truthy-s '())))

(ert-deftest truthy-s-07 nil
  "List of nils"
  (should-not
   (truthy '(nil)))
  (should-not
   (truthy-s '(nil))))

(ert-deftest truthy-s-08 nil
  "List of empty vectors"
  (should-not
   (truthy '( [] )))
  (should
   (truthy-s '( [] )))

(ert-deftest truthy-s-09 nil
  "List of zeros"
  (should-not
   (truthy '(0 0 0))))
  (should
   (truthy-s '(0 0 0))))

(ert-deftest truthy-s-10 nil
  "Cons with nil"
  (let ((value '(nil . [])))
    (should-not
     (truthy value))
    (should
     (truthy-s value))))

(ert-deftest truthy-s-11 nil
  "Posn"
  :tags '(:interactive)
  :expected-result (if (and (>= emacs-major-version 24) (>= emacs-minor-version 3)) :passed :failed)
  (with-temp-buffer
    (should
     (truthy-s (posn-at-point)))
    (should (equal (posn-at-point)
                   (truthy-s (posn-at-point))))))

(ert-deftest truthy-s-12 nil
  "Posn"
  :tags '(:interactive)
  :expected-result (if (and (>= emacs-major-version 24) (>= emacs-minor-version 3)) :passed :failed)
  (with-temp-buffer
    (let ((value (posn-at-point)))
      (kill-buffer (current-buffer))
      (delete-window (posn-window value))
      (should-not
       (truthy value))
      (should
       (truthy-s value)))))

;;; truthy-l

(ert-deftest truthy-l-01 nil
  "t"
  (let ((value t))
    (should
     (truthy value))
    (should
     (truthy-l value))))

(ert-deftest truthy-l-02 nil
  "nil"
  (let ((value nil))
    (should-not
     (truthy value))
    (should-not
     (truthy-l value))))

(ert-deftest truthy-l-03 nil
  "Number"
  (let ((value 123))
    (should
     (truthy-l value))
    (should
     (= value
        (truthy-l value)))))

(ert-deftest truthy-l-04 nil
  "Number"
  (should-not
   (truthy 0))
  (should-not
   (truthy 0.0)))

(ert-deftest truthy-l-05 nil
  "String"
  (let ((value "content"))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))
    (setq value " ")
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-06 nil
  "Empty String"
  (should-not
   (truthy-l "")))

(ert-deftest truthy-l-07 nil
  "Symbol"
  (let ((value 'truthy))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-08 nil
  "Symbol"
  (let ((value (gensym)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-09 nil
  "Vector"
  (let ((value (vector 1 2 3)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-10 nil
  "Vector - empty"
  (should-not
   (truthy '[]))
  (should-not
   (truthy-l '[])))

(ert-deftest truthy-l-11 nil
  "Vector of nil"
  (should-not
   (truthy '[nil]))
  (should
   (truthy-l '[nil])))

(ert-deftest truthy-l-12 nil
  "Vector of empty vectors"
  (should-not
   (truthy '[ [] ]))
  (should
   (truthy-l '[ [] ])))

(ert-deftest truthy-l-13 nil
  "Vector of empty vectors"
  (should-not
   (truthy '[ [] [] ]))
  (should
   (truthy-l '[ [] [] ])))

(ert-deftest truthy-l-14 nil
  "Vector of zeros"
  (should-not
   (truthy '[0 0 0]))
  (should
   (truthy-l '[0 0 0])))

(ert-deftest truthy-l-15 nil
  "List"
  (let ((value '(a b c)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))
    (setq value (list 1 2 3))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-16 nil
  "List - empty"
  (should-not
   (truthy '()))
  (should-not
   (truthy-l '())))

(ert-deftest truthy-l-17 nil
  "List of nils"
  (should-not
   (truthy '(nil)))
  (should
   (truthy-l '(nil))))

(ert-deftest truthy-l-18 nil
  "List of empty vectors"
  (should-not
   (truthy '( [] )))
  (should
   (truthy-l '( [] )))

(ert-deftest truthy-l-19 nil
  "List of zeros"
  (should-not
   (truthy '(0 0 0))))
  (should
   (truthy-l '(0 0 0))))

(ert-deftest truthy-l-20 nil
  "List"
  (should
   (truthy-l '(())))
  (should
   (truthy-l '((nil))))
  (should
   (truthy-l '((nil nil nil)))))

(ert-deftest truthy-l-21 nil
  "Vector with list"
  (let ((value (vector 1 2 (list 3))))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-22 nil
  "Vector with list"
  (should
   (truthy-l '[ () ]))
  (should
   (truthy-l '[ [] () ])))

(ert-deftest truthy-l-23 nil
  "List with vector"
  (let ((value (list 1 2 (vector 3))))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-24 nil
  "List with vector"
  (should
   (truthy-l '([])))
  (should
   (truthy-l '( [] () ))))

(ert-deftest truthy-l-25 nil
  "Improper List"
  (let ((value (list* 1 2 3)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-26 nil
  "Improper List"
  (should
   (truthy-l (cons nil '[]))))

(ert-deftest truthy-l-27 nil
  "Don't modify improper lists"
  (let* ((value (vector 1 2 3))
         (backup (copy-tree value)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))
    (should
     (equal backup value))))

(ert-deftest truthy-l-28 nil
  "Cyclic list"
  (let ((value '(a b c d e f g h)))
    (nconc value value)
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-29 nil
  "Defstruct"
  (let ((value (make-random-state)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-30 nil
  "Defstruct"
  (should
   (truthy-l (make-tconc))))

(ert-deftest truthy-l-31 nil
  "Bool vector"
  (let ((value (make-bool-vector 10 t)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))
    (setq value (make-bool-vector 0 t))
    (should-not
     (truthy-l value))))

(ert-deftest truthy-l-32 nil
  "Bool vector"
  (should
   (truthy-l (make-bool-vector 10 nil))))

(ert-deftest truthy-l-33 nil
  "Hash table"
  (let ((value (make-hash-table :size 10)))
    (puthash "one" 1 value)
    (puthash "two" 2 value)
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-34 nil
  "Hash table"
  (let ((value (make-hash-table :size 10)))
    (should-not
     (truthy-l value))))

(ert-deftest truthy-l-35 nil
  "Byte code"
  (let ((value (make-byte-code nil t nil nil)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-36 nil
  "Byte code"
  (should-not
   (truthy-l (make-byte-code nil nil nil nil))))

(ert-deftest truthy-l-37 nil
  "Marker"
  (with-temp-buffer
    (let ((value (make-marker)))
      (move-marker value 1)
      (should
       (truthy-l value))
      (should
       (eq value
           (truthy-l value))))))

(ert-deftest truthy-l-38 nil
  "Marker"
  (with-temp-buffer
    (let ((value (make-marker)))
      (should-not
       (truthy-l value)))))

(ert-deftest truthy-l-39 nil
  "Marker"
  (with-temp-buffer
    (let ((value (make-marker)))
      (move-marker value 1)
      (kill-buffer (current-buffer))
      (should-not
       (truthy-l value)))))

(ert-deftest truthy-l-40 nil
  "Overlay"
  (with-temp-buffer
    (let ((value (make-overlay 1 1)))
      (should-not
       (truthy-l value)))))

(ert-deftest truthy-l-41 nil
  "Overlay"
  (with-temp-buffer
    (insert "text")
    (let ((value (make-overlay 1 2)))
      (should
       (truthy-l value))
      (should
       (eq value
           (truthy-l value))))))

(ert-deftest truthy-l-42 nil
  "Overlay"
  (with-temp-buffer
    (let ((value (make-overlay 1 1)))
      (delete-overlay value)
      (should-not
       (truthy-l value)))))

(ert-deftest truthy-l-43 nil
  "Overlay"
  (with-temp-buffer
    (let ((value (make-overlay 1 1)))
      (kill-buffer (current-buffer))
      (should-not
       (truthy-l value)))))

(ert-deftest truthy-l-44 nil
  "Buffer"
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (should-not
       (truthy-l buffer))
      (insert " ")
      (should
       (truthy-l buffer))
      (should
       (eq buffer
           (truthy-l buffer))))))

(ert-deftest truthy-l-45 nil
  "Buffer"
  (with-temp-buffer
    (let ((buffer (current-buffer)))
      (kill-buffer (current-buffer))
      (should-not
       (truthy-l buffer)))))

(ert-deftest truthy-l-46 nil
  "Keymap"
  (let ((value (make-sparse-keymap)))
    (define-key value (kbd "a") 'ignore)
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-47 nil
  "Keymap"
  (let ((value (make-sparse-keymap)))
    (should-not
     (truthy-l value))))

(ert-deftest truthy-l-48 nil
  "Process"
  (let ((value (start-process "sleeper" "*sleeper*" "sleep" "1")))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))
    (sleep-for 2)
    (should-not
     (truthy-l value))))

(ert-deftest truthy-l-49 nil
  "Frame configuration"
  (let ((value (current-frame-configuration)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-50 nil
  "Char Table"
  (let ((value (make-char-table 'testing)))
    (set-char-table-range value '(?a . ?b) 3)
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-51 nil
  "Char Table"
  (let ((value (make-char-table 'testing)))
    (should-not
     (truthy-l value))))

(ert-deftest truthy-l-52 nil
  "Font Spec"
  (let ((value (font-spec :family "Monaco")))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-53 nil
  "Font Spec"
  (should-not
   (truthy-l (font-spec))))

(ert-deftest truthy-l-54 nil
  "Function"
  (let ((value (lambda () 1 2 3)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-55 nil
  "Function"
  (should-not
   (truthy-l (lambda (arg))))
  (should-not
   (truthy-l (lambda ()))))

(ert-deftest truthy-l-56 nil
  "Defclass"
  (defclass truthy-tester-l56 nil
    ((uid :initarg :uid)))
  (let ((value (truthy-tester-l56 "object_name")))
    (oset value :uid "my_id")
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-57 nil
  "Defclass"
  (defclass truthy-tester-l57 nil
    ((uid :initarg :uid)))
  (let ((value (truthy-tester-l57 "object_name")))
    (should-not
     (truthy-l value))))

(ert-deftest truthy-l-58 nil
  "Window"
  (let ((value (selected-window)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-59 nil
  "Window"
  :tags '(:interactive)
  (let ((value (split-window)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))
    (delete-window value)
    (should-not
     (truthy-l value))))

(ert-deftest truthy-l-60 nil
  "Frame"
  (let ((value (selected-frame)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-61 nil
  "Frame"
  :tags '(:interactive)
  (let ((value (make-frame)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))
    (delete-frame value)
    (should-not
     (truthy-l value))))

(ert-deftest truthy-l-62 nil
  "Ring"
  (let ((value (make-ring 10)))
    (should-not
     (truthy-l value))
    (ring-insert value "")
    (should
     (truthy-l value))
    (ring-insert value 1)
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-63 nil
  "Ring"
  (let ((value (make-ring 0)))
    (should-not
     (truthy-l value))))

(ert-deftest truthy-l-64 nil
  "Abbrev table"
  (let ((value (make-abbrev-table)))
    (should-not
     (truthy-l value))
    (define-abbrev value "twoo" "two")
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-65 nil
  "Macro"
  (let ((value (defmacro truthy-tester () 1)))
    (should
     (truthy-l (symbol-function value)))
    (should
     (eq (symbol-function value)
         (truthy-l (symbol-function value))))))

(ert-deftest truthy-l-66 nil
  "Macro"
  (let ((value (defmacro truthy-tester ())))
    (should-not
     (truthy-l (symbol-function value))))
  (let ((value (defmacro truthy-tester (arg))))
    (should-not
     (truthy-l (symbol-function value)))))

(ert-deftest truthy-l-67 nil
  "Cons with nil"
  (let ((value '(nil . 4)))
    (should
     (truthy-l value))
    (should
     (eq value
         (truthy-l value)))))

(ert-deftest truthy-l-68 nil
  "Cons with nil"
  (let ((value '(nil . [])))
    (should-not
     (truthy value))
    (should
     (truthy-l value))))

(ert-deftest truthy-l-69 nil
  "Posn"
  :tags '(:interactive)
  (with-temp-buffer
    (should
     (truthy-l (posn-at-point)))
    (should (equal (posn-at-point)
                   (truthy-l (posn-at-point))))))

(ert-deftest truthy-l-70 nil
  "Posn"
  :tags '(:interactive)
  :expected-result (if (and (>= emacs-major-version 24) (>= emacs-minor-version 3)) :passed :failed)
  (with-temp-buffer
    (let ((value (posn-at-point)))
      (kill-buffer (current-buffer))
      (delete-window (posn-window value))
      (should-not
       (truthy value))
      (should
       (truthy-l value)))))

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
