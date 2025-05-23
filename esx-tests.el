(require 'ert)

(defun resolve-path (relative-path)
  (expand-file-name relative-path (file-name-directory load-file-name)))
(load (resolve-path "./esx.el"))

;;; Test Helper
(defmacro with-reset-counter (&rest body)
  "Execute BODY with fresh dynamic attribute counter"
  `(with-id-counter
    (lambda () ,@body)))

(defun register-template (template)
  `(list ,@template))


(ert-deftest dynamic-attrs-from-pairs-test-basic ()
  "Test basic attribute processing."
  (should
   (equal
    (dynamic-attrs-from-pairs 'div '(:path) '((k1 . 42)) nil)
    '(((list :tag div :name "k1" :value 42) . (:path))))))


(ert-deftest dynamic-attrs-from-pairs-test-string-skipping ()
  "Test skipping attributes with string values."
  (should
   (equal
    (dynamic-attrs-from-pairs 'div '(:path) '((k2 . "string-value")) nil)
    nil)))


(ert-deftest dynamic-attrs-from-pairs-test-mixed-values ()
  "Test mixed string and non-string attributes."
  (should (equal (dynamic-attrs-from-pairs
                  'div '(:path) '((a . 1) (b . "skip") (c . 3)) nil)
                 '(((list :tag div :name "a" :value 1) . (:path))
                   ((list :tag div :name "c" :value 3) . (:path))))))

(ert-deftest dynamic-attrs-from-pairs-test-node-path-propagation ()
  "Test node-path is correctly propagated."
  (should (equal (dynamic-attrs-from-pairs
                  'div '(0 1 2) '((x . 10) (y . 20)) nil)
                 '(((list :tag div :name "x" :value 10) . (0 1 2))
                   ((list :tag div :name "y" :value 20) . (0 1 2))))))

(ert-deftest dynamic-attrs-from-pairs-test-nested-recursion ()
  "Test proper recursion through multiple pairs."
  (should (equal (dynamic-attrs-from-pairs
                  'ul '() '((a . 1) (b . 2))
                   '(((list :tag ul :name "c" :value 3))
                     ((list :tag ul :name "d" :value 4))))
                 '(((list :tag ul :name "a" :value 1))
                   ((list :tag ul :name "b" :value 2))
                   ((list :tag ul :name "c" :value 3))
                   ((list :tag ul :name "d" :value 4))))))

(ert-deftest dynamic-attrs-from-pairs-test-empty-attributes ()
  "Test empty attributes return original tail."
  (should (equal (dynamic-attrs-from-pairs
                  'div '() nil '(((list :tag ul :name "c" :value 3))
                                 ((list :tag ul :name "d" :value 4))))
                 '(((list :tag ul :name "c" :value 3))
                   ((list :tag ul :name "d" :value 4))))))


(ert-deftest esx-create-element-attr-template-test-all-static ()
  (should (equal (with-reset-counter
                  (esx-create-element-attr-template
                   'div '((name . "Alice") (role . "admin"))))
                 '((template-attribute:static
                    :element-tag div :name "name" :value "Alice")
                   (template-attribute:static
                    :element-tag div :name "role" :value "admin")))))

(ert-deftest esx-create-element-attr-template-test-all-dynamic ()
  (with-reset-counter
   (should (equal (esx-create-element-attr-template
                   'span '((age . 30) (score . 100)))
                  '((template-attribute:dynamic :id 0)
                    (template-attribute:dynamic :id 1))))))

(ert-deftest esx-create-element-attr-template-test-mixed ()
  (with-reset-counter
   (should (equal (esx-create-element-attr-template
                   'input '((placeholder . "Email") (value . user-email) (disabled . nil)))
                  '((template-attribute:static
                     :element-tag input :name "placeholder" :value "Email")
                    (template-attribute:dynamic :id 0)
                    (template-attribute:dynamic :id 1))))))

(ert-deftest esx-create-element-attr-template-test-nested ()
  (with-reset-counter
   (should (equal (esx-create-element-attr-template
                   'ul '((items . (1 2 3)) (class . "list")))
                  '((template-attribute:dynamic :id 0)
                    (template-attribute:static
                     :element-tag ul :name "class" :value "list"))))))

(ert-deftest esx-create-element-attr-template-test-empty ()
  (should (not (esx-create-element-attr-template 'div nil))))
