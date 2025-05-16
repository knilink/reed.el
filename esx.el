;; -*- lexical-binding: t -*-

(defvar counter-stack '())

(defun esx-next-dynamic-node-id ()
  "Get the next dynamic ID and increment the counter."
  (let ((counter (caar counter-stack)))
    (setq counter-stack `((,(1+ counter) . ,(cdar counter-stack)) . ,(cdr counter-stack)))
    counter))

(defun esx-next-dynamic-attr-id ()
  "Get the next dynamic ID and increment the counter."
  (let ((counter (cdar counter-stack)))
    (setq counter-stack `((,(caar counter-stack) . ,(1+ counter)) . ,(cdr counter-stack)))
    counter))

(defun with-id-counter (f)
  (setq counter-stack (cons '(0 . 0) counter-stack))  ; Push new counter
  (unwind-protect
      (funcall f)  ; Execute the function
    (setq counter-stack (cdr counter-stack))))  ; Always pop afterward

;; Helper functions for processing nodes and attributes

(defvar dynamic-node 0)

(defun esx-template-note-element-p (node)
  "Determine if SYMBOL represents a component (non-HTML element)."
  (let ((tag (car node)))
    (and (symbolp tag)
         (not (eq tag ':{}))
         (not (string-prefix-p ":" (symbol-name tag))))))

(defun esx-template-note-dynamic-p (node)
  (let ((tag (car node)))
    (or
         (eq tag ':{})
         (not (string-prefix-p ":" (symbol-name tag))))))


(defun esx-create-template-node (node)
  "j"
  (if (stringp node)
      `(template-note:text :text ,node)
      (let ((tag (car node)))
        (cond
         ((eq tag ':{}) `(template-note:dynamic :id ,(esx-next-dynamic-node-id)))
         ((and (symbolp tag) (string-prefix-p ":" (symbol-name tag)))
          (let ((tag-name (substring (symbol-name tag) 1)))
            `(template-note:element
              :tag ,tag-name
              :attrs ,(vconcat (esx-create-element-attr-template tag-name (cadr node)))
              :children ,(vconcat (esx-create-template-nodes (cddr node))))))
         (t `(template-note:dynamic :id ,(esx-next-dynamic-node-id)))))))

(defun esx-create-template-nodes (nodes)
  (and
   nodes
   (cons
    (esx-create-template-node (car nodes))
    (esx-create-template-nodes (cdr nodes)))))


(defun esx-create-element-attr-template (element-tag attrs)
  (and
   attrs
   (let ((k (car attrs))
         (v (cadr attrs)))
     (cons
      (cond
       ((stringp v) `(template-attribute:static
                      :element-tag ,element-tag
                      :name ,(substring (symbol-name k) 1)
                      :value ,v))
       (t `(template-attribute:dynamic :id ,(esx-next-dynamic-attr-id))))
      (esx-create-element-attr-template element-tag (cddr attrs))))))


(defun esx-collect-dynamic-nodes (nodes node-path tail)
  (if (not nodes) tail
    (let ((node (car nodes)))
      (if (or (not node) (not (listp node)))
          (esx-collect-dynamic-nodes (cdr nodes) (cons (1+ (car node-path)) (cdr node-path)) tail)
        (let ((tag (car node))
              (new-tail (esx-collect-dynamic-nodes
                         (cdr nodes)
                         (cons (1+ (car node-path)) (cdr node-path))
                         tail)))
          (cond
           ((eq tag ':{}) `(((list 'dynamic-node:element . ,(cdr node)) . ,node-path) . ,new-tail))
           ((and (symbolp tag) (string-prefix-p ":" (symbol-name tag)))
            (esx-collect-dynamic-nodes
             (cddr node)
             (cons 0 node-path)
             new-tail))
           (t `(((list 'dynamic-node:component
                        :type #',tag :props (list ,@(cadr node) :children ,(build-vnodes (cddr node))))
                 . ,node-path)
                . ,new-tail))))))))

(defun dynamic-attrs-from-pairs (tag node-path attrs tail)
  (if attrs
      (let ((k (car attrs))
            (v (cadr attrs))
            (new-tail (dynamic-attrs-from-pairs tag node-path (cddr attrs) tail)))
        (if (stringp v)
            new-tail
          `(((list :tag ,tag :name ,(substring (symbol-name k) 1) :value ,v) . ,node-path) . ,new-tail)))
    tail))

(defun esx-collect-dynamic-attrs (nodes node-path tail)
  (if (not nodes) tail
    (let ((node (car nodes)))
      (if (or (not node) (not (listp node)))
          (esx-collect-dynamic-attrs (cdr nodes) (cons (1+ (car node-path)) (cdr node-path)) tail)
        (let ((tag (car node))
              (new-tail (esx-collect-dynamic-attrs
                         (cdr nodes)
                         (cons (1+ (car node-path)) (cdr node-path))
                         tail)))
          (cond
           ((and (symbolp tag) (string-prefix-p ":" (symbol-name tag)) (not (eq tag ':{})))
            (dynamic-attrs-from-pairs
             (substring (symbol-name tag) 1)
             node-path
             (cadr node)
             (esx-collect-dynamic-attrs (cddr node) (cons 0 node-path) new-tail)))
           (t new-tail)))))))


(defun register-template (template)
  (or
   nil
   (reed-register-template
    (plist-get template :roots)
    (plist-get template :node-paths)
    (plist-get template :attr-paths))))



(defun build-vnodes (nodes)
  (with-id-counter
   (lambda ()
     (let ((dyn-nodes (esx-collect-dynamic-nodes nodes '(0) '()))
           (dyn-attrs (esx-collect-dynamic-attrs nodes '(0) '()))
           (path-mapper (lambda (item) (vconcat (reverse (cdr item))))))
       (list
        'list
         (register-template
          (list
           :roots (vconcat (esx-create-template-nodes nodes))
           :node-paths (vconcat (mapcar path-mapper dyn-nodes))
           :attr-paths (vconcat (mapcar path-mapper dyn-attrs))))
        `(vector ,@(mapcar #'car dyn-nodes))
        `(vector ,@(mapcar #'car dyn-attrs)))))))

(defun build-vnodes-2 (nodes)
  (with-id-counter
   (esx-create-template-nodes nodes)))

(defmacro esx! (&rest body)
  "Process ESX syntax into static template, dynamic nodes, and dynamic attributes."
  (build-vnodes body))

(defun use-signal (init)
  (let ((handle (reed-hooks-use-signal init)))
    (lambda (&rest rest)
      (if rest
          (reed-hooks-signal-set handle (car rest))
        (reed-hooks-signal-get handle)))))
