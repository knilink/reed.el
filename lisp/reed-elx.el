;; -*- lexical-binding: t -*-

(defun element-tag-p (symbol)
  "Return non-nil if SYMBOL's name starts with a lowercase letter, indicating an HTML tag.
This follows React's convention where components start with uppercase letters."
  (let ((name (symbol-name symbol)))
    (and (not (string-empty-p name))
         (<= ?a (aref name 0) ?z))))

(defun attr-symbol-p (symbol)
  (and (symbolp symbol)
       (string-prefix-p ":" (symbol-name symbol))))

(defun static-attr-value-p (value)
  (stringp value))

(defvar counter-stack '())

(defun elx-next-dynamic-node-id ()
  "Get the next dynamic ID and increment the counter."
  (let ((counter (caar counter-stack)))
    (setq counter-stack `((,(1+ counter) . ,(cdar counter-stack)) . ,(cdr counter-stack)))
    counter))

(defun elx-next-dynamic-attr-id ()
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

(defun elx-template-note-element-p (node)
  "Determine if SYMBOL represents a component (non-HTML element)."
  (let ((tag (car node)))
    (and (symbolp tag)
         (not (eq tag '{}))
         (not (element-tag-p tag)))))

(defun elx-template-note-dynamic-p (node)
  (let ((tag (car node)))
    (or
         (eq tag '{})
         (not (element-tag-p tag)))))


(defun elx-create-template-node (node)
  (if (stringp node)
      `(template-note:text :text ,node)
      (let ((tag (car node)))
        (cond
         ((eq tag '{}) `(template-note:dynamic :id ,(elx-next-dynamic-node-id)))
         ((and (symbolp tag) (element-tag-p tag))
          (let ((tag-name (symbol-name tag)))
            `(template-note:element
              :tag ,tag-name
              :attrs ,(vconcat (elx-create-element-attr-template tag-name (cadr node)))
              :children ,(vconcat (elx-create-template-nodes (cddr node))))))
         (t `(template-note:dynamic :id ,(elx-next-dynamic-node-id)))))))

(defun elx-create-template-nodes (nodes)
  (and
   nodes
   (cons
    (elx-create-template-node (car nodes))
    (elx-create-template-nodes (cdr nodes)))))


(defun elx-create-element-attr-template (element-tag attrs)
  (and
   attrs
   (let ((k (caar attrs))
         (v (macroexpand (cdar attrs))))
     (append
      (cond
       ((eq k 'key)
        '())
       ((static-attr-value-p v)
        `((template-attribute:static
           :element-tag ,element-tag
           :name ,(symbol-name k)
           :value ,v)))
       (t
        `((template-attribute:dynamic :id ,(elx-next-dynamic-attr-id)))))
      (elx-create-element-attr-template element-tag (cdr attrs))))))


(defun elx-collect-dynamic-nodes (nodes node-path tail)
  (if (not nodes) tail
    (let ((node (car nodes)))
      (if (or (not node) (not (listp node))) ; TODO maybe allow symbol
          (elx-collect-dynamic-nodes (cdr nodes) (cons (1+ (car node-path)) (cdr node-path)) tail)
        (let ((tag (car node))
              (new-tail (elx-collect-dynamic-nodes
                         (cdr nodes)
                         (cons (1+ (car node-path)) (cdr node-path))
                         tail)))
          (cond
           ((eq tag '{}) `(((list 'dynamic-node:element . ,(cdr node)) . ,node-path) . ,new-tail))
           ((and (symbolp tag) (element-tag-p tag))
            (elx-collect-dynamic-nodes
             (cddr node)
             (cons 0 node-path)
             new-tail))
           (t `(((list 'dynamic-node:component
                       :type #',tag
                       :props (list
                               ,@(mapcar (lambda (pair) `(cons ',(car pair) ,(cdr pair))) (cadr node))
                               (cons 'children ,(build-vnodes (cddr node)))))
                 . ,node-path)
                . ,new-tail))))))))

(defun dynamic-attrs-from-pairs (tag node-path attrs tail)
  (if attrs
      (let ((k (caar attrs))
            (v (macroexpand (cdar attrs)))
            (new-tail (dynamic-attrs-from-pairs tag node-path (cdr attrs) tail)))
        (if (or
             (static-attr-value-p v) ; excluding static attr (string)
             (eq k 'key) ; excluding key prop
             )
            new-tail
          `(((list :tag ,tag :name ,(symbol-name k) :value ,v) . ,node-path) . ,new-tail)))
    tail))

(defun elx-collect-dynamic-attrs (nodes node-path tail)
  (if (not nodes) tail
    (let ((node (car nodes)))
      (if (or (not node) (not (listp node)))
          (elx-collect-dynamic-attrs (cdr nodes) (cons (1+ (car node-path)) (cdr node-path)) tail)
        (let ((tag (car node))
              (new-tail (elx-collect-dynamic-attrs
                         (cdr nodes)
                         (cons (1+ (car node-path)) (cdr node-path))
                         tail)))
          (cond
           ((and (symbolp tag) (element-tag-p tag) (not (eq tag '{})))
            (dynamic-attrs-from-pairs
             (symbol-name tag)
             node-path
             (cadr node)
             (elx-collect-dynamic-attrs (cddr node) (cons 0 node-path) new-tail)))
           (t new-tail)))))))


(defvar register-template-debug nil)

(defun register-template (template)
  (if register-template-debug
      `(list ,@template)
    (reed-register-template
     (plist-get template :roots)
     (plist-get template :node-paths)
     (plist-get template :attr-paths))))



(defun build-vnodes (nodes)
  (and nodes
       (with-id-counter
        (lambda ()
          (let ((dyn-nodes (elx-collect-dynamic-nodes nodes '(0) '()))
                (dyn-attrs (elx-collect-dynamic-attrs nodes '(0) '()))
                (path-mapper (lambda (item) (vconcat (reverse (cdr item))))))
            (list
             'list
             ''vnode
             (and (consp (car nodes))
                  (not (eq (caar nodes) '{}))
                  (alist-get 'key (cadar nodes))) ; only look for the key prop in the first children, that seems to be how diosux rsx! works
             (register-template
              (list
               :roots (vconcat (elx-create-template-nodes nodes))
               :node-paths (vconcat (mapcar path-mapper dyn-nodes))
               :attr-paths (vconcat (mapcar path-mapper dyn-attrs))))
             `(vector ,@(mapcar #'car dyn-nodes))
             `(vector ,@(mapcar #'car dyn-attrs))))))))

(defun normalize-syntax-node-attrs (node-attrs)
  (cond
   ((not node-attrs) '(()))
   ((attr-symbol-p (car node-attrs))
    (let ((res (normalize-syntax-node-attrs (cddr node-attrs))))
      (cons
       (cons
        (cons (intern (substring (symbol-name (car node-attrs)) 1))
              (cadr node-attrs))
        (car res))
       (cdr res))
      ))
   (t (cons '() (normalize-syntax node-attrs)))))

(defun normalize-syntax (nodes)
  (and
   nodes
   (let ((node (car nodes))
         (res (normalize-syntax (cdr nodes))))
     (cons (if (or (not (listp node)) (eq (car node) '{}))
               node
             (cons (car node)
                   (normalize-syntax-node-attrs (cdr node))))
           res))))

(defun build-vnodes-2 (nodes)
  (with-id-counter
   (elx-create-template-nodes nodes)))


(defmacro elx! (&rest body)
  "Process ELX syntax into static template, dynamic nodes, and dynamic attributes."
  (build-vnodes (normalize-syntax body)))


(defun create-error-element (component-name err-string)
  (elx!
   (error (p ({} (format "[%s] %s" component-name err-string))))))


(defun generate-props-bindings  (props-list prop-name)
  (and
   props-list
   (if (attr-symbol-p (car props-list))
       `((,(cadr props-list) (alist-get ',(intern (substring (symbol-name (car props-list)) 1)) ,prop-name))
         . ,(generate-props-bindings (cddr props-list) prop-name))
     `((,(car props-list) (alist-get ',(car props-list) ,prop-name))
       . ,(generate-props-bindings (cdr props-list) prop-name)))))

(defmacro fc! (component-name props-list &rest body)
  `(defun ,component-name (&optional props) ; optional, for root component workaround
     (let ((error-element-ref (use-ref (lambda ()))))
       (or (funcall error-element-ref)
           (condition-case err
               (let ,(generate-props-bindings props-list 'props)
                 (progn ,@body))
             (error
              (let ((error-element
                     (create-error-element (symbol-name #',component-name) (error-message-string err))))
                (funcall error-element-ref error-element))))))))

(defmacro static-style! (expr)
  (let ((result (eval expr)))
    (format "%s" result)))




(provide 'reed-elx)
;;; reed-esx.el ends here
