;; -*- lexical-binding: t -*-

(defalias 'use-hook #'reed-hooks-use-hook)
(defalias 'use-drop #'reed-hooks-use-drop)
(defalias 'use-hook-with-cleanup #'reed-hooks-use-hook-with-cleanup)
(defalias 'use-effect #'reed-hooks-use-effect)
(defalias 'use-before-render #'reed-hooks-use-before-render)
(defalias 'use-after-render #'reed-hooks-use-after-render)
(defalias 'use-context-provider #'reed-hooks-use-context-provider)
(defalias 'use-context #'reed-hooks-use-context)
(defalias 'use-root-context #'reed-hooks-use-root-context)

(defun use-callback (callback)
  (let ((hook (reed-hooks-use-hook
               (lambda ()
                 (let ((cb nil))
                   (cons
                    (lambda (new-cb)
                      (setq cb new-cb))
                    (lambda (&rest args)
                      (apply cb args))))))))
    (funcall (car hook) callback)
    (cdr hook)))

(defun use-ref (init)
  (reed-hooks-use-hook
   (lambda ()
     (let ((current (funcall init)))
       (lambda (&rest args)
         (if args
             (setq current (car args))
           current))))))

(defun use-signal (init)
  (let ((sigal-handle (reed-hooks-use-signal init)))
    (lambda (&rest args)
      (if args
          (reed-hooks-signal-set sigal-handle (car args))
        (reed-hooks-signal-get sigal-handle)))))


(defun use-memo (init)
  (let ((memo-handle (reed-hooks-use-memo init)))
    (lambda () (reed-hooks-memo-get memo-handle))))

(provide 'reed-hooks)
