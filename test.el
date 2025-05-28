;; -*- lexical-binding: t -*-

(defun resolve-path (relative-path)
  (expand-file-name relative-path (file-name-directory load-file-name)))
(module-load (resolve-path "target/debug/libreed.so"))
(load (resolve-path "./esx.el"))
(add-hook 'kill-emacs-hook #'reed-clear-rendering-contexts)

; (require-theme)


(reed-init-tracing)

(defun style (s)
  (prin1-to-string s t))

(defvar AUTO (reed-taffy-length 'AUTO 0.0))


(fc! App ()
     (let* ((foo "foo quer\n")
           (flag (reed-hooks-use-signal (lambda () nil)))
           (counter (reed-hooks-use-signal (lambda () 0)))
           (hovering (reed-hooks-use-signal (lambda () nil)))
           (handle-timer (use-callback (lambda (&rest args) (reed-hooks-signal-set flag t)))))
       (reed-hooks-use-effect
        (lambda ()
          (message "use effect cb")
          (run-with-timer 1 nil handle-timer)))
       (message "(reed-hooks-signal-get flag) %s" (reed-hooks-signal-get flag))
       (esx!
        (div
         :style (style
                 `((display . Flex)
                   (size
                    (width . ,(reed-taffy-length 'percent 1.0))
                    (height . ,AUTO))
                   ))
             (p
              :style (style
                      `((display . Flex)
                        (border
                         (left . ,(reed-taffy-length 'length 1))
                         (right . ,(reed-taffy-length 'length 1))
                         (top . ,(reed-taffy-length 'length 1))
                         (bottom . ,(reed-taffy-length 'length 1)))
                        (size
                         (width . ,(reed-taffy-length 'length 18.0))
                         (height . ,AUTO))))
              (span "asdf\n1234567890123456789012345678901\n")
              (span ({} foo))
              (span ({} (concat "flag is: " (if (reed-hooks-signal-get flag) "true" "false")))))
             (p
              :onhover (lambda () (reed-hooks-signal-set hovering t))
              ({} (if (reed-hooks-signal-get hovering) "hovering" "not hovering")))
             (p
              :style (style
                      `((display . Block)
                        (size
                         (width . ,AUTO)
                         (height . ,AUTO))))
              (span  "asdf\n\n")
              (span  "quer"))))))

(fc! App2 ()
     (let ((hovering (reed-hooks-use-signal (lambda () nil))))
       (esx!
        (p
         :foo "asdf"
         :onhover (lambda (&rest e)
                    (message "[onhover start] %s" e)
                    (reed-hooks-signal-set hovering t)
                    (message "[onhover end]"))
         ({} (if (reed-hooks-signal-get hovering) "hovering" "not hovering"))))))


(fc! Child2 (value children)
     (message "[children] %s" children)
     (reed-hooks-use-effect
      (lambda ()
        (let ()
          (run-with-timer
           0 nil
           (lambda ()
             (message "[Child2] begin")
             (message "[Child2] buffer string" (buffer-string))
             (message "[Child2] buffer string end")
             )))))
     (esx! (p ({} children) ({} value) )))

(fc! App3 ()
     (esx!
      (div
       (p "ffffffffffffffff\nffffffffffffffff\n")
       (p "jjjjjjjjjjjjjjjj")
       ({} (mapcar (lambda (a) (esx! (Child2 :key a :value a ({} "Child2-child")))) '("foo1" "bar2" "baz3"))))))

(fc! App4 ()
     (let ((flag-sig (use-signal (lambda () nil))))
       (reed-hooks-use-effect
        (lambda ()
          (run-with-timer
           1 nil
           (lambda ()
             (message "ttttttttttttttttim")
             (funcall flag-sig t)))))
       (message "[render] app4")
       (esx!
        (div
         ({} (if (funcall flag-sig) (esx! (p "A")) (esx! (p "B"))))))))

(message "[Child2] 3")

(reed-register-app "test" #'App4)
(reed-set-width "test" 105)

(message "First output")
(message "[result/start]\n%s\n[result/end]" (reed-render-immediate "test"))

(run-with-timer
 1 nil
 (lambda ()
   (message "[reed-handle-event] start")
   (reed-handle-event "test" 7 '(move 7))
   (message "[reed-handle-event] end")))

(with-timeout (3 (message "Timeout!"))
  (let ((flag t))
    (run-with-timer 2 nil (lambda () (setq flag nil) (message "Success!")))
    (while flag (sit-for 0.1))))

(message "Second output")
(message "%s" (reed-render-immediate "test"))
