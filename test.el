;; -*- lexical-binding: t -*-
(defun resolve-path (relative-path)
  (expand-file-name relative-path (file-name-directory load-file-name)))
(module-load (resolve-path "target/debug/libreed.so"))
(load (resolve-path "./esx.el"))
(add-hook 'kill-emacs-hook #'reed-clear-rendering-contexts)

; (require-theme)

(defun App ()

  (let ((foo "foo quer\n")
        (flag (use-signal (lambda () nil))))
    (reed-hooks-use-after-render
     (lambda ()
       (run-with-timer 1 nil (lambda () (funcall flag t)))))
    (message "(funcall flag) %s" (funcall flag))
    (esx!
     (:div ()
           (:p (:width "10.0")
               (:span () "asdf\n1234567890123456789012345678901\n")
               (:span () (:{} foo))
               (:span () (:{} (if (funcall flag) "true" "false"))))
           (:p ()
               (:span () "asdf\n\n")
               (:span () "quer"))))))


(reed-register-app "test" #'App)

(message "First output")
(message "%s" (reed-render "test"))

(with-timeout (3 (message "Timeout!"))
  (let ((flag t))
    (run-with-timer 2 nil (lambda () (setq flag nil) (message "Success!")))
    (while flag (sit-for 0.1))))

(message "Second output")
(message "%s" (reed-render "test"))
