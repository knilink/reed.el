;; -*- lexical-binding: t -*-

(defun resolve-path (relative-path)
  (expand-file-name relative-path (file-name-directory load-file-name)))
(module-load (resolve-path "target/debug/libreed.so"))
(load (resolve-path "./esx.el"))
(add-hook 'kill-emacs-hook #'reed-clear-rendering-contexts)

; (require-theme)

(reed-init-tracing)

(fc! App ()
  (let ((foo "foo quer\n")
        (flag (reed-hooks-use-signal (lambda () nil))))
    (reed-hooks-use-after-render
     (lambda ()
       (run-with-timer 1 nil (lambda () (reed-hooks-signal-set flag t)))))
    (message "(reed-hooks-signal-get flag) %s" (reed-hooks-signal-get flag))
    (esx!
     (div ()
          (p (:width "30.0")
             (span () "asdf\n1234567890123456789012345678901\n")
             (span () ({} foo))
             (span () ({} (concat "flag is: " (if (reed-hooks-signal-get flag)) "true" "false"))))
          (p ()
             (span () "asdf\n\n")
             (span () "quer"))))))

(defun App2 ()
  (let ((counter (reed-hooks-use-signal (lambda () 0))))
    (reed-hooks-use-after-render
     (lambda ()
       (run-with-timer 1 nil
                       (lambda ()
                         (reed-hooks-signal-set counter (+ 200 (reed-hooks-signal-get counter)))
                         ))))
    (esx!
     (div ()
          (p (:width "30.0")
             (span () "\ncounter-1: ")
             (span () ({} (number-to-string (reed-hooks-signal-get counter))))
             (span () " end"))
          (p ()
             (span () "counter-2: ")
             (span () ({} (number-to-string (reed-hooks-signal-get counter)))))))))


(reed-register-app "test" #'App)

(message "First output")
(message "%s" (reed-render-immediate "test"))

(with-timeout (3 (message "Timeout!"))
  (let ((flag t))
    (run-with-timer 2 nil (lambda () (setq flag nil) (message "Success!")))
    (while flag (sit-for 0.1))))

(message "Second output")
(message "%s" (reed-render-immediate "test"))
