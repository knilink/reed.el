;; -*- lexical-binding: t -*-

(defvar-local reed--last-post-command-position 0
  "Stores point position after last command.")

(defun reed-get-app-window-size ()
  (cons (window-width) nil))

(defvar reed--last-app-window-size (reed-get-app-window-size))

(defun reed-handle-click ()
  (interactive)
  (reed-handle-cursor-event (buffer-name) 'click reed--last-post-command-position '())
  (reed-handle-render (buffer-name)))

(defun reed--handle-post-command ()
  (let ((should-render nil)
        (app-name (buffer-name)))
    (when (not (equal (point) reed--last-post-command-position))
      (setq reed--last-post-command-position (point))
      (setq should-render t)
      (reed-handle-cursor-event app-name 'move reed--last-post-command-position '()))
    (let ((app-window-size (reed-get-app-window-size)))
      (when (and app-window-size (not (equal reed--last-app-window-size app-window-size)))
        (setq reed--last-app-window-size app-window-size)
        (setq should-render t)
        (reed-set-size app-name reed--last-app-window-size)))
    (when should-render
      (reed-handle-render app-name))))

(define-derived-mode reed-app-mode fundamental-mode "Reed"
  (buffer-disable-undo)
  (setq-local truncate-lines t)
  (add-hook 'post-command-hook #'reed--handle-post-command nil t)
  ;(define-key reed-app-mode-map (kbd "[RET]") 'reed-handle-click)
  (local-set-key (kbd "RET") #'reed-handle-click))



(defun reed-render-to-buffer (app-name root-component)
  (with-current-buffer (get-buffer-create app-name)
    (buffer-disable-undo)
    (reed-register-app app-name root-component)
    (reed-set-size app-name (reed-get-app-window-size))
    (switch-to-buffer app-name)
    (reed-app-mode)
    (reed-handle-render app-name)))

(defun reed-handle-render (app-name)
  (with-current-buffer (get-buffer-create app-name)
    (let ((res (reed-render-immediate app-name)))
      (when res
        (let ((content (car res))
              (faces (cdr res))
              (old-point (point))
              (old-window-start (window-start)))
          (with-silent-modifications
            (erase-buffer)
            (insert content)
            (mapc (lambda (face) (apply #'add-face-text-property face)) faces)
            (goto-char old-point)
            (set-window-start (selected-window) old-window-start t)))))))

(provide 'reed-render-helper)
