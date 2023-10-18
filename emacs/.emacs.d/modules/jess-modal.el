;;; Modal editing

;;; Use `view-mode' as the normal mode for modal editing
(use-package view
  :after (mct project)
  :config
  (defun jess/auto-view-mode () (interactive)
         (when (and buffer-file-name
                    (null view-mode))
           (view-mode)))

  (defun jess/keyboard-quit () (interactive)
         (if (and buffer-file-name
                  (null view-mode)
                  (null (use-region-p)))
             (view-mode)
           (keyboard-quit)))

  (run-with-idle-timer 10 'repeat 'jess/auto-view-mode)

  ;; Redefine the function to let `jess/keyboard-quit' work
  (defun project--switch-project-command ()
    (let* ((commands-menu
            (mapcar
             (lambda (row)
               (if (characterp (car row))
                   ;; Deprecated format.
                   ;; XXX: Add a warning about it?
                   (reverse row)
                 row))
             project-switch-commands))
           (commands-map
            (let ((temp-map (make-sparse-keymap)))
              (set-keymap-parent temp-map project-prefix-map)
              (dolist (row commands-menu temp-map)
                (when-let ((cmd (nth 0 row))
                           (keychar (nth 2 row)))
                  (define-key temp-map (vector keychar) cmd)))))
           command)
      (while (not command)
        (let* ((overriding-local-map commands-map)
               (choice (read-key-sequence (project--keymap-prompt))))
          (when (setq command (lookup-key commands-map choice))
            (unless (or project-switch-use-entire-map
                        (assq command commands-menu))
              ;; TODO: Add some hint to the prompt, like "key not
              ;; recognized" or something.
              (setq command nil)))
          (let ((global-command (lookup-key (current-global-map) choice)))
            (when (memq global-command
                        '(jess/keyboard-quit keyboard-escape-quit))
              (call-interactively global-command)))))
      command))

  :hook
  (prog-mode        . jess/auto-view-mode)
  (text-mode        . jess/auto-view-mode)
  (fundamental-mode . jess/auto-view-mode)

  :bind
  ("C-g" . jess/keyboard-quit)
  ;; Prevent remapping in MCT
  (:map mct-minibuffer-completion-list-map
        ("C-g" . mct-keyboard-quit-dwim))
  (:map view-mode-map
        ("u" . (lambda () (interactive) (View-scroll-half-page-backward 5)))
        ("d" . (lambda () (interactive) (View-scroll-half-page-forward 5)))))


(provide 'jess-modal)
