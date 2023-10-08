;;; Modal editing

;;; Use `view-mode' as the normal mode for modal editing
(use-package view
  :after mct
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

  :hook
  (prog-mode        . jess/auto-view-mode)
  (text-mode        . jess/auto-view-mode)
  (fundamental-mode . jess/auto-view-mode)

  :bind
  ("C-g" . jess/keyboard-quit)
  ;; Prevent remapping
  (:map mct-minibuffer-completion-list-map
        ("C-g" . mct-keyboard-quit-dwim))
  (:map view-mode-map
        ("u" . (lambda () (interactive) (View-scroll-half-page-backward 5)))
        ("d" . (lambda () (interactive) (View-scroll-half-page-forward 5)))))


(provide 'jess-modal)
