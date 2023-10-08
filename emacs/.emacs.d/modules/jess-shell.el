;;; Shell and terminal


;;; EAT -- Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  ;; Eat terminal emulation in Eshell
  (eat-eshell-mode t)
  ;; Running Eshell visual commands with Eat
  (eat-eshell-visual-command-mode t)
  :hook
  ;; Reset `mode-line-buffer-identification' to default
  (eat-mode . (lambda ()
                (setq-local mode-line-buffer-identification
                            (default-value 'mode-line-buffer-identification)))))


;;; Config shell/terminal modes
(use-package emacs
  :after (eat corfu)
  :config
  (defun jess/shell-other-window ()
    "Shell in other window."
    (interactive)
    (let ((buffer (shell)))
      (switch-to-buffer (other-buffer buffer))
      (switch-to-buffer-other-window buffer)))

  (defun jess/eshell-other-window ()
    "Eshell in other window."
    (interactive)
    (let ((buffer (eshell)))
      (switch-to-buffer (other-buffer buffer))
      (switch-to-buffer-other-window buffer)))

  (defun jess/eat-other-window ()
    "Eat in other window."
    (interactive)
    (let ((buffer (eat)))
      (switch-to-buffer (other-buffer buffer))
      (switch-to-buffer-other-window buffer)))
  :bind
  ("C-c s" . shell)
  ("C-c e" . eshell)
  ("C-c t" . eat)

  ;; Bindings to open shells in other window without shadowing
  ;; existing keybindings
  ("C-x 4 s" . jess/shell-other-window)
  ("C-x 4 e" . jess/eshell-other-window)
  ("C-x 4 t" . jess/eat-other-window)

  ;; Enable `S-TAB' for backward complete.  NOTE: this does not work
  ;; unless eat resolve the issue.
  (:map eat-semi-char-mode-map
        ("S-<tab>" . eat-self-input))

  :hook
  ;; Disable `corfu-auto' in (e)shell-mode, since `RET' is both used
  ;; by `corfu-insert' and `comint-send-input'.  Disable
  ;; `global-hl-line-mode', `column-number-mode' and
  ;; `line-number-mode' in `eat-', `shell-' and `eshell-' modes.
  (shell-mode  . (lambda () (setq-local corfu-auto          nil
                                        global-hl-line-mode nil
                                        column-number-mode  nil
                                        line-number-mode    nil)))
  (eshell-mode . (lambda () (setq-local corfu-auto          nil
                                        global-hl-line-mode nil
                                        column-number-mode  nil
                                        line-number-mode    nil)))
  (eat-mode    . (lambda () (setq-local global-hl-line-mode nil
                                        column-number-mode  nil
                                        line-number-mode    nil)))

  ;; This is for an issue that the ace-window indicator shows no space
  ;; (it should have one space) to the right end of the mode line in
  ;; *eat* buffer.  The additional space complements the offset.
  (eat-mode . (lambda ()
                (when ace-window-display-mode
                  (setq-local mode-line-format
                              (append (default-value 'mode-line-format)
                                      '(mode-line-front-space)))))))


(provide 'jess-shell)
