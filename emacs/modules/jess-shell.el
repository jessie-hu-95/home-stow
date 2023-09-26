;;; Shell and terminal


;;; EAT -- Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  ;; Eat terminal emulation in Eshell
  (eat-eshell-mode t)
  ;; Running Eshell visual commands with Eat
  (eat-eshell-visual-command-mode t))


;;; Config shell/terminal modes
(use-package emacs
  :after (eat corfu)
  :bind
  ("C-c s" . shell)
  ("C-c e" . eshell)
  ("C-c t" . eat)

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
