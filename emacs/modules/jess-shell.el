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

  :hook
  ;; Disable `corfu-auto' in (e)shell-mode, since `RET' is both used
  ;; by `corfu-insert' and `comint-send-input'
  (shell-mode  . (lambda () (setq-local corfu-auto nil)))
  (eshell-mode . (lambda () (setq-local corfu-auto nil)))

  :custom
  ;; Disable `global-hl-line-mode' in each shell/terminal
  (shell-mode  . (lambda () (setq-local global-hl-line-mode nil)))
  (eshell-mode . (lambda () (setq-local global-hl-line-mode nil)))
  (eat-mode    . (lambda () (setq-local global-hl-line-mode nil))))


(provide 'jess-shell)
