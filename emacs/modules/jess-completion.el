;;; Completion configurations


;;; Minibuffer completion configs
(use-package minibuffer
  :custom
  ;; Save minibuffer history
  (savehist-mode t)

  ;; Adjust `*Completions*' buffer appearance and behavior
  (completions-max-height (+ 3 10))
  (completion-show-help nil)
  (completions-header-format nil)
  (completions-format 'one-column)
  (completion-auto-help 'always)
  (completion-auto-select 'second-tab)

  ;; Allow minibuffer commands while in the minibuffer
  (enable-recursive-minibuffers t)

  ;; Config completion styles
  (completion-category-overrides
   '((file (styles basic partial-completion))))

  :bind
  ;; Keybindings for `minibuffer-mode' for minibuffer completion
  (:map minibuffer-local-map
        ("C-p" . minibuffer-previous-completion)
        ("C-n" . minibuffer-next-completion))

  ;; Keybindings for `completion-in-region-mode', which is enabled by
  ;; the original `completion-at-point' command
  (:map completion-in-region-mode-map
        ("C-p" . minibuffer-previous-completion)
        ("C-n" . minibuffer-next-completion)
        ("RET" . minibuffer-choose-completion)))


;;; VERTical Interactive COmpletion
(use-package vertico
  :ensure t
  :after minibuffer
  :custom
  ;; Only use the sorting function from vertico
  (completions-sort 'vertico-sort-history-length-alpha))


;;; Completion style for matching regexps in any order
(use-package orderless
  :ensure t
  :after minibuffer
  :custom
  ;; Add orderless completion style
  (completion-styles '(orderless basic)))


;;; Annotate completion candidates
(use-package marginalia
  :ensure t
  :custom
  (marginalia-mode t))


;;; COmpletion in Region FUnction
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary nil)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match 'insert)
  (corfu-quit-no-match t)
  (corfu-auto-delay 0.1)
  (global-corfu-mode t))

;; Pop-up documentation
(use-package corfu-popupinfo
  :after corfu
  :custom
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :hook
  (corfu-mode . corfu-popupinfo-mode)
  :bind
  (:map corfu-popupinfo-map
	("M-n" . corfu-popupinfo-scroll-up)
	("M-p" . corfu-popupinfo-scroll-down)))


;;; Completion At Point Extensions
(use-package cape
  :ensure t
  :after minibuffer
  :config
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-tex))


(provide 'jess-completion)
