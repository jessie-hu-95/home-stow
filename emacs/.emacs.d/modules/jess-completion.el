;;; Completion configurations


;;; MCT 1.0.0 (from GitHub)
(unless package--initialized
  (package-initialize))
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(use-package mct
  :vc
  (:fetcher github :repo protesilaos/mct)
  :after minibuffer
  :config
  (defun jess/mct-frame-height-fourth ()
    "Return round number of 1/4 of `frame-height'.
Can be used in `mct-completion-window-size'."
    (floor (frame-height) 4))
  :custom
  (mct-mode t)
  (mct-live-completion 'visible)
  (mct-completion-window-size '(jess/mct-frame-height-fourth . 1)))


;;; Minibuffer completion configs
(use-package minibuffer
  :custom
  ;; Save minibuffer history
  (savehist-mode t)

  ;; Allow minibuffer commands while in the minibuffer
  (enable-recursive-minibuffers t)

  ;; Config completion styles
  (completion-category-overrides
   '((file (styles basic partial-completion)))))


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
  (marginalia-mode t)
  :hook
  (completion-list-mode . (lambda () (let ((inhibit-message t))
                                       (visual-line-mode -1)
                                       (toggle-truncate-lines t)))))


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
  (global-corfu-mode t)

  :config
  (defun jess/enable-corfu-in-minibuffer ()
    "Enable corfu in minibuffer for several commands."
    (when (memq current-minibuffer-command
                '(shell-command
                  async-shell-command
                  eval-expression))
      (setq-local corfu-auto nil)
      (corfu-mode)))

  :hook
  (minibuffer-setup . jess/enable-corfu-in-minibuffer))

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
  ;; The following line is commented out as it would pop candidates
  ;; when writing comments
  ;;
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-tex))


(provide 'jess-completion)
