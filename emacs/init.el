(use-package use-package
  :ensure nil
  :custom
  (use-package-always-demand t)
  (use-package-always-ensure t))

(use-package package
  :ensure nil
  :demand t
  :custom
  (package-archives
   '(("gnu"          . "https://mirrors.ustc.edu.cn/elpa/gnu/")
     ("nongnu"       . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
     ("melpa-stable" . "https://mirrors.ustc.edu.cn/elpa/stable-melpa/")
     ("melpa"        . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
  (package-archive-priorities
   '(("gnu"          . 4)
     ("nongnu"       . 3)
     ("melpa-stable" . 2)
     ("melpa"        . 1)))
  :config
  (package-initialize))

(use-package no-littering
  :config
  (no-littering-theme-backups))

(use-package emacs
  :ensure nil
  :after no-littering
  :init
  (add-hook 'kill-emacs-query-functions 'custom-prompt-customize-unsaved-options)
  :custom
  (custom-file (make-temp-file "emacs-custom"))
  :config
  (load custom-file 'noerror))

(use-package recentf
  :ensure nil
  :after no-littering
  :bind
  ("C-c r" . recentf-open-files)
  :custom
  (recentf-max-menu-items 64)
  (recentf-max-saved-items 256)
  (recentf-auto-cleanup 'never)
  (recentf-mode t)
  (recentf-exclude `(,(recentf-expand-file-name no-littering-var-directory)
		     ,(recentf-expand-file-name no-littering-etc-directory))))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package emacs
  :ensure nil
  :custom
  ;; Start up with scratch buffer
  (inhibit-startup-screen t)
  (inhibit-startup-echo-area-message t)
  ;; Disable menu bar
  (menu-bar-mode nil)
  ;; Disable scroll bar
  (scroll-bar-mode nil)
  ;; Disable tool bar
  (tool-bar-mode nil)
  ;; Enable highlighting of the current line
  (global-hl-line-mode t)
  ;; Enable visual line mode for wrapping long lines
  (global-visual-line-mode t)
  ;; Preserve cursor position when scrolling
  (scroll-preserve-screen-position 'always)
  ;; Specify the threshold for scrolling the window by lines instead of pixels
  (scroll-conservatively 101))

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode))

(use-package view
  :ensure nil
  :hook
  (prog-mode . view-mode)
  :bind
  (("H-SPC" . (lambda () (interactive)
		(unless view-mode (view-mode))))
   (:map view-mode-map
	 ("n" . next-line)
	 ("p" . previous-line)
	 ("N" . View-search-last-regexp-forward)
	 ("P" . View-search-last-regexp-backward))))

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*Async Shell Command\\*" display-buffer-no-window)
     ("\\*Completions\\*"
      (display-buffer-reuse-window display-buffer-at-bottom))))
  :bind
  (("H-o" . other-window)
   ("H-n" . scroll-up-line)
   ("H-p" . scroll-down-line)))

(use-package emacs
  :ensure nil
  :when
  (display-graphic-p)
  :custom
  (menu-bar-mode t)
  (default-frame-alist '((fullscreen . maximized)))
  :config
  (set-face-attribute 'default nil :height 130))

(use-package emacs
  :when
  (and (display-graphic-p)
       (find-font (font-spec :name "Lucida Grande Mono DK")))
  :config
  (set-face-attribute 'default nil :font "Lucida Grande Mono DK"))

(use-package ef-themes
  :when
  (display-graphic-p)
  :custom
  (ef-themes-region '(no-extend))
  :config
  (load-theme 'ef-trio-light 'no-confirm))

(put 'dired-find-alternate-file 'disabled nil)

(use-package emacs
  :ensure nil
  :after no-littering
  :custom
  ;; Backup
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 100)
  (kept-old-versions 100)
  (version-control t)
  ;; Auto-save
  (auto-save-interval 30)
  ;; Lock
  (lock-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "lock/") t)))
  ;; Point place
  (save-place-limit 1024)
  (save-place-mode t)
  ;; Trash
  (delete-by-moving-to-trash t)
  ;; Short answer
  (use-short-answers t))

(use-package emacs
  :ensure nil
  :when (eq system-type 'darwin)
  :custom
  (mac-pass-command-to-system nil)
  (mac-pass-control-to-system nil)
  (mac-command-modifier nil)
  (mac-option-modifier 'hyper)
  (mac-right-option-modifier 'meta))

(defun jess/sort-by-alpha-length (elems)
  (sort elems (lambda (c1 c2)
                (or (string-version-lessp c1 c2)
                    (< (length c1) (length c2))))))

(defun jess/sort-by-history (elems)
  (if-let ((hist (and (not (eq minibuffer-history-variable t))
                      (symbol-value minibuffer-history-variable))))
      (minibuffer--sort-by-position hist elems)
    (jess/sort-by-alpha-length elems)))

(defun jess/completion-category ()
  "Return completion category."
  (when-let ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (completion-metadata-get
       (completion-metadata
	(buffer-substring-no-properties (minibuffer-prompt-end)
					(max (minibuffer-prompt-end) (point)))
        minibuffer-completion-table
        minibuffer-completion-predicate)
       'category))))

(defun jess/sort-multi-category (elems)
  "Sort ELEMS per completion category."
  (pcase (jess/completion-category)
    ('nil elems) ; no sorting
    ('kill-ring elems)
    ('project-file (jess/sort-by-alpha-length elems))
    (_ (jess/sort-by-history elems))))

(use-package minibuffer
  :ensure nil
  :custom
  (read-extended-command-predicate 'command-completion-default-include-p)
  (enable-recursive-minibuffers t)
  (savehist-mode t)
  (completion-show-help nil)
  (completions-header-format nil)
  (completions-max-height 20)
  (completions-format 'one-column)
  (completion-auto-help 'visible)
  (completion-auto-select 'second-tab)
  (completions-sort 'jess/sort-multi-category)
  :bind
  (:map
   minibuffer-local-map
   ("C-p" . minibuffer-previous-completion)
   ("C-n" . minibuffer-next-completion)
   :map
   completion-in-region-mode-map
   ("C-p" . minibuffer-previous-completion)
   ("C-n" . minibuffer-next-completion)
   ("RET" . minibuffer-choose-completion)))

(use-package marginalia
  :custom
  (marginalia-mode t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic partial-completion emacs22))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package autorevert
  :ensure nil
  :custom
  (auto-revert-interval 0.5)
  (global-auto-revert-mode t))

(use-package undo-tree
  :after no-littering
  :custom
  ;; `undo-tree-history-directory-alist' is set by no-littering
  (global-undo-tree-mode t))

(use-package winner
  :ensure nil
  :custom
  (winner-mode t))

(use-package windmove
  :ensure nil
  :custom
  (windmove-mode t)
  :bind
  (:map windmove-mode-map
	("H-l" . windmove-left)
	("H-r" . windmove-right)
	("H-u" . windmove-up)
	("H-d" . windmove-down)))

(use-package emacs
  :ensure nil
  :bind
  ("H-f"   . forward-symbol)
  ("H-b"   . (lambda (arg) (interactive "^p")
	       (progn (setq arg (- arg))
		      (forward-symbol arg)))))

(use-package avy
  :custom
  (avy-keys
   '(?n ?t ?e ?h ?o ?s ?i ?a ?u ?r ?p ?d ?l ?c ?y ?g))
  :bind
  (("H-s" . avy-goto-char-timer)
   :map isearch-mode-map
   ("H-s" . avy-isearch)))

(use-package expand-region
  :bind
  ("H-e" . er/expand-region)
  ("H-c" . er/contract-region))

(use-package electric
  :ensure nil
  :custom
  (electric-pair-mode t))

(use-package ace-window
  :bind
  ("H-a" . ace-select-window)
  :custom
  (aw-dispatch-always t)
  (aw-display-mode-overlay nil)
  (aw-background nil)
  (aw-keys '(?n ?e ?h ?s ?i ?a))
  :config
  (setq aw-dispatch-alist
	'((?0 aw-delete-window "Delete Window")
	  (?t aw-swap-window "Swap Windows")
	  (?m aw-move-window "Move Window")
	  (?y aw-copy-window "Copy Window")
	  (?b aw-switch-buffer-in-window "Select Buffer")
	  (?4 aw-switch-buffer-other-window "Switch Buffer Other Window")
	  (?! aw-execute-command-other-window "Execute Command Other Window")
	  (?= aw-split-window-fair "Split Fair Window")
	  (?- aw-split-window-vert "Split Vert Window")
	  (?| aw-split-window-horz "Split Horz Window")
	  (?1 delete-other-windows "Delete Other Windows")
	  (?f aw-flip-window)
	  (?? aw-show-dispatch-help)))
  (ace-window-display-mode))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match 'insert)
  (corfu-quit-no-match t)
  (global-corfu-mode t))

(use-package eat
  :custom
  (eat-eshell-mode t)
  (eat-eshell-visual-command-mode t))

(use-package emacs
  :ensure nil
  :bind
  ("C-c s" . shell)
  ("C-c e" . eshell)
  ("C-c t" . eat))

(defun jess/disable-global-hl-line-mode ()
  (setq-local global-hl-line-mode nil))

(use-package hl-line
  :ensure nil
  :hook
  (shell-mode  . jess/disable-global-hl-line-mode)
  (eshell-mode . jess/disable-global-hl-line-mode)
  (eat-mode    . jess/disable-global-hl-line-mode))

(defconst jess/lisp-directory
  (expand-file-name "lisp" user-emacs-directory))

(use-package eglot
  :ensure nil
  :custom
  (eglot-extend-to-xref t))

(load (expand-file-name "lang.el" jess/lisp-directory))
