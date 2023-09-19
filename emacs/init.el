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
  (dired-mode . dired-hide-details-mode)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

(use-package view
  :ensure nil
  :config
  (defun jess/auto-view-mode () (interactive)
	 (when buffer-file-name (view-mode)))
  :hook
  (prog-mode . jess/auto-view-mode)
  (text-mode . jess/auto-view-mode)
  :bind
  (("H-v" . (lambda () (interactive)
	      (unless view-mode (view-mode))))
   (:map view-mode-map
	 ("n" . next-line)
	 ("p" . previous-line)
	 ("N" . View-search-last-regexp-forward)
	 ("P" . View-search-last-regexp-backward))))

(use-package emacs
  :ensure nil
  :when
  (display-graphic-p)
  :custom
  (menu-bar-mode t)
  (default-frame-alist '((fullscreen . maximized))))

(use-package emacs
  :when
  (and (display-graphic-p)
       (find-font (font-spec :name "Lucida Grande Mono DK"))
       (find-font (font-spec :name "Lucida Sans OT")))
  :custom
  (line-spacing 0.15)
  :custom-face
  (default        ((t (:family "Lucida Grande Mono DK" :height 130))))
  (fixed-pitch    ((t (:family "Lucida Grande Mono DK"))))
  (variable-pitch ((t (:family "Lucida Sans OT" :height 1.15)))))

(use-package emacs
  :when
  (display-graphic-p)
  :init
  (defconst jess/one-golden-ratio (+ (/ (- (sqrt 5) 1) 2) 1))
  ;; Set the height of level-0 headings to `one-golden-ratio' times to
  ;; the default height.  Heading of each level decreases
  ;; expotentially until the 5th.
  (defconst jess/1gr-6th-rt (expt jess/one-golden-ratio (/ 1.0 6.0)))
  :custom
  (modus-themes-mode-line '(accented))
  (modus-themes-hl-line '(accented intense))
  (modus-themes-fringes nil)
  (modus-themes-region '(no-extend bg-only accented))
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-deuteranopia t)
  ;; The `faint' property fades color saturation saturations.  An MIT
  ;; course suggests to avoid saturated colors in UI color design
  ;; (https://web.mit.edu/6.813/www/sp16/classes/16-color/#design-guidelines).
  (modus-themes-syntax '(faint alt-syntax green-strings yellow-comments))
  (modus-themes-diffs 'desaturated)
  (modus-themes-completions '((selection . (intense accented))
			      (popup . (intense accented))))
  (modus-themes-headings
   `((5 . (variable-pitch ,(expt jess/1gr-6th-rt 1) semibold))
     (4 . (variable-pitch ,(expt jess/1gr-6th-rt 2) semibold))
     (3 . (variable-pitch ,(expt jess/1gr-6th-rt 3) bold))
     (2 . (variable-pitch ,(expt jess/1gr-6th-rt 4) bold))
     (1 . (variable-pitch ,(expt jess/1gr-6th-rt 5) heavy))
     (0 . (variable-pitch ,(expt jess/1gr-6th-rt 6) heavy))
     (t . (variable-pitch 1.0))))
  :config
  (load-theme 'modus-operandi))

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

(use-package minibuffer
  :ensure nil
  :after vertico
  :custom
  (enable-recursive-minibuffers t)
  (savehist-mode t)
  (completions-max-height (+ 3 10))
  (completion-show-help nil)
  (completions-header-format nil)
  (completions-format 'one-column)
  (completion-auto-help 'visible)
  (completion-auto-select 'second-tab)
  (completion-category-overrides
   '((file (styles basic partial-completion))))
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

(use-package vertico
  :after minibuffer
  :custom
  (completions-sort 'vertico-sort-history-length-alpha))

(use-package marginalia
  :custom
  (marginalia-mode t))

(use-package orderless
  :after minibuffer
  :custom
  (completion-styles '(orderless basic)))

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

(use-package simple
  :ensure nil
  :custom
  (indent-tabs-mode nil)
  :bind
  ("H-k" . kill-whole-line)
  ("H-q" . kill-current-buffer)
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package files
  :ensure nil
  :bind
  ("H-g" . find-file))  ;; goto file

(use-package misc
  :ensure nil
  :bind
  ("H-z" . zap-up-to-char))

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*Async Shell Command\\*" display-buffer-no-window)
     ("\\*Completions\\*"
      (display-buffer-reuse-window display-buffer-at-bottom))))
  :bind
  (("H-o" . other-window)
   ("H-0" . kill-buffer-and-window)
   ("H-n" . scroll-up-line)
   ("H-p" . scroll-down-line)
   ("H-," . switch-to-prev-buffer)
   ("H-." . switch-to-next-buffer)))

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
  ("H-SPC" . forward-whitespace)
  ("H-f"   . forward-symbol)
  ("H-b"   . (lambda (arg) (interactive "^p")
	       (forward-symbol (- arg)))))

(use-package winner
  :ensure nil
  :custom
  (winner-mode t))

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
  (corfu-auto-prefix 2)
  (corfu-quit-at-boundary nil)
  (corfu-preview-current nil)
  (corfu-preselect 'first)
  (corfu-on-exact-match 'insert)
  (corfu-quit-no-match t)
  (global-corfu-mode t)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :hook
  (shell-mode . (lambda () (setq-local corfu-auto nil)))
  (eshell-mode . (lambda () (setq-local corfu-auto nil))))

(use-package corfu-popupinfo
  :ensure nil
  :after corfu
  :hook
  (corfu-mode . corfu-popupinfo-mode)
  :bind
  (:map corfu-popupinfo-map
	("M-n" . corfu-popupinfo-scroll-up)
	("M-p" . corfu-popupinfo-scroll-down)))

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-tex))

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
