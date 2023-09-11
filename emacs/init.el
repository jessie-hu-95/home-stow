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
  :hook
  (kill-emacs-query-functions . custom-prompt-customize-unsaved-options)
  :custom
  (custom-file (make-temp-file "emacs-custom"))
  :config
  (load custom-file 'noerror))

(use-package recentf
  :ensure nil
  :after no-littering
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
  (initial-scratch-message nil)
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

(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(("\\*Completions\\*"
      (display-buffer-reuse-window display-buffer-at-bottom)))))

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
       (completion-metadata (buffer-substring-no-properties
                             (minibuffer-prompt-end)
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
  :custom
  ;; `undo-tree-history-directory-alist' is set by no-littering
  (global-undo-tree-mode t))

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
  ("H-u" . er/expand-region)
  ("H-d" . er/contract-region))

(use-package electric
  :ensure nil
  :custom
  (electric-pair-mode t))

(use-package ace-window
  :bind
  ("H-o" . ace-select-window)
  :custom
  (aw-dispatch-when-more-than 2)
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
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (global-corfu-mode t))
