;;; Package archives
(use-package package
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


;;; No littering in `user-emacs-directory'
(use-package no-littering
  :ensure t
  :demand t
  :config
  (no-littering-theme-backups))


;;; Config stuffs benefit from `no-littering'
(use-package emacs
  :demand t
  :after no-littering
  :bind
  ("C-c r" . recentf-open-files)

  :custom
  ;; Always regenerate custom files
  (custom-file (make-temp-file "emacs-custom-"))

  ;; Record recent files
  (recentf-max-menu-items 100)
  (recentf-max-saved-items 200)
  (recentf-auto-cleanup 'never)
  (recentf-mode t)
  ;; Exclude `temporary-file-directory' and `no-littering' directories
  (recentf-exclude
   `(,(temporary-file-directory)
     ,(recentf-expand-file-name no-littering-var-directory)
     ,(recentf-expand-file-name no-littering-etc-directory)))

  ;; Config backup
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 100)
  (kept-old-versions 100)
  (version-control t)

  ;; Config auto-save
  (auto-save-interval 30)

  ;; Put lock file to `no-littering' directory
  (lock-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "lock/") t)))

  ;; Save point positions
  (save-place-limit 1024)
  (save-place-mode t)

  :config
  ;; Prompt when there are unsaved customizing options
  (add-hook 'kill-emacs-query-functions
            'custom-prompt-customize-unsaved-options))

(use-package undo-tree
  :ensure t
  :demand t
  :after no-littering
  :custom
  ;; `undo-tree-history-directory-alist' is set by no-littering
  (global-undo-tree-mode t))


;;; Load detailed configurations
(org-babel-load-file (locate-user-emacs-file "README.org"))
