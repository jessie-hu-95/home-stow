;;; Basic configurations


;;; Read `PATH' and `exec-path' from the login shell
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (exec-path-from-shell-initialize))


;;; Minimize appearance
(use-package emacs
  :custom
  ;; Start up with scratch buffer
  (inhibit-startup-screen t)
  (inhibit-startup-echo-area-message t)

  ;; Disable scroll bar
  (scroll-bar-mode nil)

  ;; Disable tool bar
  (tool-bar-mode nil)

  ;; Not to show buttons on the tab bar
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)

  ;; Enable highlighting of the current line
  (global-hl-line-mode t)

  ;; Enable visual line mode for wrapping long lines
  (global-visual-line-mode t)

  ;; Preserve cursor position when scrolling
  (scroll-preserve-screen-position 'always)

  ;; Specify the threshold for scrolling the window by lines instead of pixels
  (scroll-conservatively 101)

  ;; Maximize frame when startup
  (default-frame-alist '((fullscreen . maximized))))

;; Disable menu bar for none-darwin system.  On macOS, menu bar is
;; displayed on the global menu bar, and disabling it would result the
;; fullscreen button not to work correctly.
(use-package menu-bar
  :when (not (eq system-type 'darwin))
  :custom
  (menu-bar-mode t))


;;; Dired
(use-package dired
  :hook
  ;; Hide details when enter dired
  (dired-mode . dired-hide-details-mode)
  :config
  ;; Enable `dired-find-alternate-file' command
  (put 'dired-find-alternate-file 'disabled nil))


;;; Miscellaneous configs
(use-package emacs
  :custom
  ;; Put deleted files to trach
  (delete-by-moving-to-trash t)

  ;; Only type y/n for yes/no
  (use-short-answers t)

  ;; Auto revert any buffer associated with a file when the file
  ;; changes on disk
  (auto-revert-interval 0.5)
  (global-auto-revert-mode t)

  ;; Disable indentation to insert TAB characters
  (indent-tabs-mode nil)

  ;; Control buffer position
  (display-buffer-alist
   '(;; Disable `*Async Shell Command*' buffer pop up
     ("\\*Async Shell Command\\*"
      display-buffer-no-window)
     ;; Force `*Completions*' buffer pop up at the bottom
     ("\\*Completions\\*"
      (display-buffer-reuse-window display-buffer-at-bottom))))

  ;; Record the changes in the window configuration
  (winner-mode t)

  ;; Provide focus moving commands
  (windmove-mode t)

  ;; Automatically insert paired symbols
  (electric-pair-mode t)

  ;; Scrolling commands will not exit isearch
  (isearch-allow-scroll t)

  :hook
  ;; Delete trailing spaces automatically
  (before-save . delete-trailing-whitespace)

  ;; Recenter screen after jumping
  (occur-mode-find-occurrence . recenter)
  (xref-after-jump            . recenter)
  (imenu-after-jump           . recenter)

  :bind
  ;; Killing commands
  ("H-z" . zap-up-to-char)       ;; analogous to `M-z'
  ("H-k" . kill-whole-line)      ;; analogous to `C-k' and `M-k'
  ("H-q" . kill-current-buffer)  ;; analogous to `q' in many built-in modes

  ;; Window controlling commands
  ("H-o" . other-window)            ;; analogous to `C-x o'
  ("H-0" . kill-buffer-and-window)  ;; analogous to `C-x 0'
  ("H-n" . scroll-up-line)          ;; analogous to `C-n', move cursor down
  ("H-p" . scroll-down-line)        ;; analogous to `C-p', move cursor up

  ;; Mimic behaviors when scrolling mouse horizontally
  ("H-," . switch-to-prev-buffer)  ;; unshifted version of `<'
  ("H-." . switch-to-next-buffer)  ;; unshifted version of `>'

  ;; Move focus around windows
  (:map windmove-mode-map
	("H-l" . windmove-left)
	("H-r" . windmove-right)
	("H-u" . windmove-up)
	("H-d" . windmove-down)))


(provide 'jess-basic)
