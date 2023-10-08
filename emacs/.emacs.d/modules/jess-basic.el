;;; Basic configurations


;;; Read `PATH' and `exec-path' from the login shell
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config (exec-path-from-shell-initialize))


;;; Minimize appearance
(use-package emacs
  :config
  (defun jess/flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil #'invert-face 'mode-line))

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

  ;; Hide the tab bar when it has only one tab, and show it again once
  ;; more tabs are created
  (tab-bar-show 1)

  ;; Enable highlighting of the current line
  (global-hl-line-mode t)

  ;; Enable visual line mode for wrapping long lines
  (global-visual-line-mode t)

  ;; Not to display "percentage offset" of window through buffer in
  ;; the mode line
  (mode-line-percent-position nil)

  ;; Enable `column-number-mode' and `line-number-mode' so that
  ;; line/column numbers are combined in the mode line.
  (column-number-mode t)
  (line-number-mode t)

  ;; Set the visible bell to flash the mode line
  (visible-bell nil)
  (ring-bell-function 'jess/flash-mode-line)

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
  :custom
  ;; Tell dired to guess a default target directory
  (dired-dwim-target 'dired-dwim-target-recent)
  ;; Omit hidden files in `dired-omit-mode'
  (dired-omit-files "^\\..*$")
  :hook
  (dired-mode . (lambda () (let ((inhibit-message t))
                             (dired-hide-details-mode)
                             (require 'denote)
                             (denote-dired-mode)
                             (visual-line-mode -1)
                             (toggle-truncate-lines t))))
  :config
  ;; Enable `dired-find-alternate-file' command
  (put 'dired-find-alternate-file 'disabled nil))

;; for multi-stage copy/pasting of files
(use-package dired-ranger
  :ensure t
  :after dired
  :bind
  (:map dired-mode-map
        ;; The following three commands do not do stuffs as thier
        ;; names literally.  `dired-ranger-copy' just saves files to a
        ;; ring, and one should use `dired-ranger-paste' to copy
        ;; contents in the ring or `dired-ranger-move' to move
        ;; contents from the ring.

        ;; analogous to the keybinding `w' for
        ;; `dired-copy-filename-as-kill'
        ("C-c d w" . dired-ranger-copy)
        ;; analogous to the keybinding for `C' for `dired-do-copy'
        ("C-c d c" . dired-ranger-paste)
        ;; analogous to the keybinding for `R' for `dired-do-rename'
        ("C-c d r" . dired-ranger-move)
        ("C-c d b" . dired-ranger-bookmark)
        ("C-c d v" . dired-ranger-bookmark-visit)))


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
   '(;; For `*Completions*' buffer, its behavior is hardcoded in
     ;; `minibuffer-completion-help'.  It is almost impossible to
     ;; customize it.

     ;; Disable `*Async Shell Command*' buffer pop up
     ("\\*Async Shell Command\\*"
      display-buffer-no-window)))

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
  ("H-w" . kill-buffer-and-window)
  ("H-o" . other-window)
  ("H-0" . delete-window)
  ("H-1" . delete-other-windows)
  ("H-2" . split-window-below)
  ("H-3" . split-window-right)

  ;; Quick scrolling
  ("H-u" . (lambda () (interactive) (scroll-down-line 5)))
  ("H-d" . (lambda () (interactive) (scroll-up-line 5)))

  ;; Mimic behaviors for scrolling mouse horizontally
  ("H-," . switch-to-prev-buffer)  ;; unshifted version of `<'
  ("H-." . switch-to-next-buffer)  ;; unshifted version of `>'

  ;; Shortcuts
  ("H-f" . find-file)
  ("H-b" . switch-to-buffer)

  :config
  ;; Prefix shortcuts
  (global-set-key (kbd "H-4") (lookup-key global-map (kbd "C-x 4")))
  (global-set-key (kbd "H-5") (lookup-key global-map (kbd "C-x 5")))
  (global-set-key (kbd "H-t") (lookup-key global-map (kbd "C-x t")))
  (global-set-key (kbd "H-p") (lookup-key global-map (kbd "C-x p")))
  (global-set-key (kbd "H-r") (lookup-key global-map (kbd "C-x r")))
  (global-set-key (kbd "H-v") (lookup-key global-map (kbd "C-x v"))))


(provide 'jess-basic)
