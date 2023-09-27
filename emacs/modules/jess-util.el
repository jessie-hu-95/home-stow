;;; Utilities


;;; Increase selected region by semantic units
(use-package expand-region
  :ensure t
  :bind
  ("H-e" . er/expand-region)
  ("H-c" . er/contract-region))


;;; Jumping around
(use-package avy
  :ensure t
  :custom
  (avy-keys
   ;; Keys easy to reach in the Workman keyboard layout
   '(?n ?t ?e ?h ?o ?s ?i ?a ?u ?r ?p ?d ?l ?c ?y ?g))
  :bind
  ;; Analogous to `C-s' and `M-s', representing search commands
  ("H-s" . avy-goto-char-timer)
  (:map isearch-mode-map
        ("H-s" . avy-isearch)))


;;; Window management
(use-package ace-window
  :ensure t
  ;; We need to add window indicator in the mode line, but we need to
  ;; do this after other configurations on `mode-line-format'
  :defer 3
  :bind
  ("H-a" . ace-select-window)
  :custom
  (aw-dispatch-always t)
  (aw-display-mode-overlay nil)
  (aw-background nil)
  ;; Utilize keys at the home row of the Workman layout
  (aw-keys '(?n ?e ?h ?s ?i ?a))
  ;; Display indicators on the mode line
  (ace-window-display-mode t)

  :config
  ;; Bind mnemonic keys for these actions.  Note that these key should
  ;; not have intersection with `aw-keys'
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

  ;; The following code achieves aligning `ace-window' indicator at
  ;; the right end of the mode line, when `ace-window-display-mode' is
  ;; enabled.

  (defun jess/remove-ace-window-display-mode ()
    "Remove `ace-window' indicator from the mode line"
    (setq-default mode-line-format
                  (assq-delete-all 'ace-window-display-mode
                                   mode-line-format)))

  (defvar jess/mode-line-ace-window
    '(:eval (when ace-window-display-mode
              (setq aw-indicator-array
                    (window-parameter (selected-window)
                                      'ace-window-path))
              (if (arrayp aw-indicator-array)
                  (progn (setq aw-indicator
                               (upcase
                                (aref aw-indicator-array 0)))
                         (aset aw-indicator-array 0 aw-indicator)
                         aw-indicator-array)
                "")))
    "Customized indicator for `ace-window'")

  ;; Give permission for the variable
  (put 'jess/mode-line-ace-window 'risky-local-variable t)

  ;; The following code is copied from the `bindings.el' file in
  ;; Emacs 30 source code.
  (when (< emacs-major-version 30)
    (defcustom mode-line-right-align-edge 'window
      "Where function `mode-line-format-right-align' should align to.
Internally, that function uses `:align-to' in a display property,
so aligns to the left edge of the given area.  See info node
`(elisp)Pixel Specification'.

Must be set to a symbol.  Acceptable values are:
- `window': align to extreme right of window, regardless of margins
  or fringes
- `right-fringe': align to right-fringe
- `right-margin': align to right-margin"
      :type '(choice (const right-margin)
                     (const right-fringe)
                     (const window))
      :group 'mode-line
      :version "30.1")

    (defun mode--line-format-right-align ()
      "Right-align all following mode-line constructs.

When the symbol `mode-line-format-right-align' appears in
`mode-line-format', return a string of one space, with a display
property to make it appear long enough to align anything after
that symbol to the right of the rendered mode line.  Exactly how
far to the right is controlled by `mode-line-right-align-edge'.

It is important that the symbol `mode-line-format-right-align' be
included in `mode-line-format' (and not another similar construct
such as `(:eval (mode-line-format-right-align)').  This is because
the symbol `mode-line-format-right-align' is processed by
`format-mode-line' as a variable."
      (let* ((rest (cdr (memq 'mode-line-format-right-align
			      mode-line-format)))
	     (rest-str (format-mode-line `("" ,@rest)))
	     (rest-width (progn
                           (add-face-text-property
                            0 (length rest-str) 'mode-line t rest-str)
                           (string-pixel-width rest-str))))
        (propertize " " 'display
		    ;; The `right' spec doesn't work on TTY frames
		    ;; when windows are split horizontally (bug#59620)
		    (if (and (display-graphic-p)
                             (not (eq mode-line-right-align-edge 'window)))
		        `(space :align-to (- ,mode-line-right-align-edge
                                             (,rest-width)))
		      `(space :align-to (,(- (window-pixel-width)
                                             (window-scroll-bar-width)
                                             (window-right-divider-width)
                                             (* (or (cdr (window-margins)) 1)
                                                (frame-char-width))
                                             ;; Manually account for value of
                                             ;; `mode-line-right-align-edge' even
                                             ;; when display is non-graphical
                                             (pcase mode-line-right-align-edge
                                               ('right-margin
                                                (or (cdr (window-margins)) 0))
                                               ('right-fringe
                                                ;; what here?
                                                (or (cadr (window-fringes)) 0))
                                               (_ 0))
                                             rest-width)))))))

    (defvar mode-line-format-right-align '(:eval (mode--line-format-right-align))
      "Mode line construct to right align all following constructs.")

    (put 'mode-line-format-right-align 'risky-local-variable t))  ;; End of when

  ;; Align the customized indicator to the right of the mode line
  (when ace-window-display-mode
    (jess/remove-ace-window-display-mode)
    (setq-default mode-line-format
                  (append (default-value 'mode-line-format)
                          '(mode-line-format-right-align
                            jess/mode-line-ace-window
                            ;; Add the space at the end, which is same
                            ;; to the front one
                            mode-line-front-space))))

  :hook
  ;; Remove the default indicator for each toggle of the mode
  (ace-window-display-mode . jess/remove-ace-window-display-mode))


;;; Automatic project commands
(use-package project
  :config
  (defun jess/auto-find-file () (interactive)
         "Automatically select `find-file' or `project-find-file'"
         (if (project-current nil)
             (call-interactively 'project-find-file)
           (call-interactively 'find-file)))

  (defun jess/auto-switch-to-buffer () (interactive)
         "Automatically select `switch-to-buffer' or `project-switch-to-buffer'"
         (if (project-current nil)
             (call-interactively 'project-switch-to-buffer)
           (call-interactively 'switch-to-buffer)))

  :bind
  ("H-f" . jess/auto-find-file)          ;; analogous to `C-x C-f'
  ("H-b" . jess/auto-switch-to-buffer))  ;; analogous to `C-x b'


(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode)
  :bind
  ("C-c h m" . hs-minor-mode)
  (:map hs-minor-mode-map
        ("C-c h h" . hs-hide-block)       ;; alias for `C-c @ C-h'
        ("C-c h s" . hs-show-block)       ;; alias for `C-c @ C-s'
        ("C-c h l" . hs-hide-level)       ;; alias for `C-c @ C-l'
        ("C-c h c" . hs-toggle-hiding)    ;; alias for `C-c @ C-c'
        ("C-c h a" . hs-show-all)         ;; alias for `C-c @ C-a'
        ("C-c h t" . hs-hide-all)         ;; alias for `C-c @ C-t'
        ("C-c h d" . hs-hide-block)       ;; alias for `C-c @ C-d'
        ("C-c h e" . hs-toggle-hiding)))  ;; alias for `C-c @ C-e'


;;; Magit -- the best git client
(use-package magit
  :ensure t
  :custom
  ;; Not to add keybindings for `magit-status', `magit-dispatch' and
  ;; `magit-file-dispatch' to the global keymap
  (magit-define-global-key-bindings nil)
  :bind
  ;; Bind mnemonic key sequences to magit commands
  ("C-c m s" . magit-status)
  ("C-c m d" . magit-dispatch)
  ("C-c m f" . magit-file-dispatch))

(use-package magit-extras
  ;; This package should be loaded if we need `magit-project-status'
  ;; bound in `project-prefix-map'.
  :demand t
  :custom
  ;; Bind `m' to `magit-project-status' in `project-prefix-map', same
  ;; as default
  (magit-bind-magit-project-status t))


(provide 'jess-util)
