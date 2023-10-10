;;; Configurations on documentation


;;; Eldoc
(use-package eldoc
  :custom
  ;; Truncate documentation in echo area
  (eldoc-echo-area-use-multiline-p nil)

  ;; Call all the functions in the special hook and display all of the
  ;; resulting strings together, after all of the functions were
  ;; called, and in the order of the functions in the hook.
  (eldoc-documentation-strategy 'eldoc-documentation-compose)

  :config
  ;; Remove `eldoc-display-in-echo-area' from Eldoc display functions
  ;; to prevent echo area writing.
  (setq eldoc-display-functions (delq
                                 'eldoc-display-in-echo-area
                                 eldoc-display-functions)))


;;; Eldoc-box -- Floating childframe
(use-package eldoc-box
  :ensure t
  :custom
  ;; Use `C-g' to kill the childframe.  NOTE: this does not work for
  ;; `eldoc-box-help-at-point' unless the new release fixed this
  ;; issue.
  (eldoc-box-clear-with-C-g t)
  :bind
  ("H-h" . eldoc-box-help-at-point))


;;; LaTeX Editing
(use-package tex
  :ensure auctex)


;;; Markdown Editing
(use-package markdown-mode
  :ensure t)


;;; Denote -- effective note-taking and file-naming tool
(use-package denote
  :ensure t)


(provide 'jess-doc)
