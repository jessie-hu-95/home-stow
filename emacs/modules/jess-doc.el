;;; Configurations on documentation


;;; Eldoc
(use-package eldoc
  :custom
  ;; Truncate documentation in echo area
  (eldoc-echo-area-use-multiline-p nil)

  ;; Call all the functions in the special hook and display all of the
  ;; resulting strings together, after all of the functions were
  ;; called, and in the order of the functions in the hook.
  (eldoc-documentation-strategy 'eldoc-documentation-compose))


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


(provide 'jess-doc)
