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
  ;; Use `C-g' to kill the childframe
  (eldoc-box-clear-with-C-g t)
  :hook
  (prog-mode . eldoc-box-hover-at-point-mode))


;;; LaTeX Editing
(use-package tex
  :ensure auctex)


;;; Markdown Editing
(use-package markdown-mode
  :ensure t)


(provide 'jess-doc)
