;;; Modal editing

;;; Use `view-mode' as the normal mode for modal editing
(use-package view
  :config
  (defun jess/auto-view-mode () (interactive)
	 (when buffer-file-name (view-mode)))
  :hook
  (prog-mode        . jess/auto-view-mode)
  (text-mode        . jess/auto-view-mode)
  (fundamental-mode . jess/auto-view-mode)
  :bind
  (("H-SPC" . (lambda () (interactive)
	        (unless view-mode (view-mode))))))


(provide 'jess-modal)
