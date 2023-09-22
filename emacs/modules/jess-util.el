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
  :bind
  ("H-a" . ace-select-window)
  :custom
  (aw-dispatch-always t)
  (aw-display-mode-overlay nil)
  (aw-background nil)
  ;; Utilize keys at the home row of the Workman layout
  (aw-keys '(?n ?e ?h ?s ?i ?a))
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
  (ace-window-display-mode))


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


(provide 'jess-util)
