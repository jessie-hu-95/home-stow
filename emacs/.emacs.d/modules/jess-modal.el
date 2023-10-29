;;; Modal editing


(use-package ryo-modal
  :vc
  (:fetcher github :repo Kungsgeten/ryo-modal)
  :ensure t
  :after expand-region
  :hook
  (prog-mode . ryo-modal-mode)
  :bind
  ("<f8>" . jess/enable-ryo-modal-mode)
  ("<f9>" . jess/disable-ryo-modal-mode)

  :config
  ;; Commands
  (defun jess/enable-ryo-modal-mode ()
    (interactive)
    (unless ryo-modal-mode (ryo-modal-mode)))

  (defun jess/disable-ryo-modal-mode ()
    (interactive)
    (when ryo-modal-mode (ryo-modal-mode -1)))

  (defun jess/avy-goto-char-right ()
    (interactive)
    (call-interactively 'avy-goto-char)
    (call-interactively 'forward-char))

  (defun jess/kill-region-or-whole-line ()
    (interactive)
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (kill-whole-line)))

  (defun jess/insert-or-kill-insert ()
    (interactive)
    (when (use-region-p)
      (kill-region (region-beginning) (region-end)))
    (ryo-modal-mode -1))

  (defun jess/yank-or-kill-yank ()
    (interactive)
    (if (use-region-p)
        (progn (kill-region (region-beginning) (region-end))
               (yank 2))
      (yank)))

  (defun jess/scroll-up-lines ()
    (interactive)
    (scroll-up-line 3))

  (defun jess/scroll-down-lines ()
    (interactive)
    (scroll-down-line 3))

  (defun jess/cycle-beginning-indentation-end ()
    (interactive)
    (let ((pt (point))
          (beg-cmd (key-binding (kbd "C-a")))
          (end-cmd (key-binding (kbd "C-e"))))
      (cond ((= pt (progn (call-interactively beg-cmd) (point))) (back-to-indentation))
            ((= pt (progn (back-to-indentation) (point))) (call-interactively end-cmd))
            (t (call-interactively beg-cmd)))))

  (defun jess/backward-symbol ()
    (interactive)
    (forward-symbol -1))

  (defun jess/set-ryo-modal-cursor-color ()
    (modus-themes-with-colors
      (setq ryo-modal-cursor-color magenta-refine-fg)))

  (add-hook 'modus-themes-after-load-theme-hook
             #'jess/set-ryo-modal-cursor-color)

  (jess/set-ryo-modal-cursor-color)

  (ryo-modal-keys
   ("a" jess/backward-symbol)
   ("b" "C-b")
   ("c" jess/cycle-beginning-indentation-end)
   ("d" "C-d")
   ("e" forward-symbol)
   ("f" "C-f")
   ("g" avy-goto-line)
   ("h" eldoc-box-help-at-point)
   ("i" jess/insert-or-kill-insert)
   ("j" "C-j")
   ("k" jess/kill-region-or-whole-line)
   ("l" "C-l")
   ("m" set-mark-command)
   ("n" "C-n")
   ("o" jess/avy-goto-char-right)
   ("p" "C-p")
   ("q" quit-window)
   ("r" "M-_")
   ("s" isearch-forward-thing-at-point)
   ("t" avy-goto-char)
   ("u" "C-_")
   ("v" yank-pop)
   ("w" "M-w")
   ("x" er/expand-region)
   ("y" jess/yank-or-kill-yank)
   ("z t" zap-up-to-char)
   ("z o" zap-to-char)
   ("SPC" jess/scroll-up-lines)
   ("DEL" jess/scroll-down-lines)
   ("RET" save-buffer)
   ("," "M-,")
   ("." "M-.")
   ("<" "M-<")
   (">" "M->")))


(provide 'jess-modal)
