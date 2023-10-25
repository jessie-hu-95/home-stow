;;; Modal editing


(use-package ryo-modal
  :vc
  (:fetcher github :repo Kungsgeten/ryo-modal)
  :ensure t
  :after expand-region
  :hook
  (prog-mode . ryo-modal-mode)
  :bind
  ("<f9>" . (lambda () (interactive)
               (when (null ryo-modal-mode)
                 (ryo-modal-mode))))
  :config
  ;; Commands
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
    (scroll-up-line 5))

  (defun jess/scroll-down-lines ()
    (interactive)
    (scroll-down-line 5))

  (defun jess/back-to-indentation-or-beginning ()
    (interactive)
    (if (= (point) (progn (back-to-indentation) (point)))
        (let ((command (key-binding (kbd "C-a"))))
          (when command
            (call-interactively command)))))

  (modus-themes-with-colors
    (setq ryo-modal-cursor-color cyan-faint))

  (ryo-modal-keys
   ("a" jess/back-to-indentation-or-beginning)
   ("b" "C-b")
   ("c" er/contract-region)
   ("d" "C-d")
   ("e" "C-e")
   ("f" "C-f")
   ("g" avy-goto-line)
   ("h" "M-b")
   ("i" jess/insert-or-kill-insert)
   ("j" "C-j")
   ("k" jess/kill-region-or-whole-line)
   ("l" "M-f")
   ("m" set-mark-command)
   ("n" "C-n")
   ("o" jess/avy-goto-char-right)
   ("p" "C-p")
   ("q" eldoc-box-help-at-point)
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
