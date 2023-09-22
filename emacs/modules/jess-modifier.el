;;; Modifier definitions

(use-package emacs
  :when (eq system-type 'darwin)
  :custom
  (mac-pass-command-to-system nil)
  (mac-pass-control-to-system nil)
  (mac-command-modifier nil)
  (mac-option-modifier 'hyper)
  (mac-right-option-modifier 'meta))


(provide 'jess-modifier)
