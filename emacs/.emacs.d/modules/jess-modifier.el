;;; Modifier definitions


(use-package emacs
  :when (eq system-type 'darwin)
  :custom
  (mac-pass-command-to-system nil)
  (mac-pass-control-to-system nil)
  (mac-option-modifier 'meta)
  (mac-command-modifier 'hyper))


(provide 'jess-modifier)
