;;; TRAMP


;;; Use remote path environment variable
(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


(provide 'jess-tramp)
