;; Change the location of the native compilation cache to
;; `no-littering-var-directory'
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

(setq gc-cons-threshold most-positive-fixnum)
