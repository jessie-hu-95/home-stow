;;; LSP configurations with Eglot


;;; Python setup
(use-package pyvenv-auto
  :ensure t
  :config
  (defun jess/pyrightconfig-write (virtualenv)
    "Write `pyrightconfig.json' file automatically"
    (let* (;; file-truename and tramp-file-local-name ensure that
           ;; neither `~' nor the Tramp prefix (e.g. "/ssh:my-host:")
           ;; wind up in the final absolute directory path.
           (venv-dir (tramp-file-local-name (file-truename virtualenv)))

           ;; Given something like /path/to/venv/, this strips off the
           ;; trailing `/'.
           (venv-file-name (directory-file-name venv-dir))

           ;; Naming convention for venvPath matches the field for
           ;; pyrightconfig.json.  `file-name-directory' gets us the
           ;; parent path (one above venv).
           (venvPath (file-name-directory venv-file-name))

           ;; Grabs just the `venv' off the end of the venv-file-name.
           (venv (file-name-base venv-file-name))

           ;; Eglot demands that `pyrightconfig.json' is in the
           ;; project root folder.
           (base-dir (vc-git-root default-directory))
           (out-file (expand-file-name "pyrightconfig.json" base-dir))

           ;; Finally, get a string with the JSON payload.
           (out-contents (json-encode (list :venvPath venvPath :venv venv))))

      ;; Emacs uses buffers for everything.  This creates a temp
      ;; buffer, inserts the JSON payload, then flushes that content
      ;; to final `pyrightconfig.json' location
      (with-temp-file out-file (insert out-contents))))

  (defun jess/pyvenv-auto-venv ()
    "Automatically get the venv"
    (pyvenv-auto--locate-venvs
     default-directory
     pyvenv-auto-venv-dirnames))

  (defun jess/pyvenv-auto-setup ()
    "Activate venv and wright `pyrightconfig.json' file"
    (pyvenv-auto-run)
    (jess/pyrightconfig-write (jess/pyvenv-auto-venv)))

  :hook
  (python-mode . jess/pyvenv-auto-setup)
  (eshell-mode . jess/pyvenv-auto-setup))

(defconst jess/python-server-program '("pyright-langserver" "--stdio"))


;;; Eglot config
(use-package eglot
  :custom
  ;; Activate Eglot in cross-referenced non-project files
  (eglot-extend-to-xref t)
  :hook
  (python-mode            . eglot-ensure)
  ((bash-ts-mode sh-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
	       `(python-mode . ,jess/python-server-program)))


(provide 'jess-lsp)
