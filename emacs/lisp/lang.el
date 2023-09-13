;; Python
(defun jess/pyrightconfig-write (virtualenv)
  (let* (;; file-truename and tramp-file-local-name ensure that neither `~' nor
         ;; the Tramp prefix (e.g. "/ssh:my-host:") wind up in the final
         ;; absolute directory path.
         (venv-dir (tramp-file-local-name (file-truename virtualenv)))

         ;; Given something like /path/to/venv/, this strips off the trailing `/'.
         (venv-file-name (directory-file-name venv-dir))

         ;; Naming convention for venvPath matches the field for
         ;; pyrightconfig.json.  `file-name-directory' gets us the parent path
         ;; (one above venv).
         (venvPath (file-name-directory venv-file-name))

         ;; Grabs just the `venv' off the end of the venv-file-name.
         (venv (file-name-base venv-file-name))

         ;; Eglot demands that `pyrightconfig.json' is in the project root
         ;; folder.
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))

         ;; Finally, get a string with the JSON payload.
         (out-contents (json-encode (list :venvPath venvPath :venv venv))))

    ;; Emacs uses buffers for everything.  This creates a temp buffer, inserts
    ;; the JSON payload, then flushes that content to final `pyrightconfig.json'
    ;; location
    (with-temp-file out-file (insert out-contents))))

(defun jess/pyvenv-auto-venv ()
  (pyvenv-auto--locate-venvs
   default-directory
   pyvenv-auto-venv-dirnames))

(defun jess/pyvenv-pyright-auto-setup ()
  (progn
    (require 'pyvenv-auto)
    (pyvenv-auto-run)
    (jess/pyrightconfig-write (jess/pyvenv-auto-venv))))

(use-package pyvenv-auto
  :hook
  (python-mode . jess/pyvenv-auto-venv))

(defconst jess/python-lsp-server '("pyright-langserver" "--stdio"))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       `(python-mode . ,jess/python-lsp-server)))

(use-package eglot
  :ensure nil
  :hook
  (python-mode . eglot-ensure)
  :custom
  (eglot-extend-to-xref t))
