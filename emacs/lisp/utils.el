(defun jess/get-macos-keychain-password (account)
  "Gets `acount' keychain password from macOS Keychain"
  (if (eq system-type 'darwin)
      (let* ((passwd
	      (shell-command-to-string
	       (concat  "security 2>&1 >/dev/null find-generic-password -ga " account)))
	     (passwd (nth 1 (split-string passwd " ")))
	     (passwd (string-replace "\"" "" passwd))
	     (passwd (string-replace "\n" "" passwd)))
	passwd)
    (message "System type mismatch, required: darwin")))

(defun jess/file-string (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))
