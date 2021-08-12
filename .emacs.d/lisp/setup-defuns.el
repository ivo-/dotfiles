(defun create-buffer nil
  "Create empty buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create
                     (read-shell-command "Buffer name: "))))

(defun move-line-up ()
  "Move line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun generalized-shell-command (command arg)
  "Unifies `shell-command' and `shell-command-on-region'. If no region is
   selected, run a shell command just like M-x shell-command (M-!).  If no
   region is selected and an argument is a passed, run a shell command and place
   its output after the mark as in C-u M-x `shell-command' (C-u M-!).  If a
   region is selected pass the text of that region to the shell and replace the
   text in that region with the output of the shell command as in C-u M-x
   `shell-command-on-region' (C-u M-|). If a region is selected AND an argument
   is passed (via C-u) send output to another buffer instead of replacing the
   text in region."
  (interactive (list (read-from-minibuffer "Shell command: " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (let ((p (if mark-active (region-beginning) 0))
        (m (if mark-active (region-end) 0)))
    (if (= p m)
        ;; No active region
        (if (eq arg nil)
            (shell-command command)
          (shell-command command t))
      ;; Active region
      (if (eq arg nil)
          (shell-command-on-region p m command t t)
        (shell-command-on-region p m command)))))

(defun shell-command-file (command arg)
  "Run shell command for current file. Current file name will be attached as last argument."
  (interactive (list (read-from-minibuffer
                      "Shell command to run for current file: " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (shell-command (concat command " " (buffer-file-name)) t))

(defun run-shell-command-from-selection (start end)
  "Run selected region as shell command and show the response."
  (interactive "r")
  (shell-command (buffer-substring start end)))

(defun hide-eshell ()
  (interactive)
  (let (register-name)
    (setq register-name :eshell-pre-window-conf)
    (set-register register-name nil)))

(defun toggle-eshell (num)
  (let ((eshell-buffer-name (concat "*eshell-" num "*"))
        (register-name :eshell-pre-window-conf))
    (if (string= (concat " " eshell-buffer-name) (buffer-name))
        ;;
        (progn (jump-to-register register-name)
               (set-register register-name nil))

      ;;
      (when (eq nil (get-register register-name))
        (window-configuration-to-register register-name))

      (if (not (eq nil (get-buffer (concat " " eshell-buffer-name))))
          (switch-to-buffer (get-buffer (concat " " eshell-buffer-name)))
        (call-interactively 'eshell)
        (rename-buffer (concat " " eshell-buffer-name)))
      (delete-other-windows))))

(defun toggle-eshell-1 () (interactive) (toggle-eshell "1"))
(defun toggle-eshell-2 () (interactive) (toggle-eshell "2"))
(defun toggle-eshell-3 () (interactive) (toggle-eshell "3"))
(defun toggle-eshell-4 () (interactive) (toggle-eshell "4"))

(defun toggle-eshell-show-all ()
  (interactive)
  (let ((register-name :eshell-pre-window-conf))
    (if (= (length (window-list)) 4)
        (progn (jump-to-register register-name)
               (set-register register-name nil))
      ;; else
      (when (eq nil (get-register register-name))
        (window-configuration-to-register register-name))
      (delete-other-windows)
      (switch-to-buffer " *eshell-1*")
      (split-window-right)
      (switch-to-buffer " *eshell-2*")
      (split-window-below)
      (switch-to-buffer " *eshell-3*")
      (windmove-right)
      (split-window-below)
      (switch-to-buffer " *eshell-4*")
      (windmove-left))
    ))

(provide 'setup-defuns)
