(defun create-buffer nil
  "Create empty buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create
                     (read-shell-command "Buffer name: "))))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a 'before-save-hook', and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

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

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (-dotimes arg
        (lambda (n)
          (goto-char end)
          (newline)
          (insert region)
          (indent-for-tab-command)
          (setq end (point))))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun insert-date ()
  "Insert data at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S" (current-time))))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

   Move point to the first non-whitespace character on this line.
   If point is already there, move to the ppbeginning of the line.
   Effectively toggle between the first non-whitespace character and
   the beginning of the line.

   If ARG is not nil or 1, move forward ARG - 1 lines first.  If
   point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun sudo-edit (&optional arg)
  "Edit current file as sudo."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun kill-other-buffer-and-window ()
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((current (selected-window))
        (other (next-window)))
    (select-window other)
    (kill-this-buffer)
    (select-window current)
    (delete-other-windows)))

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

(defun join-line-or-lines-in-region ()
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-line))))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

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

(provide 'setup-defuns)
