(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Free personal key bindings space.
(global-unset-key (kbd "M-j"))

;; Remove all distractions.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)                ; enable y/n answers

(setq-default fill-column 80)                ; Lines should be 80 chars.
(setq-default indent-tabs-mode nil)          ; Don't use tabs in indentation.

(setq echo-keystrokes 0.1)                   ; Show faster incomplete commands while typing them.
(setq gc-cons-threshold 50000000)            ; Reduce GC frequency
(setq inhibit-startup-screen t)              ; Disable startup screen
(setq large-file-warning-threshold 100000000); Warn when opening files bigger than 100MB
(setq load-prefer-newer t)                   ; Always load newest byte code
(setq require-final-newline t)               ; Newline at end of file
(setq ring-bell-function 'ignore)            ; Disable the bell ring
(setq save-interprogram-paste-before-kill t) ; Save selection from external programs in kill ring.
(setq select-enable-clipboard t)             ; Allow pasting selection outside of Emacs.
(setq tab-always-indent 'complete)           ; Smart tab behavior - indent or complete


;; nice scrolling
(setq scroll-margin 0)
(setq scroll-conservatively 100000)
(setq scroll-preserve-screen-position 1)

(blink-cursor-mode -1)      ; Disable cursor blinking
(column-number-mode t)      ; Show current column
(delete-selection-mode t)   ; Delete selection when start writing.
(global-auto-revert-mode t) ; Auto revert file if it is changed on the disk.
(global-hl-line-mode +1)    ; Highlight the current line
(global-subword-mode 1)     ; Navigate trough camel cased words.
(global-visual-line-mode t) ; Operate on visual lines instead of logical lines.
(line-number-mode t)        ; Show current line
(mouse-wheel-mode t)        ; Enable scrolling using mouse wheel.
(size-indication-mode t)    ; Always display file size, line and column numbers.
(tooltip-mode -1)           ; Display help text in the echo area.

;; (electric-indent-mode +1)   ; Automatically re-indentation on some commands.

;; Always use UTF-8.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)


;; Swap super and meta on OSX
(when (memq window-system '(mac ns))
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;; Create the savefile dir if it doesn't exist
(defconst my-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p my-savefile-dir)
  (make-directory my-savefile-dir))



;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))


;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Eshell has troubles with more.
(setenv "PAGER" (executable-find "cat"))

(provide 'setup-settings)
