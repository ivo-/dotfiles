;; Remove all distractions.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))

;; Lines should be 80 chars.
(setq-default fill-column 80)

;; Highlight current line.
(when window-system (global-hl-line-mode))

;; Allow pasting selection outside of Emacs.
(setq x-select-enable-clipboard t)

;; Save selection from external programs in kill ring.
(setq save-interprogram-paste-before-kill t)

;; Save all backup files in /backups.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

;; Automatically re-indentation on some commands.
(electric-indent-mode +1)

;; Delete selection when start writing.
(delete-selection-mode t)

;; Auto revert file if it is changed on the disk.
(global-auto-revert-mode t)

;; Don't use tabs in indentation.
(setq-default indent-tabs-mode nil)

;; Set 4 spaces tab stops.
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96 100 104 108 112))

;; Enable y or n answers.
(defalias 'yes-or-no-p 'y-or-n-p)

;; We have a lot of memmory nowdays.
(setq gc-cons-threshold 50000000)

;; Show faster incomplete commands while typing them.
(setq echo-keystrokes 0.1)

;; Always use UTF-8.
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Navigate trough camel cased words.
(global-subword-mode 1)

(when window-system
  ;; Display help text in the echo area.
  (tooltip-mode -1)

  ;; Enable scrolling using mouse wheel.
  (mouse-wheel-mode t)

  ;; Disable blink cursor.
  (blink-cursor-mode -1)

  ;; Prettify frame title.
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

;; No start up screen.
(setq inhibit-startup-message t)

;; Try to flash the frame to represent a bell instead
;; of destroying my ears.
(setq visible-bell t)

;; Use one color theme for all the frames.
(setq color-theme-is-global t)

;; White space mode settings.
(setq whitespace-style '(face trailing lines-tail tabs))
(setq whitespace-line-column 80)
(global-whitespace-mode t)

;; Show matching parens.
(show-paren-mode 1)

;; Operate on visual lines instead of logical lines.
(global-visual-line-mode t)

;; Show me empty lines after buffer end.
(set-default 'indicate-empty-lines t)

;; Always display file size, line and column numbers.
(size-indication-mode t)
(setq line-number-mode t)
(setq column-number-mode t)

;; Make a bigger font-size.
;; (set-face-attribute 'default nil :height 150)

;; Eshell has troubles with more.
(setenv "PAGER" (executable-find "cat"))

;; Use home as the default directory.
(setq default-directory (getenv "HOME"))

;; Hippie-expand preferences
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

;; Fix indentation in js-mode.
(setq js-indent-level 2)

(provide 'setup-settings)
