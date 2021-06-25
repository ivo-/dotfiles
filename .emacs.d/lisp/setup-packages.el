(require 'cl)
(require 'package)

;;; =============================================================================
;;; Settings

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

;; Eshell has troubles with more.
(setenv "PAGER" (executable-find "cat"))

(blink-cursor-mode -1)      ; Disable cursor blinking
(column-number-mode t)      ; Show current column
(delete-selection-mode t)   ; Delete selection when start writing.
(global-auto-revert-mode t) ; Auto revert file if it is changed on the disk.
(global-hl-line-mode +1)    ; Highlight the current line
(global-subword-mode 1)     ; Navigate trough camel cased words.
(global-visual-line-mode t) ; Operate on visual lines instead of logical lines.
(line-number-mode t)        ; Show current line
(size-indication-mode t)    ; Always display file size, line and column numbers.
(tooltip-mode -1)           ; Display help text in the echo area.
(electric-indent-mode +1)   ; Automatically re-indentation on some commands.

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

;;; =============================================================================
;;; Packages

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(use-package ag :ensure t)                 ; Ag interface
(use-package dumb-jump :ensure t)          ; Dumb jump to definition
(use-package rainbow-delimiters :ensure t) ; Use different colors for parentheses
(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

;; Show vertical indentation lines
(use-package indent-guide
  :ensure t
  :config
  (indent-guide-mode +1)
  (add-hook 'prog-mode-hook 'indent-guide-mode))

;; Show available keybindings on inactivity
(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

;; Show git diff in edited files
(use-package diff-hl
  :ensure t
  :bind (("M-j n d" . diff-hl-next-hunk))
  :init
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))
(global-diff-hl-mode +1)

;; Automatically resize windows
(use-package golden-ratio
  :ensure t
  :bind (("M-j w" . golden-ratio)
         ("M-j M-w" . golden-ratio))
  :init (golden-ratio-mode 1))

;; Brings visual feedback to some operations
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

;; Sets timer that highlights all occurrences in the buffer of the word
;; under the point.
(use-package idle-highlight-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook '(lambda () (idle-highlight-mode t))))

;; Hide some annoying file details in dired
(use-package dired-details
  :ensure t
  :config
  (setq-default dired-details-hidden-string "--- ")
  (dired-details-install))

;; Jump to char in the same row
(use-package iy-go-to-char
  :ensure t
  :bind (("M-Z" . iy-go-to-char)))

;; Save buffer when they loose focus
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

;; Load PATH from the user's shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

;; Expand selection
(use-package expand-region
  :ensure t
  :bind (("C-;" . er/expand-region)
         ("C-'" . er/expand-region)
         ("C-<return>" . er/expand-region)
         ("M-<return>" . er/expand-region)))

(use-package multiple-cursors
  :ensure t
  :commands (mc/mark-all-like-this
             mc/mark-next-like-this
             mc/mark-previous-like-this)
  :bind (("M-n"   . mc/mark-next-like-this)
         ("C->"   . mc/mark-next-like-this)
         ("M-p"   . mc/mark-previous-like-this)
         ("C-<"   . mc/mark-previous-like-this)
         ("M-j a" . mc/mark-all-like-this))
  :config (add-to-list 'mc/unsupported-minor-modes 'flyspell-mode))

(use-package crux
  :ensure t
  :bind (("M-j f d" . crux-delete-file-and-buffer)
         ("M-j f r" . crux-rename-buffer-and-file)
         ("M-j e" . crux-eval-and-replace)
         ("M-j M-e" . crux-eval-and-replace)
         ("M-K" . crux-kill-whole-line)))

(use-package magit
  :ensure t
  :bind (("C-c s" . magit-status)))

(use-package google-translate
  :ensure t
  :bind (("M-j t"   . google-translate-at-point)
         ("M-j T"   . google-translate-at-point-reverse)
         ("M-j M-t" . google-translate-at-point)
         ("M-j M-T" . google-translate-at-point-reverse))
  :init
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "bg"))

(use-package flycheck
  :ensure t
  :bind (("M-j b c" . flycheck-buffer)
         ("M-j n e" . flycheck-next-error)))
(global-flycheck-mode) ; For whatever reason it doesn't work in :config

(use-package flycheck-color-mode-line
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(use-package yaml-mode :ensure t)
(use-package scss-mode :ensure t)
(use-package markdown-mode :ensure t :mode (("\\.md$" . markdown-mode)))

(use-package js2-mode :ensure t)
(use-package add-node-modules-path
  :ensure t
  :config
  (dolist (hook '(js2-mode-hook js-mode-hook typescript-mode-hook))
    (add-hook hook #'add-node-modules-path)))

(use-package prettier-js
  :ensure t
  :config
  (setq prettier-js-args
        '(
          ;; "--single-quote"
          ;; "--trailing-comma" "es5"
          ;; "--jsx-bracket-same-line"
          ))
  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'prettier-js-mode)))

(use-package rjsx-mode
  :ensure t
  :mode ("\\.jsx?\\'" . rjsx-mode)
  :init (electric-indent-mode -1)
  :config
  (define-key js2-mode-map (kbd "M-j") nil)
  (define-key js2-mode-map (kbd "C-c C-f") nil)
  (define-key js2-mode-map (kbd "M-.") 'dumb-jump-go)
  (custom-set-variables
   '(js2-basic-offset 2)
   '(js2-indent-switch-body t)
   '(js2-strict-missing-semi-warning t)
   '(js2-indent-on-enter-key nil)
   '(js2-missing-semi-one-line-override t)
   '(js2-strict-trailing-comma-warning nil)))

(use-package typescript-mode
  :ensure t
  :init (electric-indent-mode -1)
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

(use-package tide
  :ensure t
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package web-mode
  :ensure t
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.eco\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  :init (whitespace-mode -1)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 4)
  (local-set-key (kbd "RET") 'newline-and-indent))

(use-package clojure-mode :ensure t)
(use-package cider
  :ensure t
  :init
  :config
  (defun cider-eval-last-sexp-and-append ()
    "Evaluate the expression preceding point and append result."
    (interactive)
    (let ((last-sexp (cider-last-sexp)))
      ;; we have to be sure the evaluation won't result in an error
      (cider-eval-and-get-value last-sexp)
      (with-current-buffer (current-buffer)
        (insert ";;=>"))
      (cider-interactive-eval-print last-sexp)))
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (setq nrepl-hide-special-buffers t)
  (setq cider-auto-select-error-buffer t)
  (setq nrepl-buffer-name-show-port t)
  (define-key cider-mode-map (kbd "C-c C-f") nil)
  (define-key cider-repl-mode-map (kbd "C-c C-z") 'delete-window)
  (define-key cider-repl-mode-map (kbd "C-c C-h") 'clojure-cheatsheet))

(use-package smartparens
  :ensure t
  :bind ("C-K" . sp-kill-hybrid-sexp)
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-hybrid-kill-entire-symbol nil)

  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-use-paredit-bindings)

  (add-hook 'lisp-mode-hook 'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode))

(use-package zenburn-theme
  :ensure t
  :config
  (defadvice load-theme (before theme-dont-propagate activate)
    (mapcar #'disable-theme custom-enabled-themes))
  (defun zb ()
    (interactive)
    (load-theme 'zenburn t))

  (defun aw ()
    (interactive)
    (load-theme 'adwaita t))

  (defun db ()
    (interactive)
    (load-theme 'deeper-blue t))
  (load-theme 'zenburn t))

(use-package company-flx :ensure t)
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.3)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  (setq ivy-virtual-abbreviate 'full)
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-j h f") 'counsel-describe-function)
  (global-set-key (kbd "M-j h v") 'counsel-describe-variable)
  (global-set-key (kbd "M-j h l") 'counsel-find-library)
  (global-set-key (kbd "C-c C-f") 'counsel-git)
  (global-set-key (kbd "M-j g f") 'counsel-git)
  (global-set-key (kbd "M-j g g") 'counsel-git-grep)
  (global-set-key (kbd "M-j g a") 'counsel-ag)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(setq projectile-keymap-prefix (kbd "M-j p"))
(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1))


;; =============================================================================
;; Build-in

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

 ; Save point position.
(use-package saveplace
  :config
  (unless (file-exists-p "~/.emacs.d/places")
    (make-directory "~/.emacs.d/places"))
  (setq save-place-file "~/.emacs.d/places")
  (setq-default save-place t))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" my-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; Disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files.
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; On the fly spellcheck.
(use-package flyspell
  :config
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (global-set-key (kbd "M-j s") 'ispell-word)
  (global-set-key (kbd "M-j M-s") 'ispell-word)
  (setq-default ispell-program-name "aspell")
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(use-package dired
  :bind (("M-j d"   . dired-jump)
         ("M-j M-d" . dired-jump))
  :config
  ;; Always delete and copy recursively.
  (setq dired-recursive-deletes 'top)
  (setq dired-recursive-copies 'always)

  ;; Refresh dired, but be quiet about it.
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  ;; Reuse current buffer by opening with 'a'.
  (put 'dired-find-alternate-file 'disabled nil)

  (define-key dired-mode-map (kbd "<S-return>") 'dired-find-file)
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

  (defun mydired-sort ()
    "Sort dired listings with directories first."
    (save-excursion
      (let (buffer-read-only)
        (forward-line 2) ;; beyond dir. header
        (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
      (set-buffer-modified-p nil)))

  (defadvice dired-readin
      (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding mark."
    (mydired-sort))

  (require 'dired-x))

(use-package eshell
  :config
  (defun eshell/clear ()
    "Clear eshell buffer."
    (interactive)
    (let ((eshell-buffer-maximum-lines 0))
      (eshell-truncate-buffer))))

(use-package em-term
  :config
  (add-to-list 'eshell-visual-commands "emacs")
  (add-to-list 'eshell-visual-commands "htop"))

(use-package paren :config (show-paren-mode +1))
(use-package windmove :config (windmove-default-keybindings))
(use-package org-mode :config (add-hook 'org-mode-hook 'turn-on-auto-fill))
(use-package text-mode :config (add-hook 'text-mode-hook 'turn-on-auto-fill))
(use-package shell-mode :config (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

(provide 'setup-packages)
