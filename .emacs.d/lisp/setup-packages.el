(require 'cl)
(require 'package)

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

;; =============================================================================

(use-package ag :ensure t)                 ; Ag interface
(use-package htmlize :ensure t)            ; HTML-ize buffer code
(use-package git-timemachine :ensure t)    ; Go trough file git history.
(use-package rainbow-delimiters :ensure t) ; Use different colors for parentheses

;; Better search and replace
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode))

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

;; Show git diff in edited files
(use-package git-gutter
  :ensure t
  :bind (("M-j n d" . git-gutter:next-hunk))
  :config (global-git-gutter-mode +1))

;; Easy navigation without modifier keys
(use-package god-mode
  :ensure t
  :bind ("M-<return>" . god-local-mode)
  :config
  (defun god-mode-update-cursor () (setq cursor-type (if (or god-local-mode buffer-read-only) 'hbar 'box)))
  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor))

;; Automatically resize windows
(use-package golden-ratio
  :ensure t
  :bind (("M-j w" . golden-ratio)
         ("M-j M-w" . golden-ratio))
  :init (golden-ratio-mode 1))

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
  :bind (("M-Z" . iy-go-to-char)
         ("M-F" . iy-go-to-char-continue)
         ("M-B" . iy-go-to-char-continue-backward)))

;; Save buffer when they loose focus
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1))

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
         ("C-<return>" . er/expand-region)))

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

(use-package magit
  :ensure t
  :bind (("C-c s" . magit-status)))

(use-package quickrun
  :ensure t
  :bind (("M-j b r" . quickrun)
         ("M-j b R" . quickrun-shell)))

(use-package google-translate
  :ensure t
  :bind (("M-j t"   . google-translate-at-point)
         ("M-j T"   . google-translate-at-point-reverse)
         ("M-j M-t" . google-translate-at-point)
         ("M-j M-T" . google-translate-at-point-reverse))
  :config
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "bg"))

(use-package fancy-narrow
  :ensure t
  :bind ("M-j c" . fancy-narrow-or-widen-dwim)
  :config
  (defun fancy-narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first.  Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (fancy-narrow-active-p) (not p)) (fancy-widen))
          ((region-active-p)
           (fancy-narrow-to-region (region-beginning)
                                   (region-end)))
          (t (fancy-narrow-to-defun)))))

(use-package flycheck
  :ensure t
  :bind (("M-j b c" . flycheck-buffer))
  :config
  (global-flycheck-mode)
  (setq flycheck-display-errors-delay 0.2)
  (defun use-js-executables-from-node-modules ()
    "Set executables of JS checkers from local node modules."
    (interactive)
    (-when-let* ((file-name (buffer-file-name))
                 (root (locate-dominating-file file-name "node_modules"))
                 (module-directory (expand-file-name "node_modules" root)))
      (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                             (javascript-eslint . "eslint")
                                             (javascript-jscs   . "jscs")))
        (let ((package-directory (expand-file-name module module-directory))
              (executable-var (flycheck-checker-executable-variable checker)))
          (when (file-directory-p package-directory)
            (set (make-local-variable executable-var)
                 (expand-file-name (concat "bin/" module ".js")
                                   package-directory)))))))
  (add-hook 'flycheck-mode-hook 'use-js-executables-from-node-modules))

(use-package yaml-mode :ensure t)
(use-package scss-mode
  :ensure t
  :config
  (setq scss-compile-at-save nil))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

(use-package rbenv
  :ensure t
  :config (global-rbenv-mode))

(use-package js2-mode :ensure t)
(use-package prettier-js
  :ensure t
  :config
  (setq prettier-js-args
        '(
          "--single-quote"
          "--trailing-comma" "es5"
          "--jsx-bracket-same-line")))

(use-package rjsx-mode
  :ensure t
  :mode ("\\.jsx?\\'" . rjsx-mode)
  :init (electric-indent-mode -1)
  :config
  (define-key js2-mode-map (kbd "M-j") nil)
  (define-key js2-mode-map (kbd "C-c C-f") nil)
  (custom-set-variables
   '(js2-basic-offset 2)
   '(js2-indent-switch-body t)
   '(js2-strict-missing-semi-warning t)
   '(js2-missing-semi-one-line-override t)
   '(js2-strict-trailing-comma-warning t)))

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
(use-package clojure-cheatsheet :ensure t)
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

(use-package prolog
  :mode ("\\.pl$" . prolog-mode)
  :config
  (progn
    (setq prolog-system 'swi)
    (define-key prolog-mode-map (kbd "C-c M-j") 'run-prolog)
    (define-key prolog-mode-map (kbd "C-c M-z") 'run-prolog)
    (define-key prolog-mode-map (kbd "C-c M-z") 'prolog-consult-file)))

(use-package smartparens
   :ensure t
  ;; :ensure t
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

(use-package avy
  :ensure t
  :bind (("M-j j" . avy-goto-word-or-subword-1)
         ("M-j M-j" . avy-goto-char))
  :config
  (setq avy-background t))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package tern
  :ensure t
  :config
  (setq tern-command (append tern-command '("--no-port-file"))))

(use-package company-flx :ensure t)
(use-package company-tern :ensure t)
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (company-flx-mode +1)
  (add-to-list 'company-backends 'company-tern)
  (global-set-key (kbd "<C-tab>") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package imenu-anywhere
  :ensure t
  :bind (("M-j i" . imenu-anywhere)))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "<return>") 'ivy-alt-done)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

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
  (global-set-key (kbd "M-j g g") 'counsel-ag)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package diminish
  :ensure t
  :init
  (progn
    (diminish 'git-gutter-mode)
    (diminish 'golden-ratio-mode)
    (diminish 'anzu-mode)
    (diminish 'global-whitespace-mode)
    (diminish 'volatile-highlights-mode)
    (diminish 'subword-mode)
    (diminish 'visual-line-mode)
    (diminish 'auto-fill-function)
    (diminish 'indent-guide-mode)))

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

(use-package paren
  :config
  (show-paren-mode +1))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

 ; Save point position.
(require 'saveplace)
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

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package flyspell
  :config
  (define-key flyspell-mode-map (kbd "C-;") nil))

(use-package ispell
  :bind (("M-j s"   . 'ispell-word)
         ("M-j M-s" . 'ispell-word))
  :config
  (setq-default ispell-program-name "aspell")
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

(use-package re-builder
  :config
  (define-key reb-mode-map (kbd "C-c C-r") 'reb-query-replace))

(require 'dired-x)
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
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))))

;; (use-package dash :ensure t)

(add-hook 'prog-mode-hook 'add-watchwords)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(provide 'setup-packages)
