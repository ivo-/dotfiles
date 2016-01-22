(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun packages-install (my-packages)
  (dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))))

(defun init-install-packages ()
  (packages-install
   '(
     dash
     smex
     anzu
     use-package
     ido-ubiquitous
     ido-vertical-mode

     projectile
     exec-path-from-shell

     god-mode
     iy-go-to-char
     ace-jump-mode
     ace-jump-buffer
     expand-region
     multiple-cursors

     guide-key
     git-gutter
     indent-guide
     git-timemachine
     golden-ratio
     dired-details
     browse-kill-ring
     volatile-highlights
     idle-highlight-mode

     moe-theme
     monokai-theme
     zenburn-theme
     twilight-theme
     fancy-narrow
     htmlize

     gist
     magit
     prodigy
     quickrun
     restclient
     google-translate

     flycheck

     yari
     rbenv
     rubocop

     anaconda-mode

     tern
     js2-mode

     go-mode
     lua-mode
     php-mode
     web-mode
     slim-mode
     yaml-mode
     haml-mode
     scss-mode
     less-css-mode
     coffee-mode
     sourcemap
     markdown-mode

     cider
     smartparens
     clojure-mode
     clojure-cheatsheet

     know-your-http-well)))

(condition-case nil
    (init-install-packages)
  (error
   (package-refresh-contents)
   (init-install-packages)))

(require 'use-package)

;; =============================================================================
;; Build-in

(use-package midnight) ;; Clean up obsolete buffers automatically.

(use-package dired-x   ;; Load some advanced dired functions.
  :bind (("M-j d"   . dired-jump)
         ("M-j M-d" . dired-jump)))

(use-package uniquify  ;; When several buffers visit identically-named files.
  :config (setq uniquify-buffer-name-style 'forward))

(use-package saveplace ;; Save point position.
  :config
  (progn (setq-default save-place t)
         (setq save-place-file "~/.emacs.d/places")))

(use-package anzu
  :config
  (global-anzu-mode +1))

(use-package ispell
  :config
  (progn
    (when (executable-find ispell-program-name)
      (add-hook 'text-mode-hook 'turn-on-flyspell))))

(use-package flyspell
  :config
  (progn
    (define-key flyspell-mode-map (kbd "C-;") nil)))

(use-package ido
  :init
  (progn
    (add-hook 'ido-setup-hook
              '(lambda ()
                 (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
                 (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))
    (ido-mode t))
  :config
  (progn
    (use-package ido-ubiquitous
      :init (ido-ubiquitous-mode +1))

    (setq ido-max-prospects 10
          ido-enable-prefix nil
          ido-use-virtual-buffers t
          ido-enable-flex-matching t
          ido-create-new-buffer 'always)

    (add-to-list 'ido-ignore-buffers "\\*eshell")

    (use-package ido-vertical-mode
      :init (ido-vertical-mode t))))

(use-package re-builder
  :config (define-key reb-mode-map (kbd "C-c C-r") 'reb-query-replace))

(use-package org
  :config
  (progn
    ;; Don't export postambles.
    (setq org-export-html-postamble nil)))

(use-package dired
  :config
  (progn
    ;; Always delete and copy recursively.
    (setq dired-recursive-deletes 'top)
    (setq dired-recursive-copies 'always)

    ;; Refresh dired, but be quiet about it.
    (setq global-auto-revert-non-file-buffers t)
    (setq auto-revert-verbose nil)

    ;; Reuse current buffer by opening with 'a'.
    (put 'dired-find-alternate-file 'disabled nil)

    (define-key dired-mode-map (kbd "<S-return>") 'dired-find-file)
    (define-key dired-mode-map (kbd "<return>") 'dired-smart-open-file)
    (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

    (defun dired-smart-open-file ()
      (interactive)
      (let* (
             (suffixMap
              `(
                ("mp3" . "cvlc")
                ("wav" . "cvlc")
                ("pdf" . "google-chrome-stable")
                ))
             (fname (dired-get-file-for-visit))
             (fext (file-name-extension fname))
             (cmd (cdr (assoc fext suffixMap))))
        (if cmd
            (shell-command (concat cmd " \"" fname "\""))
          (dired-find-alternate-file))))

    (defun dired-start-sxiv ()
      (interactive)
      (shell-command (concat "sxiv -r " "\""
                             (dired-get-file-for-visit) "\"")))))

(use-package ruby-mode
  :mode (("\\.rake$" . ruby-mode)
         ("\\.thor$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("\\.ru$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Thorfile$" . ruby-mode)
         ("Gemfile$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Vagrantfile$" . ruby-mode))
  :init (add-hook 'ruby-mode-hook 'rubocop-mode)
  :config
  (progn
    (setq ruby-insert-encoding-magic-comment nil)
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren nil)))

;; =============================================================================
;; External

;; -----------------------------------------------------------------------------
;; Utilities

(use-package dash)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package quickrun
  :bind (("M-j b r" . quickrun)
         ("M-j b R" . quickrun-shell)))

(use-package dired-details
  :config (progn
            (setq-default dired-details-hidden-string "--- ")
            (dired-details-install)))

(use-package multiple-cursors
  :commands (mc/mark-all-like-this
             mc/mark-next-like-this
             mc/mark-previous-like-this)
  :bind (("M-n"   . mc/mark-next-like-this)
         ("C->"   . mc/mark-next-like-this)
         ("M-p"   . mc/mark-previous-like-this)
         ("C-<"   . mc/mark-previous-like-this)
         ("M-j a" . mc/mark-all-like-this))
  :config (add-to-list 'mc/unsupported-minor-modes 'flyspell-mode))

(require 'volatile-highlights)
(volatile-highlights-mode t)

(use-package golden-ratio
  :bind (("M-j w" . golden-ratio)
         ("M-j M-w" . golden-ratio))
  :init (golden-ratio-mode 1))

(use-package guide-key
  :config (progn (setq guide-key/guide-key-sequence t)
                 (setq guide-key/recursive-key-sequence-flag t)
                 (setq guide-key/popup-window-position 'right))
  :init (guide-key-mode 1))

(use-package flycheck
  :init (global-flycheck-mode +1))

(use-package yari
  :config (define-key 'help-command (kbd "R") 'yari))

(use-package gist
  :bind (("M-j g l" . gist-list)
         ("M-j g r" . gist-region)
         ("M-j g b" . gist-buffer)))

(use-package smex
  :bind ("M-x" . smex)
  :config (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package expand-region
  :bind ("C-;" . er/expand-region))

(use-package ace-jump-mode
  :bind (("M-j j"   . ace-jump-mode)
         ("M-j M-j" . ace-jump-char-mode)))

(use-package ace-jump-buffer
  :bind ("M-j b l" . ace-jump-buffer))

(use-package browse-kill-ring
  :bind (("M-j k" . browse-kill-ring)
         ("M-j M-k" . browse-kill-ring)))

(use-package iy-go-to-char
  :bind (("M-Z" . iy-go-to-char)
         ("M-F" . iy-go-to-char-continue)
         ("M-B" . iy-go-to-char-continue-backward)))

(use-package magit
  :bind (("C-c s" . magit-status)))

(use-package google-translate
  :bind (("M-j t"   . google-translate-at-point)
         ("M-j T"   . google-translate-at-point-reverse)
         ("M-j M-t" . google-translate-at-point)
         ("M-j M-T" . google-translate-at-point-reverse))
  :init (progn (setq google-translate-default-source-language "en")
               (setq google-translate-default-target-language "bg")))

(use-package projectile
  :bind (("C-c C-f" . projectile-find-file)
         ("C-c C-d" . projectile-find-dir))
  :init (projectile-global-mode))

(use-package rbenv
  :bind (("M-j n d" . git-gutter:next-diff))
  :config (global-rbenv-mode))

(use-package git-gutter
  :init (global-git-gutter-mode +1))

(use-package indent-guide
  :init (indent-guide-mode +1))

(use-package scss-mode
  :config (progn
            (setq scss-compile-at-save nil)
            (setq css-indent-offset 2)))

(use-package god-mode
  :bind ("M-<return>" . god-local-mode)
  :config (progn
            (defun god-mode-update-cursor ()
              (setq cursor-type
                    (if (or god-local-mode buffer-read-only) 'hbar 'box)))

            (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
            (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor)))

(use-package know-your-http-well)

;; Smartparens
(use-package smartparens-config
  :bind ("C-K" . sp-kill-hybrid-sexp)
  :init
  (progn
    (setq sp-base-key-bindings 'paredit)
    (setq sp-hybrid-kill-entire-symbol nil)

    (smartparens-global-mode t)
    (show-smartparens-global-mode t)
    (sp-use-paredit-bindings)

    (add-hook 'lisp-mode-hook 'smartparens-strict-mode)
    (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)))

;; -----------------------------------------------------------------------------
;; Major modes

(use-package js2-mode
  :mode ("\\.jsx?\\'" . js2-mode)
  :config
  (progn
    (define-key js2-mode-map (kbd "M-j") nil)
    (custom-set-variables
     '(js2-basic-offset 2)
     '(js2-strict-missing-semi-warning nil)
     '(js2-missing-semi-one-line-override t))))

(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

(use-package coffee-mode
  :mode ("\\.coffee\\'" . coffee-mode)
  :init
  (progn
    ;; CoffeeScript uses two spaces.
    (custom-set-variables '(coffee-tab-width 2))

    ;; *Messages* spam
    (setq coffee-debug-mode t)

    (setq coffee-args-compile '("-c" "-m")) ;; generating sourcemap

    ;; If you want to remove sourcemap file after jumping corresponding point
    (defun my/coffee-after-compile-hook (props)
      (sourcemap-goto-corresponding-point props)
      (delete-file (plist-get props :sourcemap)))
    (add-hook 'coffee-after-compile-hook 'my/coffee-after-compile-hook)))

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
         ("\\.eco\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
  :init
  (progn
    (add-hook 'web-mode-hook
              '(lambda ()
                 ;; Disable whitespace-mode.
                 (whitespace-mode -1)

                 (setq web-mode-markup-indent-offset 2)
                 (setq web-mode-css-indent-offset 2)
                 (setq web-mode-code-indent-offset 4)
                 (setq web-mode-disable-autocompletion t)

                 (local-set-key (kbd "RET") 'newline-and-indent)))))

(use-package clojure-mode
  :mode ("\\.edn$" . clojure-mode)
  :init
  (use-package cider
    :init
    (progn
      (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))
    :config
    (progn
      (defun cider-eval-last-sexp-and-append ()
        "Evaluate the expression preceding point and append result."
        (interactive)
        (let ((last-sexp (cider-last-sexp)))
          ;; we have to be sure the evaluation won't result in an error
          (cider-eval-and-get-value last-sexp)
          (with-current-buffer (current-buffer)
            (insert ";;=>"))
          (cider-interactive-eval-print last-sexp)))
      (setq nrepl-hide-special-buffers t)
      (setq cider-auto-select-error-buffer t)
      (setq nrepl-buffer-name-show-port t)
      (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
      (define-key clojure-mode-map (kbd "C-3") 'cider-eval-last-sexp) ;; piano key
      (define-key cider-mode-map (kbd "C-c C-f") nil)
      (define-key cider-repl-mode-map (kbd "C-c E") 'cider-eval-last-sexp-and-append)
      (define-key cider-repl-mode-map (kbd "C-c C-z") 'delete-window)
      (define-key cider-repl-mode-map (kbd "C-c C-h") 'clojure-cheatsheet)))
  :config
  (progn
    (setq clojure-defun-style-default-indent t)
    (define-key clojure-mode-map (kbd "C-c C-h") 'clojure-cheatsheet)))

(use-package prolog
  :mode ("\\.pl$" . prolog-mode)
  :config
  (progn
    (setq prolog-system 'swi)
    (define-key prolog-mode-map (kbd "C-c M-j") 'run-prolog)
    (define-key prolog-mode-map (kbd "C-c M-z") 'run-prolog)
    (define-key prolog-mode-map (kbd "C-c M-z") 'prolog-consult-file)))

(use-package python
  :config (define-key inferior-python-mode-map (kbd "C-l") 'eshell/clear))

;; =============================================================================
;; Hooks

(add-hook 'prog-mode-hook 'add-watchwords)
(add-hook 'prog-mode-hook 'shorten-lambdas)
(add-hook 'prog-mode-hook 'fancy-narrow-mode)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook '(lambda () (idle-highlight-mode t)))
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'indent-guide-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(add-hook 'before-save-hook (lambda ()
                              (when (not (eq major-mode 'go-mode))
                                (call-interactively 'cleanup-buffer-safe))))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(add-hook 'js2-mode-hook 'tern-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'go-mode-hook (lambda ()
                          (let ((goimports (executable-find "goimports")))
                            (when goimports
                              (setq gofmt-command goimports)))
                          (set (make-local-variable 'whitespace-style) nil)
                          (add-hook 'before-save-hook 'gofmt-before-save nil t)))

;; =============================================================================
;; Hide some minor modes

(require 'diminish)

(diminish 'git-gutter-mode)
(diminish 'golden-ratio-mode)
(diminish 'anzu-mode)
(diminish 'global-whitespace-mode)
(diminish 'volatile-highlights-mode)
(diminish 'subword-mode)
(diminish 'visual-line-mode)
(diminish 'auto-fill-function)
(diminish 'indent-guide-mode)

(provide 'setup-packages)
