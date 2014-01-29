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
     use-package
     ido-ubiquitous

     projectile
     ack-and-a-half

     god-mode
     iy-go-to-char
     ace-jump-mode
     expand-region
     multiple-cursors

     git-gutter
     golden-ratio
     dired-details
     idle-highlight-mode
     volatile-highlights

     zenburn-theme
     twilight-theme

     gist
     magit
     prodigy
     quickrun
     restclient
     google-translate

     rvm
     yari
     rubocop

     lua-mode
     php-mode
     web-mode
     yaml-mode
     haml-mode
     scss-mode
     coffee-mode
     markdown-mode

     cider
     paredit
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

(use-package ispell
  :config
  (progn
    (when (executable-find ispell-program-name)
      (add-hook 'text-mode-hook 'turn-on-flyspell))))

(use-package ido
  :init (ido-mode t)
  :config
  (progn
    (use-package ido-ubiquitous
      :init (ido-ubiquitous-mode))
    (setq ido-max-prospects 10
          ido-enable-prefix nil
          ido-use-virtual-buffers t
          ido-enable-flex-matching t
          ido-create-new-buffer 'always)))

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
                ("pdf" . "google-chrome")
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
    (setq ruby-indent-level 2)
    (setq ruby-deep-indent-paren nil)))

;; =============================================================================
;; External

;; -----------------------------------------------------------------------------
;; Utilities

(use-package dash)

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

(use-package volatile-highlights
  :init (volatile-highlights-mode t))

(use-package golden-ratio
  :bind (("M-j w" . golden-ratio)
         ("M-j M-w" . golden-ratio))
  :init (golden-ratio-mode 1))

;; TODO: Remove after Despark.
(use-package rvm
  :config (rvm-use-default))

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

(use-package iy-go-to-char
  :bind (("M-Z" . iy-go-to-char)
         ("M-F" . iy-go-to-char-continue)
         ("M-B" . iy-go-to-char-continue-backward)))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package google-translate
  :bind (("M-j t"   . google-translate-at-point)
         ("M-j T"   . google-translate-at-point-reverse)
         ("M-j M-t" . google-translate-at-point)
         ("M-j M-T" . google-translate-at-point-reverse))
  :config (progn (setq google-translate-default-source-language "en")
                 (setq google-translate-default-target-language "bg")))

(use-package projectile
  :bind ("C-c C-f" . projectile-find-file)
  :init (projectile-global-mode))


(use-package git-gutter
  :init (global-git-gutter-mode +1))

(use-package scss-mode
  :config (setq scss-compile-at-save nil))

(use-package god-mode
  :bind ("M-<return>" . god-local-mode)
  :config (progn
            (defun god-mode-update-cursor ()
              (setq cursor-type
                    (if (or god-local-mode buffer-read-only) 'hbar 'box)))

            (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
            (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor)))

(use-package know-your-http-well)

;; -----------------------------------------------------------------------------
;; Major modes

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         ("\\.markdown$" . markdown-mode)))

(use-package coffee-mode
  :init
  (add-hook 'coffee-mode-hook
            '(lambda ()
               ;; CoffeeScript uses two spaces.
               (set (make-local-variable 'tab-width) 2)

               ;; *Messages* spam
               (setq coffee-debug-mode t))))

(use-package web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.blade\\.php\\'" . web-mode)
         ("\\.jsp\\'" . web-mode)
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

                 (setq web-mode-markup-indent-offset 4)
                 (setq web-mode-css-indent-offset 2)
                 (setq web-mode-code-indent-offset 4)
                 (setq web-mode-disable-autocompletion t)

                 (local-set-key (kbd "RET") 'newline-and-indent)))))

(use-package clojure-mode
  :mode ("\\.edn$" . clojure-mode)
  :init
  (progn
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (use-package cider
      :init
      (progn
        (add-hook 'cider-repl-mode-hook 'paredit-mode)
        (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))
      :config
      (progn
        (setq nrepl-hide-special-buffers t)
        (setq cider-auto-select-error-buffer t)
        (setq nrepl-buffer-name-show-port t)
        (setq cider-repl-history-file "~/.emacs.d/nrepl-history")
        (define-key cider-repl-mode-map (kbd "C-c C-z") 'delete-window)
        (define-key cider-repl-mode-map (kbd "C-c C-h") 'clojure-cheatsheet))))
  :config
  (progn
    (setq clojure-defun-style-default-indent t)
    (define-key clojure-mode-map (kbd "C-c C-h") 'clojure-cheatsheet)))

(use-package prolog-mode
  :config
  (progn
    (define-key prolog-mode-map (kbd "C-c M-j") 'run-prolog)
    (define-key prolog-mode-map (kbd "C-c C-k") 'prolog-consult-file)))


;; =============================================================================
;; Hooks

(add-hook 'prog-mode-hook 'add-watchwords)
(add-hook 'prog-mode-hook 'shorten-lambdas)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook '(lambda () (idle-highlight-mode t)))

(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(add-hook 'before-save-hook 'cleanup-buffer-safe)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

(provide 'setup-packages)
