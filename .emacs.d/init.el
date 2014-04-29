;;; TODO:
;;;
;;;   - [ ] Setup erc.
;;;   - [ ] Review docs file.
;;;   - [ ] Tagedit.
;;;   - [ ] Setup zsh + multi-term(instead of eshell).
;;;   - [ ] Setup smartparens-strict for all prog modes.
;;;   - [ ] http://bzg.fr/emacs-strip-tease.html
;;;   - [ ] https://github.com/m2ym/popwin-el
;;;   - [ ] https://github.com/yjwen/org-reveal
;;;   - [ ] https://github.com/alexander-yakushev/compliment
;;;   - [ ] Move to nth column (fill with spaces)
;;;   - [ ] https://github.com/Bruce-Connor/fancy-narrow
;;;   - [ ] https://github.com/mbunkus/mo-git-blame
;;;   - [ ] clojure auto complete
;;;   - [ ] cljs repl

;; Set up load path.
(add-to-list 'load-path user-emacs-directory)

;; Keep emacs custom settings in custom file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Free personal key bindings space.
(global-unset-key (kbd "M-j"))

(require 'setup-settings)
(require 'setup-packages)
(require 'setup-services)
(require 'setup-keybindings)

(require 'setup-themes)
(require 'setup-defuns)

;; Always keep init.el buffer.
(find-file "~/.emacs.d/init.el")

;; Don't need the *scratch* buffer.
(kill-buffer "*scratch*")

;; Start server.
(require 'server)
(unless (server-running-p)
  (server-start))
