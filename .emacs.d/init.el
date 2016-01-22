;; TODO:
;;
;;   - [ ] cleanup
;;   - [ ] review docs file
;;   - [ ] company-mode
;;   - [ ] https://github.com/skeeto/skewer-mode
;;   - [ ] https://github.com/ch11ng/exwm/wiki
;;   - [ ] http://paralambda.org/2012/07/02/using-gnu-emacs-as-a-terminal-emulator/
;;   - [ ] http://rawsyntax.com/blog/learn-emacs-zsh-and-multi-term/
;;

;; Set up load path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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
