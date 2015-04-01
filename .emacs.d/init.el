;;; TODO:
;;;
;;;   - [ ] .ssh
;;;   - [ ] Setup erc
;;;   - [ ] Review docs file
;;;   - [ ] Autocomplete mode
;;;   - [ ] Remember C-x TAB, C-c C-f, C-c C-d
;;;   - [ ] https://github.com/skeeto/skewer-mode
;;;   - [ ] company-mode
;;;   - [ ] cleanup
;;;   - [ ] exec ssh-agent bash; ssh-add ~/.ssh/tradeo_rsa
;;;   - [ ] https://github.com/eschulte/epresent
;;;   - [ ] https://github.com/Malabarba/aggressive-indent-mode
;;;

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
