;;; TODO:
;;;
;;;   - [ ] Setup erc
;;;   - [ ] Review docs file
;;;   - [ ] https://github.com/bodil/emacs.d/commit/10e7836cf51e27ba3e696f5dae7589e441a74ad6
;;;   - [ ] https://www.youtube.com/watch?v=XjKtkEMUYGc
;;;   - [ ] https://github.com/shime/livedown

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
