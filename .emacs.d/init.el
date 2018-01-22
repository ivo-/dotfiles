;; TODO:
;;
;;   - [ ] review docs filex
;;   - [ ] cleanup
;;
;; Remainders:
;;
;;   M-j i => imenu
;;   C-;   => expand region
;;
;; Packages:
;;
;;   - [ ] flow, prettier
;;   - [ ] company-mode, yasnippet
;;   - [ ] https://github.com/abo-abo/swiper

(package-initialize)

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
