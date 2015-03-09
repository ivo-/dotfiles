;;; TODO:
;;;
;;;   - [ ] .ssh
;;;   - [ ] Setup erc
;;;   - [ ] Review docs file
;;;   - [ ] Reveal:   https://github.com/bodil/emacs.d/commit/10e7836cf51e27ba3e696f5dae7589e441a74ad6
;;;   - [ ] Diagrams: https://github.com/josteink/wsd-mode
;;;   - [ ] Autocomplete mode
;;;   - [ ] Remember C-x TAB, C-c C-f, C-c C-d
;;;   - [ ] https://github.com/skeeto/skewer-mode
;;;   - [ ] https://github.com/magnars/yesql-ghosts
;;;   - [ ] https://github.com/clojure-emacs/clj-refactor.el
;;;   - [ ] https://github.com/clojure-emacs/squiggly-clojure

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
