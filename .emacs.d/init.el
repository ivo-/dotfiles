;; TODO:
;;
;; Remainders:
;;
;;   M-j i        => imenu
;;   M-j c        => focus on defn
;;   M-j g g      => git grep
;;   C-<Return>   => expand region

(package-initialize)

;; Set up load path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'setup-settings)
(require 'setup-packages)
(require 'setup-keybindings)

(require 'setup-themes)
(require 'setup-defuns)

;; Always keep init.el buffer.
(find-file "~/.emacs.d/init.el")

;; Don't need the *scratch* buffer.
(kill-buffer "*scratch*")
