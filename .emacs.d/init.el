;;;
;; Remainders:
;;
;;   M-.          => jump-to-definition
;;   M-,          => jump-back
;;   C-<Return>   => expand region
;;   M-j RET      => switch to previous buffer

(package-initialize)

;; Set up load path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'setup-packages)
(require 'setup-defuns)
(require 'setup-keybindings)

;; Always keep init.el buffer.
(find-file "~/.emacs.d/init.el")

;; Don't need the *scratch* buffer.
(kill-buffer "*scratch*")
