;; Remainders:
;;
;;   M-.          => jump-to-definition
;;   M-,          => jump-back
;;   C-<Return>   => expand region
;;   M-j RET      => switch to previous buffer
;;
;;  - [ ] Use spectrum https://github.com/bbatsov/emacs.d/commit/e5065681e68151732791c2fc5f41bbaf969af1e8
;;  - [ ] https://github.com/josharian/impl
;;  - [ ] https://github.com/jscheid/prettier.el
;;  - [ ] https://github.com/jcs-elpa/auto-rename-tag

(package-initialize)

;; Set up load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'setup-packages)
(require 'setup-defuns)
(require 'setup-keybindings)

;; Always keep init.el buffer.
(find-file "~/.emacs.d/init.el")

;; Don't need the *scratch* buffer.
(kill-buffer "*scratch*")
