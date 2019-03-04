;; TODO:
;;
;;   - [ ] flow-js2-mode
;;   - [ ] symlinks script for files in dotfiles
;;   - [ ] exwm with, arch/debian? https://technomancy.us/184
;;   - [ ] https://github.com/rlister/org-present
;;   - [ ] https://github.com/kunalb/poet
;;   - [ ] https://github.com/codesuki/add-node-modules-path
;;   - [ ] enable eslint
;;   - [ ] https://github.com/bling/fzf.el
;;   - [ ] https://github.com/anmonteiro/dotfiles/blob/master/.emacs.d/customizations/setup-js.el
;;
;;
;; Remainders:
;;
;;   M-j i        => imenu
;;   M-j c        => focus on defn
;;   M-j g g      => git grep
;;   M-.          => jump-to-definition
;;   M-,          => jump-back
;;   C-<Return>   => expand region
;;   M-j RET      => switch to previous buffer

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
