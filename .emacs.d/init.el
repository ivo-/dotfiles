;; TODO:
;;
;;   - [ ] symlinks script for files in dotfiles
;;   - [ ] https://github.com/codesuki/add-node-modules-path
;;   - [ ] https://github.com/redguardtoo/js-comint
;;   - [ ] exwm with, arch/debian? https://technomancy.us/184
;;
;; Remainders:
;;
;;   M-j i        => imenu
;;   M-j c        => focus on defn
;;   M-j g g      => git grep
;;   C-<Return>   => expand region
;;
;; https://github.com/GoogleChrome/puppeteer ->
;;   login to your accounts and show some data: epay, gmail, pocket

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
