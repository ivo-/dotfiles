;; =============================================================================
;; Defaults

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Regexp search by default.
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

;; Paragraph commands are by far more useful than the senctence commands.
(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "M-a") 'backward-paragraph)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "C-x O") '(lambda () (interactive) (other-window -1)))

(global-set-key (kbd "M-P") 'move-line-up)
(global-set-key (kbd "M-N") 'move-line-down)
(global-set-key (kbd "M-K") 'kill-whole-line)
(global-set-key (kbd "M-L") 'duplicate-current-line-or-region)

(global-set-key (kbd "C-S-o") 'kill-other-buffer-and-window)
(global-set-key (kbd "C-o") '(lambda () (interactive) (delete-other-windows)))

;; =============================================================================
;; M-j space

(global-set-key (kbd "M-j SPC") 'just-one-space)
(global-set-key (kbd "M-j M-SPC") 'delete-blank-lines)

(global-set-key (kbd "M-j q") 'generalized-shell-command)
(global-set-key (kbd "M-j M-q") 'shell-command-file)

(global-set-key (kbd "M-j e") 'eval-and-replace)
(global-set-key (kbd "M-j M-e") 'eval-and-replace)

(global-set-key (kbd "M-j l") 'join-line-or-lines-in-region)
(global-set-key (kbd "M-j M-l") 'join-line-or-lines-in-region)

(global-set-key (kbd "M-j f o") 'open-with)
(global-set-key (kbd "M-j f e") 'sudo-edit)
(global-set-key (kbd "M-j f d") 'delete-buffer-file)
(global-set-key (kbd "M-j f r") 'rename-buffer-file)
(global-set-key (kbd "M-j f c") 'copy-file-name-to-clipboard)

(global-set-key (kbd "M-j b n") 'create-buffer)
(global-set-key (kbd "M-j b i") 'cleanup-buffer)

(global-set-key (kbd "M-j RET") 'switch-to-previous-buffer)

(global-set-key (kbd "M-j `") 'hide-eshell)
(global-set-key (kbd "M-j 1") 'toggle-eshell-1)
(global-set-key (kbd "M-j 2") 'toggle-eshell-2)
(global-set-key (kbd "M-j 3") 'toggle-eshell-3)
(global-set-key (kbd "M-j 4") 'toggle-eshell-4)

(global-set-key (kbd "M-j M-`") 'hide-eshell)
(global-set-key (kbd "M-j M-1") 'toggle-eshell-1)
(global-set-key (kbd "M-j M-2") 'toggle-eshell-2)
(global-set-key (kbd "M-j M-3") 'toggle-eshell-3)
(global-set-key (kbd "M-j M-4") 'toggle-eshell-4)

(provide 'setup-keybindings)
