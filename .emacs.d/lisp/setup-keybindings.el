;; NOTE: Comment for non-UK keyboards
(global-set-key (kbd "s-3") '(lambda () (interactive) (insert "#")))

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "M-e") 'forward-paragraph)
(global-set-key (kbd "M-a") 'backward-paragraph)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-x O") '(lambda () (interactive) (other-window -1)))

(global-set-key (kbd "M-P") 'move-line-up)
(global-set-key (kbd "M-N") 'move-line-down)

(global-set-key (kbd "C-o") '(lambda () (interactive) (delete-other-windows)))

(global-set-key (kbd "M-j SPC") 'just-one-space)
(global-set-key (kbd "M-j M-SPC") 'delete-blank-lines)

(global-set-key (kbd "M-j q") 'generalized-shell-command)
(global-set-key (kbd "M-j M-q") 'shell-command-file)
(global-set-key (kbd "M-j C-q") 'run-shell-command-from-selection)

(global-set-key (kbd "M-j f c") 'copy-file-name-to-clipboard)
(global-set-key (kbd "M-j b n") 'create-buffer)

(global-set-key (kbd "M-j `") 'hide-eshell)
(global-set-key (kbd "M-j 1") 'toggle-eshell-1)
(global-set-key (kbd "M-j 2") 'toggle-eshell-2)
(global-set-key (kbd "M-j 3") 'toggle-eshell-3)
(global-set-key (kbd "M-j 4") 'toggle-eshell-4)
(global-set-key (kbd "M-j 0") 'toggle-eshell-show-all)

(global-set-key (kbd "M-j M-`") 'hide-eshell)
(global-set-key (kbd "M-j M-1") 'toggle-eshell-1)
(global-set-key (kbd "M-j M-2") 'toggle-eshell-2)
(global-set-key (kbd "M-j M-3") 'toggle-eshell-3)
(global-set-key (kbd "M-j M-4") 'toggle-eshell-4)
(global-set-key (kbd "M-j M-0") 'toggle-eshell-show-all)

(provide 'setup-keybindings)
