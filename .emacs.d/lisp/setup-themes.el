(require 'moe-theme)

(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

(defun zb ()
  (interactive)
  (load-theme 'zenburn t))

(defun aw ()
  (interactive)
  (load-theme 'adwaita t))

(defun db ()
  (interactive)
  (load-theme 'deeper-blue t))

(provide 'setup-themes)
