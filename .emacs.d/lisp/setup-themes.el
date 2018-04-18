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

(defun kd ()
  (interactive)
  (load-theme 'kaolin-dark t))

(defun st ()
  (interactive)
  (load-theme 'spacemacs-dark t))

(provide 'setup-themes)
