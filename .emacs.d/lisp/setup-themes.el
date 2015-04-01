(require 'moe-theme)

(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

(load-theme 'zenburn t)

(defun zb ()
  (interactive)
  (load-theme 'zenburn t))

(defun tw ()
  (interactive)
  (load-theme 'twilight t))

(defun aw ()
  (interactive)
  (load-theme 'adwaita t))

(defun db ()
  (interactive)
  (load-theme 'deeper-blue t))

(defun md ()
  (interactive)
  (load-theme 'moe-dark t))

(defun ml ()
  (interactive)
  (load-theme 'moe-light t))

(defun mk ()
  (interactive)
  (load-theme 'monokai t))

(provide 'setup-themes)
