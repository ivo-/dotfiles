(require 'prodigy)

(prodigy-define-service
  :name "UIAPP"
  :command "bundle exec foreman start"
  :cwd "~/Projects/uiapp/"
  :tags '(tradeo))

(provide 'setup-services)
