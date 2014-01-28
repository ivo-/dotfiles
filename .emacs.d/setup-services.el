(require 'prodigy)

(prodigy-define-service
  :name "Redis"
  :command "redis-server"
  :cwd "~/"
  :tags '(despark))

(prodigy-define-service
  :name "Sidekiq"
  :command "sidekiq"
  :cwd "~/Projects/Reborn"
  :args '("-C" "./config/sidekiq.yml")
  :tags '(despark))

(prodigy-define-service
  :name "Reborn"
  :command "bundle exec rails server"
  :cwd "~/Projects/Reborn"
  :tags '(despark))

(prodigy-define-service
  :name "Spotify"
  :command "spotify"
  :cwd "~/"
  :tags '(despark))

(provide 'setup-services)
