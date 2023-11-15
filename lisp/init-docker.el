;; -*- lexical-binding: t; -*-

(use-package docker
  :defer t)

(use-package docker-compose-mode
  :defer t)

(use-package dockerfile-mode
  :defer t
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))

(provide 'init-docker)
