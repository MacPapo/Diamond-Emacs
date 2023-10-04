(use-package docker)

(use-package docker-compose-mode)

(use-package dockerfile-mode
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))

(provide 'init-docker)
