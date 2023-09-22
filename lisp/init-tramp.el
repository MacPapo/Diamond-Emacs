(use-package tramp
  :straight nil
  :config
  (setq remote-file-name-inhibit-cache nil
        tramp-verbose 6
        tramp-inline-compress-start-size 1000000
        tramp-default-method "ssh"))

(provide 'init-tramp)
