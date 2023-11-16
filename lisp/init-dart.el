;; -*- lexical-binding: t; -*-

(use-package dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . flutter-run-or-hot-reload)))

(use-package dart-server
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-c C-o" . dart-server-format))
  :config
  (setq dart-server-enable-analysis-server t))

(use-package flutter
  :after dart-mode
  :config
  (setq flutter-sdk-path "~/FlutterDev/flutter/"))

(provide 'init-dart)
