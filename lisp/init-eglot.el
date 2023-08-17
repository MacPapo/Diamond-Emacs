;; -*- lexical-binding: t; -*-

(require 'eglot)

(setq read-process-output-max (* 3 1024 1024))

;; Default of 800 was too low.
;; Avoid Lisp nesting exceeding in swift-mode.
(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

(provide 'init-eglot)
