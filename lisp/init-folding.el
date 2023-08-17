;; -*- lexical-binding: t; -*-

(use-package origami
  :bind (("C-c f" . origami-recursively-toggle-node)
         ("C-c F" . origami-toggle-all-nodes))
  :hook prog-mode)

(provide 'init-folding)
