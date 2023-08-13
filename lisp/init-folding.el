;; -*- lexical-binding: t; -*-

(use-package origami
  :ensure t
  :bind (("C-c f" . origami-recursively-toggle-node)
         ("C-c F" . origami-toggle-all-nodes))
  :hook (prog-mode . origami-mode))

(provide 'init-folding)
