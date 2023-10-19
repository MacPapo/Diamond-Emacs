;; -*- lexical-binding: t; -*-

(use-package copilot
  :diminish copilot-mode
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook ((ruby-ts-mode . copilot-mode)
         (c++-ts-mode  . copilot-mode)
         (java-ts-mode . copilot-mode)
         (lisp-mode    . copilot-mode)
         (elisp-mode   . copilot-mode))
  :bind (("C-TAB"    . copilot-accept-completion)
         ("C-<tab>"  . copilot-accept-completion))
  :config
  (setq copilot-idle-delay 1))

(provide 'init-copilot)
