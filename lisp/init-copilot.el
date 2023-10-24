;; -*- lexical-binding: t; -*-

(use-package copilot
  :diminish copilot-mode
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook ((ruby-mode . copilot-mode)
         (c++-mode  . copilot-mode)
         (java-mode . copilot-mode)
         (lisp-mode    . copilot-mode)
         (elisp-mode   . copilot-mode))
  :bind (("C-TAB"    . copilot-accept-completion)
         ("C-<tab>"  . copilot-accept-completion))
  :config
  (setq copilot-idle-delay 1))

(provide 'init-copilot)
