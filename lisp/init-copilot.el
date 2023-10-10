(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :defer 15
  :bind (("C-TAB"    . copilot-accept-completion)
         ("C-<tab>"  . copilot-accept-completion))
  :config
  (setq copilot-idle-delay 1))

(provide 'init-copilot)
