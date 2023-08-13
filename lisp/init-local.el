;; init-local.el --- User preferences -*- lexical-binding: t -*-

(when (member "IBM Plex Mono" (font-family-list))
  (custom-set-faces
   '(default ((t (:family "IBM Plex Mono" :height 125))))))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(unless (package-installed-p 'quelpa-use-package)
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git")))

(require 'quelpa-use-package)

(use-package copilot
  :ensure t
  :defer 5
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("S-<tab>" . 'copilot-accept-completion)
              ("S-TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(provide 'init-local)
