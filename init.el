;; Diamond Emacs for Mac
;;
;; Visco01 config started in 2022

(require 'package)

(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
 (package-refresh-contents)
 (package-install 'use-package))

(eval-when-compile
 (require 'use-package))

(setq packages "~/.emacs.d/packages.el")
(load-file packages)

(setq customisation "~/.emacs.d/custom.el")
(load-file customisation)

(setq modalities "~/.emacs.d/mode.el")
(load-file modalities)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auto-complete web-mode winum vterm-toggle vertico use-package undo-tree solaire-mode show-font-mode saveplace-pdf-view pdf-tools org-modern org-auto-tangle olivetti no-littering magit helm-descbinds gcmh eshell-toggle dashboard auto-package-update all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
