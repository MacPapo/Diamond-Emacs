;; Diamond Emacs for Mac
;;
;; MacPapo config started in 2022

(require 'package)

(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-pin  "melpa"
      use-package-always-ensure t
      use-package-compute-statistics t
      use-package-verbose t
      use-package-always-defer t)

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
 '(auto-dark--dark-theme 'modus-vivendi)
 '(auto-dark--light-theme 'modus-operandi)
 '(auto-dark--polling-interval-seconds 1)
 '(package-selected-packages '(helm-tramp exec-path-from-shell all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
