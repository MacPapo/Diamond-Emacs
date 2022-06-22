;; Diamond Emacs for Mac
;;
;; MacPapo config started in 2022

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

;; Packages list and config file
(setq packages "~/.emacs.d/packages.el")
(load-file packages)

;; Emacs editor configurations file
(setq customisation "~/.emacs.d/custom.el")
(load-file customisation)

;; Diamond Emacs Major and Minor mode customization file
(setq modalities "~/.emacs.d/mode.el")
(load-file modalities)
