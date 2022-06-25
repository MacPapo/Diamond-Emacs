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
