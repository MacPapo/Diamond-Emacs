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
