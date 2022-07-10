;;; -*- lexical-binding: t -*-
;; Diamond Emacs for Mac
;;
;; MacPapo config started in 2022

(require 'package)
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; 配置 use-package 集成 straight。
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-recipes-gnu-elpa-use-mirror t)
(setq straight-check-for-modifications '(check-on-save find-when-checking watch-files))
(setq straight-host-usernames '((github . "opsnull")))

;; 安装 straight.el。
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 安装 use-package。
(straight-use-package 'use-package)
(setq use-package-verbose t)
(setq use-package-compute-statistics t)
(setq use-package-always-defer t)

;; 为 use-package 添加 :ensure-system-package 指令。
(use-package use-package-ensure-system-package)

(setq packages "~/.emacs.d/packages.el")
(load-file packages)

(setq customisation "~/.emacs.d/custom.el")
(load-file customisation)

(setq modalities "~/.emacs.d/mode.el")
(load-file modalities)
