;; -*- lexical-binding: t; -*-

;; (use-package ido
;;   :config
;;   (setq ido-enable-prefix nil
;;         ido-case-fold t
;;         ido-auto-merge-work-directories-length -1
;;         ido-create-new-buffer 'always
;;         ido-use-filename-at-point nil
;;         ido-use-virtual-buffers 'auto
;;         ido-virtual-buffers t
;;         ido-file-extensions-order '(".org" ".el" ".c" ".cpp" ".rb" ".java" ".lisp" ".md" ".dart")
;;         ido-use-faces t
;;         ido-max-prospects 10)
;;   (add-to-list 'ido-ignore-directories "target")
;;   (add-to-list 'ido-ignore-directories "node_modules")
;;   (ido-mode 1)
;;   (ido-everywhere 1))

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

;; (use-package smex
;;   :bind (("M-x" . smex)
;;          ("M-X" . smex-major-mode-commands))
;;   :config
;;   (smex-initialize))

(use-package amx
  :init (amx-mode))

;; ;; (use-package icomplete
;;   :straight nil
;;   :config
;;   (icomplete-mode 1))

(provide 'init-ido)
