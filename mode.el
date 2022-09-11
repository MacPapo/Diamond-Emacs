;; Diamond Emacs for Mac
;;
;; MacPapo config started in 2022

(defun diamond-prog-mode-hook ()
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  (origami-mode 1)
  )
(add-hook 'prog-mode-hook 'diamond-prog-mode-hook)

(defun diamond-text-mode-hook ()
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  )
(add-hook 'text-mode-hook 'diamond-text-mode-hook)

(defun diamond-conf-mode-hook ()
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
  )
(add-hook 'conf-mode-hook 'diamond-conf-mode-hook)

(defun diamond-org-mode-hook ()
  (display-line-numbers-mode 0)
  (visual-line-mode 1)
  (olivetti-mode 1)
  (olivetti-set-width 115)
  )
(add-hook 'org-mode-hook 'diamond-org-mode-hook)

(defun diamond-eshell-mode-hook ()
  (display-line-numbers-mode 0)
  (hl-line-mode 0)
  )
(add-hook 'eshell-mode-hook 'diamond-eshell-mode-hook)

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-block-face t)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.ixx\\'" . c++-mode))
