;; Diamond Emacs for Mac
;;
;; MacPapo config started in 2022

(defun diamond-prog-mode-hook ()
  (display-line-numbers-mode 1)
  (hl-line-mode 1)
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

;; (defun diamond-org-mode-hook ()
;;   (display-line-numbers-mode 0)
;;   (org-modern-mode 1)
;;   (visual-line-mode 1)
;;   (olivetti-mode 1)
;;   (olivetti-set-width 115)
;;   )
;; (add-hook 'org-mode-hook 'diamond-org-mode-hook)

(defun diamond-eshell-mode-hook ()
  (display-line-numbers-mode 0)
  (hl-line-mode 0)
  )
(add-hook 'eshell-mode-hook 'diamond-eshell-mode-hook)

(add-hook 'helm-tramp-pre-command-hook '(lambda ()
                                          (projectile-mode 0)))
(add-hook 'helm-tramp-quit-hook '(lambda ()
                                   (projectile-mode 1)))

(defun adjust-languages-indent (n)
  (setq-local c-basic-offset n)

  (setq-local javascript-indent-level n)
  (setq-local js-indent-level n)
  (setq-local js2-basic-offset n)

  (setq-local web-mode-attr-indent-offset n)
  (setq-local web-mode-attr-value-indent-offset n)
  (setq-local web-mode-code-indent-offset n)
  (setq-local web-mode-css-indent-offset n)
  (setq-local web-mode-markup-indent-offset n)
  (setq-local web-mode-sql-indent-offset n)

  (setq-local css-indent-offset n))

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'sh-mode-hook
               'scss-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (setq indent-tabs-mode nil)
                     (adjust-languages-indent 4)
                     )))

(dolist (hook (list
               'go-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     ;; go-mode 默认启用 tabs, 否则 gofmt 和 lsp-bridge 的 auto import 会异常。
                     (setq indent-tabs-mode t)
                     (setq-default c-basic-offset 4)
                     (setq-local indent-tabs-mode t)
                     )))

(dolist (hook (list
               'web-mode-hook
               'js-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (setq indent-tabs-mode nil)
                     (adjust-languages-indent 2)
                     )))

;; 使用 fundamental-mode 打开大文件。
(defun my/large-file-hook ()
  (when (and (> (buffer-size) (* 1024 2))
             (or (string-equal (file-name-extension (buffer-file-name)) "json")
                 (string-equal (file-name-extension (buffer-file-name)) "yaml")
                 (string-equal (file-name-extension (buffer-file-name)) "yml")
                 (string-equal (file-name-extension (buffer-file-name)) "log")))
    (fundamental-mode)
    (setq buffer-read-only t)
    (font-lock-mode -1)
    (rainbow-delimiters-mode -1)))
(add-hook 'find-file-hook 'my/large-file-hook)
;; 默认直接用 fundamental-mode 打开 json 和 log 文件, 确保其它 major-mode 不会先执行。
(add-to-list 'auto-mode-alist '("\\.log?\\'" . fundamental-mode))
(add-to-list 'auto-mode-alist '("\\.json?\\'" . fundamental-mode))
