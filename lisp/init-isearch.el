;; -*- lexical-binding: t; -*-

(use-package anzu
  :bind (([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap query-replace]        . anzu-query-replace)
         ("C-c a r"                    . anzu-query-replace-at-cursor)
         :map isearch-mode-map
         ([remap isearch-delete-char]  . isearch-del-char))
  :config
  (setq anzu-mode-lighter "")
  (global-anzu-mode +1))

(provide 'init-isearch)
