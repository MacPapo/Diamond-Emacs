;; -*- lexical-binding: t; -*-

(use-package anzu
  :ensure t
  :init (global-anzu-mode +1)
  :bind (([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap query-replace]        . anzu-query-replace)
         ("C-c a r"                    . anzu-query-replace-at-cursor))
  :config
  (setq anzu-mode-lighter ""))

(with-eval-after-load 'isearch
  ;; DEL during isearch should edit the search string, not jump back to the previous result
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char))

(provide 'init-isearch)
