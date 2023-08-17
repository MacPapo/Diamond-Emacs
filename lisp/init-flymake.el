;; -*- lexical-binding: t; -*-

(use-package flymake-flycheck
  :bind (:map flymake-mode-map
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! c" . flymake-start))
  :hook ((prog-mode     . flymake-mode)
         (text-mode     . flymake-mode)
         (flymake-mode  . (lambda ()
                            (setq-local flymake-diagnostic-functions
                                        (append flymake-diagnostic-functions
                                                (flymake-flycheck-all-chained-diagnostic-functions)))))
         (flycheck-mode . (lambda ()
                            (setq-default flycheck-disabled-checkers
                                          (append (default-value 'flycheck-disabled-checkers)
                                                  '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package))))))
  :config
  (setq flymake-proc-allowed-file-name-masks nil))

(provide 'init-flymake)
