;; -*- lexical-binding: t; -*-

(use-package company
  :hook ((prog-mode . (lambda ()
                        (setq-local company-backends
                                    '((company-capf :with company-yasnippet)))
                        (company-mode)))
         (text-mode . (lambda ()
                        (setq-local company-backends
                                    '((company-dabbrev company-ispell :separate)
                                      company-files))
                        (company-mode))))
  :config
  (setq company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-lighter-base "©"
        company-tooltip-limit 10
        company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-require-match nil
        company-format-margin-function 'company-text-icons-margin
        company-tooltip-minimum 4
        company-text-face-extra-attributes '(:weight bold :slant italic)
        company-text-icons-add-background t
        company-tooltip-flip-when-above t
        company-show-quick-access 'left
        company-files-exclusions '(".git/" ".DS_Store")
        company-transformers '(delete-consecutive-dups
                           company-sort-by-occurrence)
        company-global-modes '(not erc-mode message-mode eshell-mode))
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)))

(provide 'init-company)
