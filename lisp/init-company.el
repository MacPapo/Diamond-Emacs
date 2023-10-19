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
        company-tooltip-limit 14
        company-idle-delay 0.05 ;; correggimi
        company-minimum-prefix-length 2
        company-require-match 'never
        ;; company-format-margin-function 'company-text-icons-margin
        company-tooltip-minimum 4
        ;; company-text-face-extra-attributes '(:weight bold :slant italic)
        ;; company-text-icons-add-background t
        company-auto-commit nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-tooltip-flip-when-above t
        company-show-quick-access 'left
        company-backends '(company-capf)
        company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend)
        company-files-exclusions '(".git/" ".DS_Store")
        company-transformers '(delete-consecutive-dups
                               company-sort-by-occurrence)
        company-global-modes '(not erc-mode message-mode help-mode eshell-mode))
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection)))

(use-package company-box
  :diminish company-box-mode
  :hook company-mode
  :config
  (setq company-box-show-single-candidate t
        company-box-backends-colors nil
        company-box-tooltip-limit 50))

(provide 'init-company)
