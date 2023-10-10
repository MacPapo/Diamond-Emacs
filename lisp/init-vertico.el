;; -*- lexical-binding: t; -*-
(use-package vertico
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)
         (minibuffer-setup . vertico-repeat-save)
         (minibuffer-setup . cursor-intangible-mode))
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("?"   . minibuffer-completion-help))
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (setq enable-recursive-minibuffers t)
  :config
  (setq vertico-resize nil
        vertico-count 17
        vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args))))
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :straight nil
  :hook after-init)

(use-package consult
  :defer t
  :bind
  (([remap bookmark-jump] . consult-bookmark)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap Info-search] . consult-info)
   ([remap locate] . consult-locate)
   ([remap load-theme] . consult-theme)
   ([remap man] . consult-man)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop))
  :config
  (setq consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
  )

(use-package consult-dir
  :bind (([remap list-directory] . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package embark
  :defer t
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  :bind (
         ([remap describe-bindings] . embark-bindings)
         ("C-;" . embark-act)
         :map minibuffer-local-map
         ("C-;" . embark-act)
         ("C-c C-;" . embark-export)
         ("C-c C-l" . embark-collect)))

(use-package embark-consult
  :after (embark consult))

(use-package marginalia
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(provide 'init-vertico)
