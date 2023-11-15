;; -*- lexical-binding: t; -*-
(use-package vertico
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)
         (minibuffer-setup . vertico-repeat-save)
         (minibuffer-setup . cursor-intangible-mode))
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("?"     . minibuffer-completion-help))
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
        vertico-count 12
        vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args))))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package savehist
  :straight nil
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" diamond-savefile-dir))
  (savehist-mode +1))

;; (use-package consult
;;   :ensure t
;;   :bind (
;;          ;; C-x bindings (ctl-x-map)
;;          ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
;;          ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
;;          ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
;;          ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
;;          ;; Custom M-# bindings for fast register access
;;          ("M-#" . consult-register-load)
;;          ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
;;          ("C-M-#" . consult-register)
;;          ;; Other custom bindings
;;          ("M-y" . consult-yank-pop)                ;; orig. yank-pop
;;          ("<help> a" . consult-apropos)            ;; orig. apropos-command
;;          ;; M-g bindings (goto-map)
;;          ("M-g e" . consult-compile-error)
;;          ("M-g f" . consult-flycheck)
;;          ("M-g g" . consult-goto-line)             ;; orig. goto-line
;;          ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
;;          ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
;;          ("M-g m" . consult-mark)
;;          ("M-g k" . consult-global-mark)
;;          ("M-g i" . consult-imenu)
;;          ("M-g I" . consult-imenu-multi)
;;          ;; M-s bindings (search-map)
;;          ("M-s f" . consult-find)
;;          ("M-s F" . consult-locate)
;;          ("M-s g" . consult-grep)
;;          ("M-s G" . consult-git-grep)
;;          ("M-s r" . consult-ripgrep)
;;          ("M-s l" . consult-line)
;;          ("M-s L" . consult-line-multi)
;;          ("M-s m" . consult-multi-occur)
;;          ("M-s k" . consult-keep-lines)
;;          ("M-s u" . consult-focus-lines)))
(use-package consult
  :defer t
  :config
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
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
   ([remap yank-pop] . consult-yank-pop)
   ("C-h D t" . consult-theme)
   ("M-g M-g" . consult-line)
   ("M-g M-m" . consult-line-multi)
   ("C-x M-f" . consult-fd)
   ("C-c ! j" . consult-flymake)
   ("C-c o"   . consult-outline)
   ("C-c M-r" . consult-ripgrep)
   :map org-mode-map
   ("C-c o" . consult-org-heading))
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
  :hook ((vertico-mode . marginalia-mode))
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(provide 'init-vertico)
