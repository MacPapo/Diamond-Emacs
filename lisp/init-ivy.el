;; -*- lexical-binding: t; -*-

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view))
  :config
  (setq ivy-use-virtual-buffers        t
        ivy-count-format               "(%d/%d) "
        ivy-display-style              'fancy
        ivy-initial-inputs-alist       nil
        enable-recursive-minibuffers   t
        ivy-wrap t)
  (ivy-mode 1))

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-modify-column
   'ivy-switch-buffer
   'ivy-rich-switch-buffer-major-mode
   '(:width 20 :face error))
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("M-y"     . counsel-yank-pop)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-find-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("<f2> j"  . counsel-set-variable)

         ;; Ivy-based interface to shell and system tools
         ("C-c c"   . counsel-compile)
         ("C-c g"   . counsel-git)
         ("C-c j"   . counsel-git-grep)
         ("C-c L"   . counsel-git-log)
         ("C-c k"   . counsel-rg)
         ("C-x l"   . counsel-locate)
         ("C-c J"   . counsel-file-jump)

         ;; Ivy-resume and other commands
         ("C-c C-r" . ivy-resume)
         ("C-c b"   . counsel-bookmark)
         ("C-c D"   . counsel-descbinds)
         ("C-c o"   . counsel-outline)
         ("C-c t"   . counsel-load-theme)
         ("C-c F"   . counsel-org-file)))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(provide 'init-ivy)
