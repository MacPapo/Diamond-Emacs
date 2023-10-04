;; -*- lexical-binding: t; -*-

(use-package hydra)

(use-package ivy
  :init (ivy-mode 1)
  :bind (:map ivy-minibuffer-map
              ("RET" . ivy-alt-done))
  :custom
  (ivy-use-virtual-buffers 'recentf))

(use-package ivy-hydra
  :after ivy hydra)

(use-package counsel
  :init (counsel-mode 1))

(use-package counsel-projectile
  :after counsel projectile
  :init (counsel-projectile-mode))

(use-package posframe
  :custom
  (posframe-arghandler #'focks/posframe-fallback))

(use-package swiper
  :bind
  ("C-s" . swiper)
  ("C-r" . swiper))

;; (use-package ivy
;;   :diminish ivy-mode
;;   :bind (("C-x b" . ivy-switch-buffer)
;;          ("C-c v" . ivy-push-view)
;;          ("C-c V" . ivy-pop-view))
;;   :config
;;   (setq ivy-use-virtual-buffers        t
;;         ivy-count-format               "(%d/%d) "
;;         ivy-display-style              'fancy
;;         ivy-initial-inputs-alist       nil
;;         enable-recursive-minibuffers   t
;;         ivy-wrap t)
;;   (ivy-mode 1))

;; (use-package ivy-rich
;;   :config
;;   (ivy-rich-modify-column
;;    'ivy-switch-buffer
;;    'ivy-rich-switch-buffer-major-mode
;;    '(:width 20 :face error))
;;   (ivy-rich-mode 1))

;; (use-package counsel
;;   :bind (("M-x"     . counsel-M-x)
;;          ("C-x C-f" . counsel-find-file)
;;          ("M-y"     . counsel-yank-pop)
;;          ("<f1> f"  . counsel-describe-function)
;;          ("<f1> v"  . counsel-describe-variable)
;;          ("<f1> l"  . counsel-find-library)
;;          ("<f2> i"  . counsel-info-lookup-symbol)
;;          ("<f2> u"  . counsel-unicode-char)
;;          ("<f2> j"  . counsel-set-variable)

;;          ;; Ivy-based interface to shell and system tools
;;          ("C-c c"   . counsel-compile)
;;          ("C-c g"   . counsel-git)
;;          ("C-c j"   . counsel-git-grep)
;;          ("C-c L"   . counsel-git-log)
;;          ("C-c k"   . counsel-rg)
;;          ("C-x l"   . counsel-locate)
;;          ("C-c J"   . counsel-file-jump)

;;          ;; Ivy-resume and other commands
;;          ("C-c C-r" . ivy-resume)
;;          ("C-c b"   . counsel-bookmark)
;;          ("C-c D"   . counsel-descbinds)
;;          ("C-c o"   . counsel-outline)
;;          ("C-c t"   . counsel-load-theme)
;;          ("C-c F"   . counsel-org-file)))
(provide 'init-ivy)
