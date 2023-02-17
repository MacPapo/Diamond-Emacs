;;   -*- lexical-binding: t; -*-

(defconst *is-a-mac* (eq system-type 'darwin))

(require 'package)
(setq package-archives '(("elpa"  . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"	  . "https://orgmode.org/elpa/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (require 'use-package-ensure)
  (setq use-package-always-ensure t
        use-package-expand-minimally t
	package-check-signature nil))

(eval-when-compile
  (require 'use-package))

(use-package use-package-ensure-system-package
  :ensure t)

(use-package diminish
  :ensure t)

(use-package gcmh
  :diminish gcmh-mode
  :init
  (gcmh-mode 1))

(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(unless (window-system nil)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (menu-bar-mode 0)
  (setq inhibit-startup-message t)
  (if *is-a-mac*
      (progn
	(use-package exec-path-from-shell
	  :init
	  (setq mac-command-modifier 'meta)
	  (setq mac-option-modifier 'none)
	  :config
	  (exec-path-from-shell-initialize)))
    (setq x-super-keysym 'meta)))

(fringe-mode '(8 . 0))
(load-theme 'modus-vivendi t)
(global-auto-revert-mode 1)  ;; auto revert/refresh file when change detected
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq user-full-name "Jacopo Costantini")
(setq user-mail-address "891938@stud.unive.it")

(setq global-prettify-symbols-mode 1)

(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)

(setq create-lockfiles nil)
(setq select-enable-clipboard t)                   ; merge system's and emacs' clipboard

(electric-pair-mode)

;; (when (member "Iosevka" (font-family-list))
;;   (set-face-attribute 'default nil
;; 		      :family "Iosevka"
;; 		      :height 150
;; 		      :weight 'normal
;; 		      :width 'normal))

(setq display-time-default-load-average nil)
(setq use-short-answers t)
(setq new-line-add-newlines t)

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		xwidget-webkit-mode-hook
		neotree-mode-hook
		doc-view-mode-hook
		dired-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(delete-selection-mode 1)
(global-subword-mode 1)
(diminish 'subword-mode)
(diminish 'eldoc-mode)
(diminish 'hi-lock-mode)

(display-time-mode 1)
(display-battery-mode 1)
(size-indication-mode 1)

(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 600)
(setq recentf-exclude `(,(expand-file-name "straight/" user-emacs-directory)
                        ,(expand-file-name "eln-cache/" user-emacs-directory)
                        ,(expand-file-name "etc/" user-emacs-directory)
                        ,(expand-file-name "var/" user-emacs-directory)
                        ,(expand-file-name ".cache/" user-emacs-directory)
                        ,tramp-file-name-regexp
                        "/tmp" ".gz" ".tgz" ".xz" ".zip" "/ssh:"
			".png" ".jpg" "/\\.git/" ".gitignore" "\\.log" "COMMIT_EDITMSG"
                        ,(concat package-user-dir "/.*-autoloads\\.el\\'")))
(recentf-mode 1)

(winner-mode 1)
(setq winner-boring-buffers
      '("*Completions*"
        "*Compile-Log*"
        "*inferior-lisp*"
        "*helpful"
        "*lsp-help*"
        "*Fuzzy Completions*"
        "*Apropos*"
        "*Help*"
        "*cvs*"
        "*Buffer List*"
        "*Ibuffer*"
        "*esh command on file*"))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))


;;; NEWSTICKER - RSS FEED
(require 'newsticker)
(setq newsticker-url-list
      '(("EmacsWiki Recently Change"
         "http://www.emacswiki.org/cgi-bin/emacs?action=rss;showedit=1"
         nil nil nil)
        ("Planet Emacs Life"
         "https://planet.emacslife.com/atom.xml"
         nil nil nil)
        ("Haskell Planet"
         "http://planet.haskell.org/rss20.xml"
         nil nil nil)))

;; always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))
(setq backup-by-copying t)

(with-eval-after-load 'ispell
  (setq ispell-dictionary "italian"))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)
     (haskell . t)
     (latex . t)
     (python . t)
     (ruby . t)
     (shell . t)
     (sql . t)
     (C . t)
     (sqlite . t))))

(use-package org-pomodoro
  :after org)

(use-package org-bullets
  :after org)

(use-package olivetti
  :diminish olivetti-mode
  :after org
  :config
  (setq olivetti--visual-line-mode t)
  :init
  (defun olivetti-writer-mode ()
    (text-scale-set +2)
    (olivetti-mode 1)
    (flyspell-mode 1))
  :hook
  ((org-mode . olivetti-writer-mode)))

;;; Eglot
;; (setq eglot-autoshutdown t)
;; (add-hook 'c-ts-mode-hook 'eglot-ensure)
;; (add-hook 'c++-ts-mode-hook 'eglot-ensure)
;; (add-hook 'ruby-ts-mode-hook 'eglot-ensure)
;; (add-hook 'java-ts-mode-hook 'eglot-ensure)
;; (add-hook 'rust-mode-hook 'eglot-ensure)

;; setup files ending in .java to open in java-tree-sitter-mode
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(lua "https://github.com/Azganoth/tree-sitter-lua")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(r "https://github.com/r-lib/tree-sitter-r")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
	(typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package beacon
  :diminish beacon-mode
  :config
  (beacon-mode 1)
  :custom
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-window-scrolls nil)
  (beacon-blink-when-point-moves    nil)
  (beacon-blink-duration            .5)
  (beacon-blink-delay               .5)
  (beacon-size                      20))

(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-x C-g" . magit-status)))

(use-package forge
  :after magit)

;; (use-package magithub
;;   :after magit
;;   :config
;;   (magithub-feature-autoinject t)
;;   (setq magithub-clone-default-directory "~/Documents/Code"))

(use-package magit-todos
  :after magit)

(use-package ido-completing-read+
  :custom
  (ido-virtual-buffers t)
  (ido-use-faces t)
  (ido-enable-flex-matching t)
  (ido-use-virtual-buffers 'auto)
  (ido-default-buffer-method 'selected-window)
  (ido-auto-merge-work-directories-length -1)
  :init
  (ido-mode 1)
  (ido-everywhere t)
  (ido-ubiquitous-mode 1))

;; (use-package ido-vertical-mode
;;   :requires ido
;;   :custom
;;   (ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
;;   :init
;;   (ido-vertical-mode 1))

(use-package smex
  :requires ido
  :init
  (smex-initialize)
  :bind
  (("M-x" . smex)))

;; (use-package ido-at-point
;;   :requires ido
;;   :after ido
;;   :init
;;   (ido-at-point-mode))

;; (use-package treesit-auto
;;   :demand t
;;   :config
;;   (global-treesit-auto-mode))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package company
  :diminish company-mode
  :custom
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-selection-wrap-around t)
  :config

  (defun my-text-mode ()
    (setq-local company-backends
		'((company-dabbrev company-ispell :separate)
                  company-files)))

  (defun my-prog-mode ()
    (setq-local company-backends
		'((company-capf :with company-yasnippet)
                  (company-capf company-dabbrev-code))))

  (setq company-backends '((company-capf :with company-yasnippet)))
  :hook
  ((text-mode . my-text-mode))
  ((prog-mode . my-prog-mode))
  :init
  (global-company-mode))

(use-package lsp-mode
  :config
  (setq read-process-output-max (* 3 1024 1024)) ; 3MB
  :custom
  (lsp-idle-delay 0.100)
  (lsp-log-io nil)	  ; if set to true can cause a performance hit
  :init
  (require 'lsp-ido)
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (ruby-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (c++-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; (use-package corfu
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0.05)
;;   (corfu-quit-no-match t)
;;   (corfu-preview-current 'insert)
;;   (corfu-preselect 'prompt)

;;   (lsp-completion-provider :none) ; Use corfu instead for lsp completions

;;   ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
;;   ;; want to perform completion
;;   (tab-always-indent 'complete)
;;   (completion-cycle-threshold nil)    ; Always show candidates in menu
  
;;   :bind
;;   (:map corfu-map
;; 	("C-n"		.	'corfu-next)
;; 	("C-p"		.	'corfu-previous)
;; 	("<escape>"	.	'corfu-quit)
;; 	("<return>"	.	'corfu-insert)
;; 	("S-SPC"	.	'corfu-insert-separator))
;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   :config
;;   (add-hook 'eshell-mode-hook
;; 	    (lambda () (setq-local corfu-quit-at-boundary t
;; 				   corfu-quit-no-match t
;; 				   corfu-auto nil)
;; 	      (corfu-mode)))
;;   ;; Enable Corfu more generally for every minibuffer, as long as no other
;;   ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
;;   ;; completion UI. From
;;   ;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
;;   (defun corfu-enable-always-in-minibuffer ()
;;     "Enable Corfu in the minibuffer if Vertico/Mct are not active."
;;     (unless (or (bound-and-true-p mct--active) ; Useful if I ever use MCT
;;                 (bound-and-true-p vertico--input))
;;       (setq-local corfu-auto nil) ; Ensure auto completion is disabled
;;       (corfu-mode 1)))
;;   (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
;;   ;; Setup lsp to use corfu for lsp completion
;;   (defun kb/corfu-setup-lsp ()
;;     "Use orderless completion style with lsp-capf instead of the default lsp-passthrough."
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless))))

;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-use-icons t)
;;   (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
;;   (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
;;   (kind-icon-blend-frac 0.08)

;;   ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
;;   ;; directory that defaults to the `user-emacs-directory'. Here, I change that
;;   ;; directory to a location appropriate to `no-littering' conventions, a
;;   ;; package which moves directories of other packages to sane locations.
;;   ;; (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

;;   ;; Add hook to reset cache so the icon colors match my theme
;;   ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
;;   ;; the theme using my custom defined command for switching themes. If I don't
;;   ;; do this, then the backgound color will remain the same, meaning it will not
;;   ;; match the background color corresponding to the current theme. Important
;;   ;; since I have a light theme and dark theme I switch between. This has no
;;   ;; function unless you use something similar
;;   (add-hook 'kb/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

;; (use-package cape
;;   :hook ((emacs-lisp-mode .  kb/cape-capf-setup-elisp)
;;          (lsp-completion-mode . kb/cape-capf-setup-lsp)
;;          (org-mode . kb/cape-capf-setup-org)
;;          (eshell-mode . kb/cape-capf-setup-eshell)
;;          (git-commit-mode . kb/cape-capf-setup-git-commit)
;;          (LaTeX-mode . kb/cape-capf-setup-latex)
;;          (sh-mode . kb/cape-capf-setup-sh)
;;          )
;;   ;; :general (:prefix "M-c"               ; Particular completion function
;;   ;;           "p" 'completion-at-point
;;   ;;           "t" 'complete-tag           ; etags
;;   ;;           "d" 'cape-dabbrev           ; or dabbrev-completion
;;   ;;           "f" 'cape-file
;;   ;;           "k" 'cape-keyword
;;   ;;           "s" 'cape-symbol
;;   ;;           "a" 'cape-abbrev
;;   ;;           "i" 'cape-ispell
;;   ;;           "l" 'cape-line
;;   ;;           "w" 'cape-dict
;;   ;;           "\\"' cape-tex
;;   ;;           "_" 'cape-tex
;;   ;;           "^" 'cape-tex
;;   ;;           "&" 'cape-sgml
;;   ;;           "r" 'cape-rfc1345
;;   ;;           )
;;   :custom
;;   (cape-dabbrev-min-length 3)
;;   :init
;;   ;; Elisp
;;   (defun kb/cape-capf-ignore-keywords-elisp (cand)
;;     "Ignore keywords with forms that begin with \":\" (e.g.
;; :history)."
;;     (or (not (keywordp cand))
;;         (eq (char-after (car completion-in-region--data)) ?:)))
;;   (defun kb/cape-capf-setup-elisp ()
;;     "Replace the default `elisp-completion-at-point'
;; completion-at-point-function. Doing it this way will prevent
;; disrupting the addition of other capfs (e.g. merely setting the
;; variable entirely, or adding to list).

;; Additionally, add `cape-file' as early as possible to the list."
;;     (setf (elt (cl-member 'elisp-completion-at-point completion-at-point-functions) 0)
;;           #'elisp-completion-at-point)
;;     (add-to-list 'completion-at-point-functions #'cape-symbol)
;;     ;; I prefer this being early/first in the list
;;     (add-to-list 'completion-at-point-functions #'cape-file)
;;     (require 'company-yasnippet)
;;     (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet)))

;;   ;; LSP
;;   (defun kb/cape-capf-setup-lsp ()
;;     "Replace the default `lsp-completion-at-point' with its
;; `cape-capf-buster' version. Also add `cape-file' and
;; `company-yasnippet' backends."
;;     (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
;;           (cape-capf-buster #'lsp-completion-at-point))
;;     ;; TODO 2022-02-28: Maybe use `cape-wrap-predicate' to have candidates
;;     ;; listed when I want?
;;     (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
;;     (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

;;   ;; Org
;;   (defun kb/cape-capf-setup-org ()
;;     ;; (require 'org-roam)
;;     ;; (if (org-roam-file-p)
;;     ;;     (org-roam--register-completion-functions-h)
;;       (let (result)
;;         (dolist (element (list
;;                           (cape-super-capf #'cape-ispell #'cape-dabbrev)
;;                           (cape-company-to-capf #'company-yasnippet))
;;                          result)
;;           (add-to-list 'completion-at-point-functions element)))
;;       )

;;   ;; Eshell
;;   (defun kb/cape-capf-setup-eshell ()
;;     (let ((result))
;;       (dolist (element '(pcomplete-completions-at-point cape-file) result)
;;         (add-to-list 'completion-at-point-functions element))
;;       ))

;;   ;; Git-commit
;;   (defun kb/cape-capf-setup-git-commit ()
;;     (general-define-key
;;      :keymaps 'local
;;      :states 'insert
;;      "<tab>" 'completion-at-point)      ; Keybinding for `completion-at-point'
;;     (let ((result))
;;       (dolist (element '(cape-dabbrev cape-symbol) result)
;;         (add-to-list 'completion-at-point-functions element))))

;;   ;; LaTeX
;;   (defun kb/cape-capf-setup-latex ()
;;     (require 'company-auctex)
;;     (let ((result))
;;       (dolist (element (list
;;                         ;; First add `company-yasnippet'
;;                         (cape-company-to-capf #'company-yasnippet)
;;                         ;; Then add `cape-tex'
;;                         #'cape-tex
;;                         ;; Then add `company-auctex' in the order it adds its
;;                         ;; backends.
;;                         (cape-company-to-capf #'company-auctex-bibs)
;;                         (cape-company-to-capf #'company-auctex-labels)
;;                         (cape-company-to-capf
;;                          (apply-partially #'company--multi-backend-adapter
;;                                           '(company-auctex-macros company-auctex-symbols company-auctex-environments))))
;;                        result)
;;         (add-to-list 'completion-at-point-functions element))))


;;   ;; Sh
;;   (defun kb/cape-capf-setup-sh ()
;;     (require 'company-shell)
;;     (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-shell)))
;;   :config
;;   ;; For pcomplete. For now these two advices are strongly recommended to
;;   ;; achieve a sane Eshell experience. See
;;   ;; https://github.com/minad/corfu#completing-with-corfu-in-the-shell-or-eshell

;;   ;; Silence the pcomplete capf, no errors or messages!
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
;;   ;; Ensure that pcomplete does not write to the buffer and behaves as a pure
;;   ;; `completion-at-point-function'.
;;   (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))


(use-package which-key
  :diminish which-key-mode
  :init
  (which-key-mode))

(use-package winum
  :custom
  (winum-auto-setup-mode-line t)
  :hook (after-init . winum-mode)
  :bind (("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)
         ("M-7" . winum-select-window-7)
         ("M-8" . winum-select-window-8)
         ("M-9" . winum-select-window-9)))

(use-package buffer-move
  :config
  (setq buffer-move-behavior 'move)
  :bind
  (("M-<up>"    . buf-move-up)
   ("M-<down>"  . buf-move-down)
   ("M-<left>"  . buf-move-left)
   ("M-<right>" . buf-move-right)))

(use-package ctrlf
  :init
  (ctrlf-mode +1))

(use-package dimmer
  :init
  (dimmer-configure-which-key)
  (dimmer-mode t))

(use-package indent-guide
  :diminish indent-guide-mode
  :custom
  (indent-guide-delay 0.2)
  :init
  (indent-guide-global-mode))

(use-package highlight-thing
  :diminish highlight-thing-mode
  :custom
  (highlight-thing-exclude-thing-under-point t)
  (highlight-thing-ignore-list '("False" "True"))
  (highlight-thing-limit-to-region-in-large-buffers-p nil)
  (highlight-thing-narrow-region-lines 15)
  (highlight-thing-large-buffer-limit 5000)
  :init
  (global-highlight-thing-mode))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-S-c C-S-a" . mc/edit-beginnings-of-lines)
   ("C-S-c C-S-e" . mc/edit-ends-of-lines)
   ("C-S-c C-S-w" . mc/mark-all-words-like-this)
   ("C-S-c C-S-q" . mc/mark-all-words-like-this-in-defun)
   ("C->"         . mc/mark-next-like-this)
   ("C-<"         . mc/mark-previous-like-this)
   ("C-c C-<"     . mc/mark-all-like-this)))

(use-package phi-search
  :after multiple-cursors
  :bind
  (("C-S-c C-s" . phi-search)
   ("C-S-c C-r" . phi-search-backward)))

;; (use-package smartparens
;;   :hook
;;   ((prog-mode . smartparens-mode)))

(use-package move-dup
  :bind
  (("M-p"   . move-dup-move-lines-up)
   ("M-n"   . move-dup-move-lines-down)
   ("C-M-p" . move-dup-duplicate-up)
   ("C-M-n" . move-dup-duplicate-down)))

(use-package projectile
  :custom
  (projectile-mode-line-prefix " Proj")
  (projectile-generic-command "rg --files --hidden")
  (projectile-indexing-method 'alien)
  (projectile-enable-caching t)
  (projectile-completion-systemp 'ido)
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package projectile-rails
  :after projectile
  :init
  (projectile-rails-global-mode)
  :bind (:map projectile-rails-mode-map
              ("C-c r" . projectile-rails-command-map)))

(use-package projectile-ripgrep
  :after projectile)

(use-package ag
  :after projectile)

;;; HTML + ERB
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'"		.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'"	.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'"	.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'"	.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'"		.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'"	.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'"		.	web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'"	.	web-mode)))

(use-package js2-mode
  :hook
  ((javascript-mode-hook . js2-mode)))

(use-package tide
  :after (flycheck)
  :hook ((typescript-ts-mode	.	tide-setup)
         (tsx-ts-mode		.	tide-setup)
         (typescript-ts-mode	.	tide-hl-identifier-mode)
         (before-save		.	tide-format-before-save)))

;;; RUBY - TODO
(use-package bundler)
(use-package yari)

;; (use-package robe
;;   :defer 7)

(use-package rspec-mode)

(use-package ruby-electric
  :diminish ruby-electric-mode
  :hook
  ((ruby-mode . ruby-electric-mode)))

;;; LISP - TODO
(use-package lispy
  :diminish lispy-mode
  :hook
  ((emacs-lisp-mode . lispy-mode)
   (lisp-mode       . lispy-mode)))

(use-package sly)
(use-package suggest)

;;; HASKELL - TODO
(use-package haskell-mode)

;;; SWIFT - TODO
(use-package swift-mode)

;;; R - TODO
(use-package ess)

;;; MARKDOWN - TODO
(use-package markdown-mode)
(use-package grip-mode
  :after markdown-mode)

;;; LATEX - TODO
;; (use-package auctex)
;; (use-package latex-preview-pane)

;;; DOCKER - TODO
(use-package docker)

(use-package csv-mode)

(use-package neotree
  :custom
  (neo-smart-open t)
  (projectile-switch-project-action 'neotree-projectile-action)
  :bind
  (("C-c n" . neotree-toggle)))
