;; Diamond Emacs for Mac
;;
;; MacPapo config started in 2022

(use-package auto-package-update
  :commands update-packages
  :custom
  (auto-package-update-delete-old-versions t)
  :config
  (auto-package-update-at-time "03:00")
  )

(use-package recentf
  :straight (:type built-in)
  :config
  ;; 不清理 recentf tramp buffers.
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 600)
  (recentf-mode +1)
  (setq recentf-exclude `(,(expand-file-name "straight/" user-emacs-directory)
                          ,(expand-file-name "eln-cache/" user-emacs-directory)
                          ,(expand-file-name "etc/" user-emacs-directory)
                          ,(expand-file-name "var/" user-emacs-directory)
                          ,(expand-file-name ".cache/" user-emacs-directory)
                          ,tramp-file-name-regexp
                          "/tmp" ".gz" ".tgz" ".xz" ".zip" "/ssh:" ".png" ".jpg" "/\\.git/" ".gitignore" "\\.log" "COMMIT_EDITMSG"
                          ,(concat package-user-dir "/.*-autoloads\\.el\\'"))))

(use-package exec-path-from-shell
  :demand
  :custom
  ;; 去掉 -i 参数, 加快启动速度。
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package gcmh
  :demand
  :init
  ;; 在 minibuffer 显示 GC 信息。
  ;;(setq garbage-collection-messages t)
  ;;(setq gcmh-verbose t)
  (setq gcmh-idle-delay 5)
  (setq gcmh-high-cons-threshold (* 64 1024 1024))
  (gcmh-mode 1)
  (gcmh-set-high-threshold))

;; minibuffer 历史。
(use-package savehist
  :straight (:type built-in)
  :hook (after-init . savehist-mode)
  :config
  (setq history-length 200)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-autosave-interval 200)
  (setq savehist-additional-variables
        '(mark-ring
          global-mark-ring
          search-ring
          regexp-search-ring
          extended-command-history)))

(use-package org-auto-tangle
  :straight t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(use-package no-littering
  :straight t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (setq custom-file (no-littering-expand-etc-file-name "packages.el"))
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory)))))

(use-package projectile
:init
(projectile-mode +1)
:config
(setq projectile-indexing-method 'alien)
(setq projectile-file-exists-remote-cache-expire nil)
(setq projectile-completion-system 'helm)
:bind (:map projectile-mode-map
            ("C-c p" . projectile-command-map)))

;; all-the-icons 和 fire-code-mode 只能在 GUI 模式下使用。
(when (display-graphic-p)
  (use-package all-the-icons :demand))

(use-package all-the-icons-dired
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package dashboard
  :demand t
  :init
  (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  (progn
    (setq dashboard-items '((recents . 8)
                            (projects . 5)
                            (bookmarks . 5)))
    (setq dashboard-center-content t)
    (setq dashboard-set-init-info t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-startup-banner "~/.emacs.d/etc/banner/snake.gif")
    (setq dashboard-banner-logo-title "[ D I A M O N D   E M A C S ]")
    (setq dashboard-set-navigator t)

    ;; Format: "(icon title help action face prefix suffix)"
    (setq dashboard-navigator-buttons
          `(;; line1
            ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
              "Diamond Git"
              "Diamond homepage"
              (lambda (&rest _) (browse-url "https://github.com/MacPapo/Diamond-Emacs")))
             (,(all-the-icons-material "update" :height 1.1 :v-adjust -0.2)
              "Update"
              "Update Packages"
              (lambda (&rest _) (auto-package-update-now)))
             (,(all-the-icons-material "flag" :height 1.1 :v-adjust -0.2)
              "Report bug"
              "Report a bug"
              (lambda (&rest _) (browse-url "https://github.com/MacPapo/Diamond-Emacs/issues/new")))
             )
            ))

    (setq dashboard-footer-messages '("Vim! Ahahah, it’s only one of the many Emacs modes!  CIT. Master of the Masters"))
    (setq dashboard-footer-icon (all-the-icons-octicon "flame"
                                                       :height 1.1
                                                       :v-adjust -0.02
                                                       :face 'font-lock-keyword-face))
    )
  :config
  (
   dashboard-setup-startup-hook)
  )

(use-package doom-modeline
:straight t
:init
(setq doom-modeline-height 10
      doom-modeline-bar-width 4
      doom-modeline-buffer-file-name-style 'truncate-upto-project
      doom-modeline-minor-modes t
      doom-modeline-icon t
      doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-enable-word-count t
      doom-modeline-checker-simple-format t
      doom-modeline-persp-name t
      ;; Whether display environment version or not
      doom-modeline-env-version t
      ;; Or for individual languages
      doom-modeline-env-enable-python t
      doom-modeline-env-enable-ruby t
      doom-modeline-env-enable-perl t
      doom-modeline-env-enable-go t
      doom-modeline-env-enable-elixir t
      doom-modeline-env-enable-rust t

      ;; Change the executables to use for the language version string
      doom-modeline-env-python-executable "python"
      doom-modeline-env-ruby-executable "ruby"
      doom-modeline-env-perl-executable "perl"
      doom-modeline-env-go-executable "go"
      doom-modeline-env-elixir-executable "iex"
      doom-modeline-env-rust-executable "rustc"
      ;; What to display as the version while a new one is being loaded
      doom-modeline-env-load-string "..."
        ;; Hooks that run before/after the modeline version string is updated
      doom-modeline-before-update-env-hook nil
      doom-modeline-after-update-env-hook nil
      all-the-icons-scale-factor 1.1)
;; Prevent flash of unstyled modeline at startup
(unless after-init-time
  (setq-default mode-line-format nil))
(custom-set-faces
 '(mode-line ((t (:family "Iosevka Term" :height 0.9)))))
:hook (after-init . doom-modeline-mode))

(use-package helm-config
  :straight helm
  :init
  (helm-mode 1)
  (helm-ff-icon-mode 1)
  (helm-autoresize-mode 1)
  :config
  (progn
    (require 'helm-for-files)
    (setq helm-candidate-number-limit 100)
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-ff-file-name-history-use-recentf t
          helm-autoresize-max-height 0
          helm-autoresize-min-height 20
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match    t))
    :bind
    (("C-x b" . helm-buffers-list)
        ("C-x C-b" . helm-mini)
        ("C-x r b" . helm-bookmarks)
        ("C-x C-f" . helm-find-files)
        ("C-s" . helm-occur)
        ("M-x" . helm-M-x)
        ("M-y" . helm-show-kill-ring)
        ("C-c h" . helm-command-prefix)))

(use-package helm-swoop
:straight t
:bind
(("M-i" . helm-swoop)
 ("M-I" . helm-swoop-back-to-last-point)
 ("C-c M-i" . helm-multi-swoop)
 ("C-x M-i" . helm-multi-swoop-all))
:config
(progn
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))
;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; If you prefer fuzzy matching
;; (setq helm-swoop-use-fuzzy-match t)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically))

(use-package helm-projectile
  :init (helm-projectile-on))

(use-package helm-tramp
  :bind (("C-c t s" . helm-tramp)
         ("C-c t q" . helm-tramp-quit)))

(use-package winum
  ;; :custom
  ;; (winum-auto-setup-mode-line t)
  :hook (after-init . winum-mode)
  :bind (
         ;; Select the window with Meta
         ("M-1" . winum-select-window-1)
         ("M-2" . winum-select-window-2)
         ("M-3" . winum-select-window-3)
         ("M-4" . winum-select-window-4)
         ("M-5" . winum-select-window-5)
         ("M-6" . winum-select-window-6)))

(use-package winner
:straight (:type built-in)
:hook (after-init . winner-mode)
:init
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
        "*esh command on file*")))

(use-package ace-window
  :init
  ;; 使用字母而非数字标记窗口，便于跳转。
  (setq aw-keys '(?a ?w ?e ?g ?i ?j ?k ?l ?p))
  ;; 根据自己的使用习惯来调整快捷键，这里使用大写字母避免与 aw-keys 冲突。
  (setq aw-dispatch-alist
        '((?0 aw-delete-window "Delete Window")
          (?1 delete-other-windows "Delete Other Windows")
          (?2 aw-split-window-vert "Split Vert Window")
          (?3 aw-split-window-horz "Split Horz Window")
          (?F aw-split-window-fair "Split Fair Window")
          (?S aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?C aw-copy-window "Copy Window")
          ;; 为指定 window 选择新的 Buffer 并切换过去。
          (?B aw-switch-buffer-in-window "Select Buffer")
          ;; 为指定 window 选择新的 Buffer，切换到其它 buffer；
          (?O aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?N aw-flip-window)
          ;; 依赖 transpose-frame package
          (?T aw-transpose-frame "Transpose Frame")
          (?? aw-show-dispatch-help)))
  :config
  ;; 设置为 frame 后会忽略 treemacs frame，否则即使两个窗口时也会提示选择。
  ;;(setq aw-scope 'frame)
  ;; 总是提示窗口选择，进而执行 ace 命令。
  (setq aw-dispatch-always t)
  (global-set-key (kbd "M-o") 'ace-window))

(use-package magit
  :commands magit-file-delete
  :init
  (setq magit-auto-revert-mode nil)  ; we do this ourselves further down
  ;; Must be set early to prevent ~/.emacs.d/transient from being created
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil)

  (add-hook 'magit-popup-mode-hook 'hide-mode-line-mode)

  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package org
  :straight (org :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git")
  :ensure auctex
  :demand
  :ensure-system-package
  ((watchexec . watchexec)
   (magick . imagemagick))
  :config
  ;; 关闭与 pyim 冲突的 C-, 快捷键。
  (define-key org-mode-map (kbd "C-,") nil)
  (setq org-ellipsis ".."
        org-highlight-latex-and-related '(latex)
        ;; 隐藏标记。
        org-hide-emphasis-markers t
        ;; 去掉 * 和 /, 使它们不再具有强调含义。
        org-emphasis-alist
        '(("_" underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+" (:strike-through t)))
        ;; 隐藏 block
        org-hide-block-startup t
        org-hidden-keywords '(title)
        org-cycle-separator-lines 2
        org-cycle-level-faces t
        org-n-level-faces 4
        org-tags-column -80
        org-log-into-drawer t
        org-log-done 'note
        ;; 先从 #+ATTR.* 获取宽度，如果没有设置则默认为 300 。
        org-image-actual-width '(300)
        org-export-with-broken-links t
        org-startup-folded 'content
        ;; 使用 R_{s} 形式的下标（默认是 R_s, 容易与正常内容混淆) 。
        org-use-sub-superscripts nil
        ;; export 时不处理 super/subscripting, 等效于 #+OPTIONS: ^:nil 。
        org-export-with-sub-superscripts nil
        org-startup-indented t
        ;; 支持鼠标点击链接。
        org-return-follows-link t
        org-mouse-1-follows-link t
        ;; 文件链接使用相对路径, 解决 hugo 等 image 引用的问题。
        org-link-file-path-type 'relative)
  (setq org-catch-invisible-edits 'show)
  (setq org-todo-keywords
        '((sequence "☞ TODO(t)" "PROJ(p)" "⚔ INPROCESS(s)" "⚑ WAITING(w)"
                    "|" "☟ NEXT(n)" "✰ Important(i)" "✔ DONE(d)" "✘ CANCELED(c@)")
          (sequence "✍ NOTE(N)" "FIXME(f)" "☕ BREAK(b)" "❤ Love(l)" "REVIEW(r)" )))

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c b") 'org-switchb)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode 0))))

(use-package org-modern
  :after (org)
  :hook
  (org-mode . org-modern-mode))

;; 自动创建和更新目录。
(use-package org-make-toc
  :config
  (add-hook 'org-mode-hook #'org-make-toc-mode))

(use-package vterm
  :ensure-system-package
  ((cmake . cmake)
   (glibtool . libtool)
   (exiftran . exiftran))
  :config
  (setq vterm-set-bold-hightbright t)
  (setq vterm-always-compile-module t)
  (setq vterm-max-scrollback 100000)
  ;; vterm buffer 名称，需要配置 shell 来支持（如 bash 的 PROMPT_COMMAND）。
  (setq vterm-buffer-name-string "*vterm: %s")
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setf truncate-lines nil)
              (setq-local show-paren-mode nil)
              ))
  ;; 使用 M-y(consult-yank-pop) 粘贴剪贴板历史中的内容。
  ;; (define-key vterm-mode-map [remap consult-yank-pop] #'vterm-yank-pop)
  :bind
  (:map vterm-mode-map ("C-l" . nil))
  ;; 防止输入法切换冲突。
  (:map vterm-mode-map ("C-\\" . nil)) )

(use-package vterm-toggle
  :bind
  ("C-c v" . vterm-toggle))

(use-package multi-vterm
  :after (vterm)
  :config
  (define-key vterm-mode-map (kbd "M-RET") 'multi-vterm))

(use-package eshell-toggle
    :custom
    (eshell-toggle-size-fraction 3)
    (eshell-toggle-run-command nil)
    (eshell-toggle-init-function #'eshell-toggle-init-eshell)
    (eshell-toggle-window-side 'right)
    :bind
    ("C-c e" . eshell-toggle)
    )

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config   (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil))

(use-package saveplace-pdf-view
  :after pdf-view)

(use-package exec-path-from-shell
  :demand
  :custom
  ;; 去掉 -i 参数, 加快启动速度。
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-variables '("PATH" "MANPATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; 显示缩进。
(use-package highlight-indent-guides
  :demand
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-suppress-auto-error t)
  (highlight-indent-guides-delay 0.1)
  :config
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'yaml-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'js-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'web-mode-hook 'highlight-indent-guides-mode))

(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
         ([remap move-end-of-line] . mwim-end-of-code-or-line)))

;; 智能括号。
(use-package smartparens
  :demand
  ;; :disabled
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; 彩色括号。
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; 高亮匹配的括号。
(use-package paren
  :straight (:type built-in)
  :hook
  (after-init . show-paren-mode)
  :init
  (setq show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package envrc
  :ensure-system-package direnv
  :hook (after-init . envrc-global-mode)
  :config
  (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))

; browser-url 使用 Mac 默认浏览器。
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(use-package engine-mode
  :config
  (engine/set-keymap-prefix (kbd "C-c s"))
  (engine-mode t)
  ;;(setq engine/browser-function 'eww-browse-url)
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine twitter
    "https://twitter.com/search?q=%s"
    :keybinding "t")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis."))

(use-package tramp
  :straight (tramp :files ("lisp/*"))
  :config
  ;; 使用 ~/.ssh/config 中的 ssh 持久化配置。（Emacs 默认复用连接，但不持久化连接）
  (setq  tramp-ssh-controlmaster-options nil)
  ;; TRAMP buffers 关闭 version control, 防止卡住.
  (setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" vc-ignore-dir-regexp tramp-file-name-regexp))
  ;; 关闭自动保存 ad-hoc proxy 代理配置, 防止为相同 IP 的 VM 配置了错误的 Proxy.
  (setq tramp-save-ad-hoc-proxies nil)
  ;; 调大远程文件名过期时间（默认 10s), 提高查找远程文件性能.
  (setq remote-file-name-inhibit-cache 600)
  ;;tramp-verbose 10
  ;; 增加压缩传输的文件起始大小（默认 4KB），否则容易出错： “gzip: (stdin): unexpected end of file”
  (setq tramp-inline-compress-start-size (* 1024 8))
  ;; 当文件大小超过 tramp-copy-size-limit 时，用 external methods(如 scp）来传输，从而大大提高拷贝效率。
  (setq tramp-copy-size-limit (* 1024 1024 2))
  ;; 临时目录中保存 TRAMP auto-save 文件, 重启后清空。
  (setq tramp-allow-unsafe-temporary-files t)
  (setq tramp-auto-save-directory temporary-file-directory)
  ;; 连接历史文件。
  (setq tramp-persistency-file-name (expand-file-name "tramp-connection-history" user-emacs-directory))
  ;; 在整个 Emacs session 期间保存 SSH 密码.
  (setq password-cache-expiry nil)
  (setq tramp-default-method "ssh")
  (setq tramp-default-user "root")
  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))

;; 相比 Emacs 内置 Help, 提供更多上下文信息。
(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-minibuffer)
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  :diminish which-key-mode)

(use-package smartparens
  :demand
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package origami
:hook (prog-mode-hook . origami-mode)
:bind (("C-c f f" . origami-toggle-node)
       ("C-c f a" . origami-toggle-all-nodes)
       ("C-c f s" . origami-show-only-node)
       ("C-c f p" . origami-previous-fold)
       ("C-c f n" . origami-forward-fold)))
