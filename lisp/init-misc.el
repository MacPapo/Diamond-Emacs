;; -*- lexical-binding: t; -*-

(use-package info-colors
  :ensure t
  :init
  (with-eval-after-load 'info
    (add-hook 'Info-selection-hook 'info-colors-fontify-node)))

(use-package shfmt
  :ensure t)

(use-package dotenv-mode
  :ensure t)

(use-package crux
  :ensure t
  :bind
  ([remap move-beginning-of-line] . crux-move-beginning-of-line)
  ([remap kill-whole-line]        . crux-kill-whole-line)
  ("C-<backspace>"                . crux-kill-line-backwards)
  ("C-S-o"                        . crux-smart-open-line-above)
  ("C-o"                          . crux-smart-open-line)
  ("C-c n"                        . crux-cleanup-buffer-or-region)
  ("C-c d"                        . crux-duplicate-current-line-or-region)
  ("C-c M-d"                      . crux-duplicate-and-comment-current-line-or-region)
  ("C-c r"                        . crux-rename-file-and-buffer)
  ("C-^"                          . crux-top-join-line)
  ("C-x C-u"                      . crux-upcase-region)
  ("C-x C-l"                      . crux-downcase-region)
  ("C-x M-c"                      . crux-capitalize-region))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-misc)
