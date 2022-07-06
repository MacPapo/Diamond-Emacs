;; Diamond Emacs for Mac
;;
;; MacPapo config started in 2022

(when window-system
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  )

(set-frame-parameter (selected-frame) 'alpha '(97 . 100))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter nil 'fullscreen 'fullboth)

(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 bidi-paragraph-direction 'left-to-right
 help-window-select t                             ; Focus new help windows when opened
 indent-tabs-mode nil                             ; Use tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                            ; Yank at point rather than pointer
 ns-use-srgb-colorspace nil                       ; Don't use sRGB colors
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-help-function nil                           ; Disable help messages
 show-trailing-whitespace t                       ; Display trailing whitespaces
 split-height-threshold nil                       ; Disable vertical window splitting
 split-width-threshold nil                        ; Disable horizontal window splitting
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally
 x-stretch-cursor t                               ; Stretch cursor to the glyph width
 delete-old-versions -1                           ; Delete excess backup versions silently
 version-control t                                ; Use version control
 ring-bell-function 'ignore                       ; Silent bell when you make a mistake
 inhibit-compacting-font-caches t                 ; Faster navigation point (costs more memory)
 recentf-mode t                                   ; Keep recent files
 make-backup-files nil                            ; Stop creating backup files
 create-lockfiles nil
 vc-follow-symlinks t                             ; When the symlink points to a version-controlled file
 use-default-font-for-symbols nil                 ; Do not use the frame font when rendering emojis
 frame-inhibit-implied-resize nil                ; Don't ask for confirmation when opening symlinked file
 )

(cd "~/")                                         ; Move to the user directory
(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(global-hl-line-mode 1)                           ; HIghlight the cursor line
(display-battery-mode 1)                          ; Display battery percentage in modeline
(global-auto-revert-mode 1)                       ; Automatically revert a buffer when it changes on disk
(fringe-mode '(8 . 0))                            ; Enable fringe on the left for git-gutter-fringe+
(electric-pair-mode t)                            ; Enable Matching delimeters
(electric-indent-mode t)                          ; Auto indentation
(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-subword-mode 1)                           ; Iterate through CamelCase words
(menu-bar-mode 1)                                 ; Enable the menu bar for macOS Full Screen
(mouse-avoidance-mode 'jump)                      ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)              ; Enable downcase-region
(put 'upcase-region 'disabled nil)                ; Enable upcase-region
(show-paren-mode 1)                               ; Highlight matching parenthesis

(save-place-mode 1)

(setq find-file-visit-truename t
      vc-follow-symlinks t)

(setq find-file-suppress-same-file-warnings t)

(setq kill-do-not-save-duplicates t)

(nconc
 auto-mode-alist
 '(("/LICENSE\\'" . text-mode)
   ("\\.log\\'" . text-mode)
   ("rc\\'" . conf-mode)
   ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode))
 )

(setq confirm-nonexistent-file-or-buffer nil)

(setq hscroll-margin 2
       hscroll-step 1
       scroll-conservatively 101
       scroll-margin 0
       scroll-preserve-screen-position t
       auto-window-vscroll nil
       mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
       mouse-wheel-scroll-amount-horizontal 2
       )

(setq blink-matching-paren nil)

(setq x-stretch-cursor nil)

(setq frame-title-format '("%b – Diamond Emacs")
      icon-title-format frame-title-format)

(setq frame-resize-pixelwise t)

(setq window-resize-pixelwise nil)

(setq enable-recursive-minibuffers t)

(setq echo-keystrokes 0.02)

(setq resize-mini-windows 'grow-only)

(setq-default display-line-numbers-width 3)

(setq-default display-line-numbers-widen t)

(setq ansi-color-for-comint-mode t)
(setq org-hide-emphasis-markers t)

(setq user-full-name "Jacopo Costantini")
(setq user-mail-address "891938@stud.unive.it")

(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\}))
      )

(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(setq show-paren-style 'mixed)

(setq modus-themes-mode-line '(borderless)
      )

(setq modus-themes-region '(bg-only))

(setq modus-themes-completions 'opinionated)

(setq modus-themes-completions
        '((matches . (extrabold background intense))
          (selection . (semibold accented intense))
          (popup . (accented))))

(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-paren-match '(bold intense))
(setq modus-themes-hl-line t)
(setq modus-themes-prompts '(bold italic)
      )

(setq modus-themes-headings
      '((1 . (rainbow background 1.4))
        (2 . (rainbow background 1.3))
        (3 . (rainbow bold 1.2))
        (4 . (semilight 1.1))))

(setq modus-themes-org-blocks nil)
(setq modus-themes-scale-headings t)

(load-theme 'modus-operandi t)

(when (member "Iosevka" (font-family-list))
  (set-frame-font "Iosevka-16" t t))

(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(setq tramp-default-method "ssh")
(define-key global-map (kbd "C-c t") 'helm-tramp)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
