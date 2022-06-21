;; Custom Emacs config for macOS
;;
;; MacPapo config started in 2022

;; Hide clutter
(when window-system
  (scroll-bar-mode 0)   ; Disable the scroll bar
  (tool-bar-mode 0)     ; Disable the tool bar
  (tooltip-mode 0))     ; Disable the tooltips

;; Better Default
(setq-default
 ad-redefinition-action 'accept                   ; Silence warnings for redefinition
 auto-window-vscroll nil                          ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 cursor-in-non-selected-windows nil               ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
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
 vc-follow-symlinks t                             ; When the symlink points to a version-controlled file
 use-default-font-for-symbols nil                 ; Do not use the frame font when rendering emojis
 frame-inhibit-implied-resize nil                ; Don't ask for confirmation when opening symlinked file
 )

(cd "~/")                                         ; Move to the user directory
(global-display-line-numbers-mode t)              ; Enable line numbers globally
(delete-selection-mode 1)                         ; Replace region when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)
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

;; Fido
(fido-vertical-mode 1)

;; Winner for window managing
(winner-mode 1)

;; User name
(setq user-full-name "Pietro Visconti")
(setq user-mail-address "885448@stud.unive.it")

;; make electric-pair-mode work on more brackets
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))

;; UTF-8 as default encoding
(set-default-coding-systems 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;; highlight brackets if visible, else entire expression
(setq show-paren-style 'mixed)

;; Modus theme customizations ;;

;; Modeline
(setq modus-themes-mode-line '(borderless))

;; Selection
(setq modus-themes-region '(bg-only))

;; Completion
(setq modus-themes-completions 'opinionated)

;; Syntax
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-paren-match '(bold intense))

(setq modus-themes-prompts '(bold italic))

(setq modus-themes-completions
      '((matches . (extrabold background intense))
        (selection . (semibold accented intense))
        (popup . (accented))))

;; Org
(setq modus-themes-headings
      '((1 . (rainbow overline background 1.4))
        (2 . (rainbow background 1.3))
        (3 . (rainbow bold 1.2))
        (4 . (semilight 1.1))))

(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-scale-headings t)

(load-theme 'modus-operandi t)

;; Font attribute
(set-face-attribute 'default nil
                    :family "Roboto Mono"
                    :weight 'light
                    :height 140)

(set-face-attribute 'bold nil
                    :family "Roboto Mono"
                    :weight 'regular)

(set-face-attribute 'italic nil
                    :family "Victor Mono"
                    :weight 'semilight
                    :slant 'italic)

(set-fontset-font t 'unicode
                    (font-spec :name "Inconsolata Light"
                               :size 16) nil)

(set-fontset-font t '(#xe000 . #xffdd)
                     (font-spec :name "RobotoMono Nerd Font"
                                :size 12) nil)
