;; Diamond Emacs for Mac
;;
;; MacPapo config started in 2022

(when window-system
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (tooltip-mode 0)
  (menu-bar-mode 0))

(set-frame-parameter (selected-frame) 'alpha '(97 . 100))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter nil 'fullscreen 'fullboth)

(when (member "Iosevka" (font-family-list))
  (set-frame-font "Iosevka-16" t t))

(setq-default
 indent-tabs-mode nil                             ; Use tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ";; Kept you waiting huh!"  ; Empty the initial *scratch* buffer
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 display-time-default-load-average nil            ; Don't display load average
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 delete-old-versions -1                           ; Delete excess backup versions silently
 make-backup-files nil                            ; Stop creating backup files
 create-lockfiles nil
 use-default-font-for-symbols nil                 ; Do not use the frame font when rendering emojis
 frame-inhibit-implied-resize nil                 ; Don't ask for confirmation when opening symlinked file
 )

(delete-selection-mode 1)                          ; Replace region when inserting text
(display-time-mode 1)                              ; Enable time in the mode-line
(fringe-mode '(8 . 0))                             ; Enable fringe on the left for git-gutter-fringe+
(display-battery-mode 1)                           ; Display battery percentage in modeline
(global-auto-revert-mode 1)                        ; Automatically revert a buffer when it changes on disk
(fset 'yes-or-no-p 'y-or-n-p)                      ; Replace yes/no prompts with y/n
(global-subword-mode 1)                            ; Iterate through CamelCase words

(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 1024 1024))

(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)

(setq jit-lock-defer-time nil)
(setq jit-lock-context-time 0.1)
(setq fast-but-imprecise-scrolling nil)
(setq redisplay-skip-fontification-on-input nil)
(setq idle-update-delay 0.1)

(setq minibuffer-message-timeout 1)

(setq inhibit-compacting-font-caches t)                              ; Highlight matching parenthesis

(require 'emacs-mac-follow-appearance)
