;;; emacs-mac-follow-appearance.el --- Follow macOS appearance settings (Dark or Light modes) -*- lexical-binding: t; -*-

;; Copyright (C) 2021 ksixty

;; Licensed under the same terms as Emacs.

;; Author: ksixty
;; Maintainer: ksixty <k60 at fmap dot me>
;; Created: Apr 8 2021
;; Version: 0.1
;; Keywords: gui, os-integration
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;; This program works only with Yamamoto Mitsuharu's emacs-mac-port.

;;; Code:

(defcustom emfa/dark-theme 'modus-vivendi
  "The theme to enable when Dark appearance is active.  Cannot be nil."
  :type 'symbol
  :group 'emacs-mac-follow-appearance)

(defcustom emfa/light-theme 'modus-operandi
  "The theme to enable when Dark appearance is active.  Nil means default theme will be used."
  :type 'symbol
  :group 'emacs-mac-follow-appearance)

(defun emfa/dark-mode-p ()
  "Return t if macOS Dark mode is currently active."
  (string= (plist-get (mac-application-state) :appearance)
           "NSAppearanceNameDarkAqua"))

(defun emfa/load-theme ()
  "Disable all themes and enables a theme based on value of emfa/dark-theme and emfa/light-theme custom variables."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (if (emfa/dark-mode-p)
      (load-theme emfa/dark-theme t)
    (when emfa/light-theme
      (load-theme emfa/light-theme t))))

(emfa/load-theme)

(define-key mac-apple-event-map [application-kvo effectiveAppearance]
  'emfa/load-theme)

(provide 'emacs-mac-follow-appearance)

;;; emacs-mac-follow-appearance.el ends here
