;; Diamond Emacs for Mac
;;
;; MacPapo config started in 2022

(defun diamond-prog-mode-hook ()
  (display-line-numbers-mode 1))
(add-hook 'prog-mode-hook 'diamond-prog-mode-hook)

(defun diamond-text-mode-hook ()
  (display-line-numbers-mode 1))
(add-hook 'text-mode-hook 'diamond-text-mode-hook)

(defun diamond-conf-mode-hook ()
  (display-line-numbers-mode 1))
(add-hook 'conf-mode-hook 'diamond-conf-mode-hook)
