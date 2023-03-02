;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

(setq package-enable-at-startup nil) ;; Prevent double loading of libraries
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

(setenv "LIBRARY_PATH" (concat
			"/opt/homebrew/opt/gcc/lib/gcc/12"
			":"
			"/opt/homebrew/opt/libgccjit/lib/gcc/12"
			":"
			"/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin22/12"
			))
