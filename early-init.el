;; Diamond Emacs for Mac
;;
;; MacPapo config started in 2022

(setq gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer noninteractive)

(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay))
          )

(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage)
  )

(define-advice startup--load-user-init-file (:before (&rest _) init-doom)
  (advice-remove #'load-file #'load-file@silence)
  )

(when (fboundp 'native-compile-async)
  (setenv "LIBRARY_PATH"
          (concat (getenv "LIBRARY_PATH")
                  "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin21/11"))
  (setq native-comp-speed 2)
  (setq native-comp-async-jobs-number 4)
  (setq native-comp-deferred-compilation nil)
  (setq native-comp-deferred-compilation-deny-list '())
  (setq native-comp-async-report-warnings-errors 'silent))

;; 加载较新的 .el 文件。
(setq-default load-prefer-newer t)

;; 关闭 cl 告警。
(setq byte-compile-warnings '(cl-functions))

;; 关闭 package.el(后续使用 straight.el) 。
(setq package-enable-at-startup nil)

;; 启动时开启 debug, 启动后关闭。
(setq debug-on-error t)
(add-hook 'emacs-startup-hook (lambda () (setq debug-on-error nil)))

;; 设置缩放模式, 避免 MacOS 最大化窗口后右边和下边有空隙。
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)

;; 加 t 参数让 togg-frame-XX 最后运行，这样最大化才生效。
;;(add-hook 'window-setup-hook 'toggle-frame-fullscreen t) 
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

;; 在单独文件保存自定义配置，避免污染 ~/.emacs 文件。
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(add-hook 'after-init-hook (lambda () (when (file-exists-p custom-file) (load custom-file))))

;; 个人信息。
(setq user-full-name "Jacopo Costantini")
(setq user-mail-address "jacopocostantini32@gmail.com")

;; 缺省使用 email 地址加密。
(setq-default epa-file-select-keys nil)
(setq-default epa-file-encrypt-to user-mail-address)

;; 使用 minibuffer 输入 GPG 密码。
(setq-default epa-pinentry-mode 'loopback)

;; 加密认证信息文件。
(setq auth-sources '("~/.authinfo.gpg"))

;; 缓存对称加密密码。
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; 认证不过期, 默认 7200。
(setq auth-source-cache-expiry nil)
;;(setq auth-source-debug t)
