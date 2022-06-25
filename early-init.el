;; Diamond Emacs for Mac
;;
;; Visco01 config started in 2022

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
