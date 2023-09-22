;; -*- lexical-binding: t; -*-

(use-package org)

(use-package org-wc
  :after org)

(use-package org-pomodoro
  :after org)

(use-package org-zettelkasten
  :hook org-mode)

(use-package org-super-agenda
  :after org-agenda
  :config
  (setq org-agenda-files '("~/org/agenda/agenda.org"
                           "~/org/agenda/sessione.org"))
  (setq org-super-agenda-groups
        '((:name "Oggi"
                 :time-grid t
                 :scheduled today)
          (:name "Importante"
                 :priority "A")))
  (org-super-agenda-mode))

(provide 'init-org)
