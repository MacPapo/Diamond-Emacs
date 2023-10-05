;; -*- lexical-binding: t; -*-

(use-package org
  :config
  (setq org-latex-listings 'minted)

  (add-to-list 'org-latex-packages-alist '("" "minted"))

  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

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
