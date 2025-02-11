(use-package org
  :custom
  (org-startup-indented t)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-files '("~/notes"))
  (org-archive-location "~/notes/archive")
  (org-agenda-remove-tags t)
  (org-agenda-prefix-format '((todo . " %15c: ")))
  (org-todo-keywords '((sequence "TODO(t)"
				 "|"
				 "DONE(!)" "CANCELED(!)")))
  (org-priority-lowest 4)
  (org-priority-default 3)
  (org-priority-highest 1))
;(setq org-refile-use-outline-path 'file)

;(setq org-refile-targets
    ;'((nil :maxlevel . 1) (org-agenda-files :level . 1)))

(provide 'org-config)
