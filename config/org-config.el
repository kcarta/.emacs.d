(setq org-startup-indented t)

(setq
 org-auto-align-tags nil
 org-tags-column 0)

(setq org-agenda-files '("~/notes"))
(setq org-archive-location "~/notes/archive")

(setq org-agenda-remove-tags t)

(setq org-agenda-prefix-format
      '((todo . " %15c: ")))

(setq org-todo-keywords '((sequence "TODO(t)"
				    "|"
				    "DONE(!)" "CANCELED(!)")))

(setq org-priority-lowest 4
      org-priority-default 3
      org-priority-highest 1)

;(setq org-refile-use-outline-path 'file)

;(setq org-refile-targets
    ;'((nil :maxlevel . 1) (org-agenda-files :level . 1)))

(provide 'org-config)
