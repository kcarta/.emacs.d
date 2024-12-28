(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;;(setq org-hide-block-startup t)
;; Bug? https://lists.gnu.org/archive/html/emacs-orgmode/2024-02/msg00472.html
(setq org-startup-indented t)
(setq org-todo-keyword-faces
  '
  (("TODO" . "#FF6347")
    ("CANCELED" . "#A9A9A9")
    ("DONE" . "#32CD32")))

(setq org-agenda-files '("~/notes"))
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets
  '((nil :maxlevel . 1) (org-agenda-files :level . 1)))

(provide 'org-config)
