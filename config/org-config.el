(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;(setq org-hide-block-startup t)
;; Bug? https://lists.gnu.org/archive/html/emacs-orgmode/2024-02/msg00472.html
(setq org-startup-indented t)

(use-package org-modern
  :ensure t
  :init
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))
(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t)

;; Ellipsis styling
(setq org-ellipsis "...")
(set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)
 (global-org-modern-mode)

(setq org-agenda-files '("~/notes"))
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets
    '((nil :maxlevel . 1) (org-agenda-files :level . 1)))

(provide 'org-config)
