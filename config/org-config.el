(use-package toc-org
    :ensure t
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

;(setq org-hide-block-startup t)
; Bug? https://lists.gnu.org/archive/html/emacs-orgmode/2024-02/msg00472.html
(setq org-startup-indented t)
(setq org-todo-keyword-faces '(("TODO" . "#FF6347") ("CANCELED" . "#A9A9A9") ("DONE" . "#32CD32")))

(defun my/org-mode-hook ()
  "Customize org-level headers to have a consistent height and a pastel blue color scheme."
  (dolist (face-color-pair '((org-level-1 . "#050C9C")
                             (org-level-2 . "#3572EF")
                             (org-level-3 . "#3ABEF9")
                             (org-level-4 . "#A7E6FF")
                             (org-level-5 . "#A7E6FF")))
    (set-face-attribute (car face-color-pair) nil
                        :weight 'semi-bold
                        :height 1.0
                        :foreground (cdr face-color-pair))))

(add-hook 'org-mode-hook #'my/org-mode-hook)
(setq org-agenda-files '("~/notes"))
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((nil :maxlevel . 1)(org-agenda-files :level . 1)))

(provide 'org-config)
