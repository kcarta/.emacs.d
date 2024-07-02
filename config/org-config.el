(use-package toc-org
    :ensure t
    :commands toc-org-enable
    :init (add-hook 'org-mode-hook 'toc-org-enable))

(setq org-startup-indented t)
(setq org-todo-keyword-faces '(("TODO" . "#FF6347") ("CANCELED" . "#A9A9A9") ("DONE" . "#32CD32")))

(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
  (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

(add-hook 'org-mode-hook #'my/org-mode-hook)

(provide 'org-config)
