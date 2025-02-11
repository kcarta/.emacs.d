(use-package emacs
  :custom
  ;; Backup and Autosave
  (backup-directory-alist '((".*" . "~/.emacs.d/archive/")))
  (auto-save-file-name-transforms `((".*" "~/.emacs.d/archive/" t)))
  
  ;; macOS-specific key settings
  (mac-option-key-is-meta t)
  (mac-right-option-modifier nil)
  
  ;; UI Settings
  (visible-bell nil)
  (ring-bell-function 'ignore)
  (truncate-lines nil)

  :init
  ;; UI tweaks
  (blink-cursor-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  
  ;; Enable context menu if in GUI
  (when (display-graphic-p)
  (context-menu-mode))

  (global-auto-revert-mode 1)
  (global-visual-line-mode 1))

;; Electric Pair Mode
(use-package electric-
  :hook (after-init . electric-pair-mode))

;; Org Mode Hook for Electric-Pair- Pairing Fix
(add-hook 'org-mode-hook
          (lambda (%)
            (setq-local electric-pair-inhibit-predicate
                        (lambda (c) (or (char-equal c ?<) (funcall electric-pair-inhibit-predicate c))))))

(provide 'variables)
