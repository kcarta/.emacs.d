(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq custom-file "~/.config/emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; In config/
(add-to-list 'load-path "~/.emacs.d/config/")
(require 'variables)
(require 'bindings)
(require 'themes)
(require 'autocomplete)
(require 'shells)
(require 'org-config)
(require 'ai)
(require 'lang)

(use-package rg
  :ensure t
  :defer t)

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents . 5) (projects . 5)))
  :custom 
  (dashboard-modify-heading-icons '((recents . "file-text")))
  :config
  (dashboard-setup-startup-hook))

(use-package evil 
    :ensure t 
    :demand t
    :custom 
    (evil-want-keybinding nil)
    (evil-auto-indent nil)
    (org-return-follows-link t)
    :config
    (evil-mode)
    ;; Unmap keys in 'evil-maps
    ;; if not done, (setq org-return-follows-link t) will not work
    (define-key evil-motion-state-map (kbd "SPC") nil)
    (define-key evil-motion-state-map (kbd "RET") nil)
    (define-key evil-motion-state-map (kbd "TAB") nil))
