(add-to-list 'load-path "~/.emacs.d/scripts/")
(add-to-list 'load-path "~/.emacs.d/config/")

(require 'elpaca-setup)  ; The Elpaca Package Manager
(require 'buffer-move)   ; Buffer-move for better window management

(require 'variables)
(require 'bindings)
(require 'themes)
(require 'autocomplete)
(require 'shells)
(require 'org-config)
(require 'projects)
(require 'lang)

(use-package transient :ensure t)

(use-package rg :ensure t)

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-items '(
			  (recents . 5)
			  (projects . 3)
                          ))
  :custom 
  (dashboard-modify-heading-icons '((recents . "file-text")))
  :config
  (dashboard-setup-startup-hook))

(use-package evil 
    :ensure t 
    :demand t
    :init 
      (setq evil-want-keybinding nil)
      (setq evil-vsplit-window-right t)
      (setq evil-split-window-below t)
      (setq evil-auto-indent nil)
      ;; Unmap keys in 'evil-maps, if not done, (setq org-return-follows-link t) will not work
      (with-eval-after-load 'evil-maps
	(define-key evil-motion-state-map (kbd "SPC") nil)
	(define-key evil-motion-state-map (kbd "RET") nil)
	(define-key evil-motion-state-map (kbd "TAB") nil))
      (setq org-return-follows-link t)
      (evil-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
