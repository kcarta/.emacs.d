(use-package emacs
  :init
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; In config/
(add-to-list 'load-path "~/.emacs.d/config/")
(require 'variables)
(require 'bindings)
(require 'themes)
(require 'autocomplete)
(require 'shells)
(require 'org-config)
(require 'projects)
(require 'ai)
(require 'lang)

(use-package rg :ensure t)

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '(
			  (recents . 5)
			  (projects . 5)
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
   '("34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(package-selected-packages
   '(elisa gptel org-modern elisp-autofmt evil dashboard rg lsp-dart markdown-mode json-mode prettier treemacs-icons-dired treemacs-projectile treemacs-evil treemacs toc-org eshell-syntax-highlighting eshell-toggle exec-path-from-shell which-key cape corfu consult marginalia orderless vertico doom-modeline rainbow-delimiters doom-themes all-the-icons-dired all-the-icons idle-highlight-mode general)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:background "#1c1f24" :foreground "#bbc2cf"))))
 '(org-checkbox ((t (:foreground "#ECBE7B" :weight bold))))
 '(org-level-1 ((t (:foreground "#51afef" :weight bold :height 1.3))))
 '(org-level-2 ((t (:foreground "#98be65" :weight bold :height 1.2))))
 '(org-level-3 ((t (:foreground "#da8548" :weight bold :height 1.1))))
 '(org-level-4 ((t (:foreground "#c678dd" :weight bold))))
 '(org-level-5 ((t (:foreground "#46D9FF"))))
 '(org-level-6 ((t (:foreground "#ECBE7B"))))
 '(org-level-7 ((t (:foreground "#a9a1e1"))))
 '(org-level-8 ((t (:foreground "#ff6c6b"))))
 '(org-table ((t (:foreground "#5699AF"))))
 '(org-todo-keyword-faces '(("TODO" . "#FF6347") ("CANCELED" . "#A9A9A9") ("DONE" . "#32CD32"))))
