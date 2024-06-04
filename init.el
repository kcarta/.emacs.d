(add-to-list 'load-path "~/.emacs.d/scripts/")
(add-to-list 'load-path "~/.emacs.d/config/")

(require 'elpaca-setup)  ; The Elpaca Package Manager
(require 'buffer-move)   ; Buffer-move for better window management

(require 'variables)
(require 'bindings)
(require 'themes)
;; TODO follow https://lambdaland.org/posts/2024-05-30_top_emacs_packages/#fn:2
(require 'autocomplete)
(require 'shells)
(require 'org-config)
(require 'feeds)

(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          ))
  :custom 
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

(use-package evil 
    :ensure t 
    :demand t
    :init 
      (setq evil-want-keybinding nil)
      (setq evil-vsplit-window-right t)
      (setq evil-split-window-below t)
      ;; Unmap keys in 'evil-maps, if not done, (setq org-return-follows-link t) will not work
      (with-eval-after-load 'evil-maps
	(define-key evil-motion-state-map (kbd "SPC") nil)
	(define-key evil-motion-state-map (kbd "RET") nil)
	;; TODO figure out how to get this to work 
	;;(define-key evil-normal-state-map (kbd "RET") (lambda () (interactive) (next-line)))
	(define-key evil-motion-state-map (kbd "TAB") nil))
      (setq org-return-follows-link t)
      (evil-mode))
