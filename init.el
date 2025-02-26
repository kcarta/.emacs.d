;;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; In config/
(add-to-list 'load-path "~/.emacs.d/config/")
(require 'custom-bindings)
(require 'custom-commands)

;;; Set basic variables
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

;; Turn on line numbers while code editing
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Electric Pair Mode
(use-package electric-
  :hook (after-init . electric-pair-mode))

;; Org Mode Hook for Electric-Pair- Pairing Fix
(add-hook 'org-mode-hook
          (lambda (%)
            (setq-local electric-pair-inhibit-predicate
                        (lambda (c) (or (char-equal c ?<) (funcall electric-pair-inhibit-predicate c))))))
(use-package rg
  :ensure t
  :defer t)

;;; General Packages

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

;;; Theming

(use-package idle-highlight-mode
  :ensure t
  :custom
  (idle-highlight-idle-time 0.2)
  :hook ((prog-mode text-mode) . idle-highlight-mode))

(when (display-graphic-p)
  (set-face-attribute 'default nil
		      :font "Fira Code"
		      :height 160
		      :weight 'regular)
  (set-face-attribute 'variable-pitch nil
		      :family "Georgia"
		      :height 1.2))

;; Create a hook for markdown-mode to use the variable-pitch face
(add-hook 'markdown-mode-hook
  (lambda ()
    (variable-pitch-mode 1)
    (setq buffer-face-mode-face 'variable-pitch)
    (buffer-face-mode 1)
    (setq-local line-spacing 0.4)))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :defer t)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  :defer t)

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload))

;; Highlight delimiters like parenthesis 
(use-package rainbow-delimiters
  :ensure t
  :hook
  ((prog-mode . rainbow-delimiters-mode))
  :defer t)

;;; Org

(use-package org
  :custom
  (org-startup-indented t)
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-files '("~/notes"))
  (org-archive-location "~/notes/archive")
  (org-agenda-remove-tags t)
  (org-agenda-prefix-format '((todo . " %15c: ")))
  (org-todo-keywords '((sequence "TODO(t)"
				 "|"
				 "DONE(!)" "CANCELED(!)")))
  (org-priority-lowest 4)
  (org-priority-default 3)
  (org-priority-highest 1))
;(setq org-refile-use-outline-path 'file)

;(setq org-refile-targets
    ;'((nil :maxlevel . 1) (org-agenda-files :level . 1)))

;;; Programming

(use-package prettier
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :custom
  (markdown-command "/usr/local/bin/multimarkdown"))

;;; AI

(use-package gptel
  :ensure t
  :defer t
  :custom
  (gptel-model 'phi4:latest)
  :init
  (setq gptel-backend (gptel-make-ollama "Ollama"
			:host "localhost:11434"
			:stream t
			:models '(phi4:latest))))

;;; Shells

(use-package exec-path-from-shell
  :ensure t
  :if (display-graphic-p) ;; Only run in GUI mode
  :init
  (exec-path-from-shell-initialize)
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "SHELL")))

(use-package eshell-toggle
  :ensure t
  :defer t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

(use-package eshell-syntax-highlighting
  :ensure t
  :hook (eshell-mode . eshell-syntax-highlighting-global-mode))

(setq
  eshell-rc-script (concat user-emacs-directory "eshell/profile")
  eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
  eshell-history-size 5000
  eshell-buffer-maximum-lines 5000
  eshell-hist-ignoredups t
  eshell-scroll-to-bottom-on-input t
  eshell-destroy-buffer-when-process-dies t
  eshell-visual-commands '("bash" "htop" "ssh" "top" "zsh"))


;;; Minibuffer

;; Vertico: Completions in the minibuffer.
(use-package vertico
  :ensure t
  :init (vertico-mode))

;; Orderless: fuzzy completions in the minibuffer.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia: Rich annotations in the minibuffer.
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding available in the *Completions* buffer, add it to the `completion-list-mode-map'.
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that the mode gets enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;; A few more useful Minad-stack configurations...
(use-package emacs
  :init
  ;; vertico
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons
      (format "[CRM%s] %s"
        (replace-regexp-in-string
          "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'"
          ""
          crm-separator)
        (car args))
      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t))

;; which-key: show keybindings in the minibuffer
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-add-column-padding 1)
  (which-key-max-display-columns nil)
  (which-key-min-display-lines 6)
  (which-key-side-window-slot -10)
  (which-key-side-window-max-height 0.25)
  (which-key-idle-delay 0.5)
  (which-key-max-description-length 25)
  (which-key-allow-imprecise-window-fit nil)
  (which-key-separator " -> "))
