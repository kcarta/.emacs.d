;;; Startup - Packages & Custom

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;;; Basic Variables

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

  ;; Fold/Unfold with <tab> when in outline-minor-mode
  (outline-minor-mode-cycle t)

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

(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)

;; Turn on line numbers while code editing
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq-default mode-line-format
      '("%e" mode-line-front-space
	mode-line-frame-identification
	mode-line-buffer-identification
	mode-line-position
	mode-line-format-right-align
	(project-mode-line project-mode-line-format) "  "
	mode-line-modes
	mode-line-misc-info
	mode-line-end-spaces))
(setq evil-mode-line-format '(before . mode-line-modes))

;;; General Packages

;; Electric Pair Mode
(use-package electric-
  :hook (after-init . electric-pair-mode))

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

;;; Custom Bindings

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package general :ensure t
  :config (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer kc/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (kc/leader-keys
    "SPC" '(execute-extended-command :wk "Execute command (M-x)")
    "." '(find-file :wk "Find file"))

  (kc/leader-keys
    "a" '(:ignore t :wk "AI")
    "a s" '(gptel-send :wk "Send Prompt")
    "a f" '(gptel-add-file :wk "Add File to Context")
    "a c" '(gptel-add :wk "Add Region/Buffer to Context")
    "a q" '(gptel-abort :wk "Abort")
    )

  (kc/leader-keys
    "b" '(:ignore t :wk "Buffers")
    "b b" '(switch-to-buffer :wk "Switch to buffer (C-x b)")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer"))

  (kc/leader-keys
    "f" '(:ignore t :wk "Files")
    "f c" '(lambda ()
	     (interactive)
	     (find-file user-init-file) :wk "Open emacs init.el")
    "f d" '(make-directory :wk "Create directory")
    "f f" '(project-find-file :wk "Find file in the project")
    "f n" '(rename-file :wk "Rename (move) file")
    "f r" '(delete-file :wk "Delete file")
    "f y" '(copy-file :wk "Copy file"))

  (kc/leader-keys
    "h" '(:ignore t :wk "Help")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h i" '(info :wk "Info")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))

  (kc/leader-keys
    "e" '(:ignore t :wk "Elisp")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate an elisp expression")
    "e r" '(eval-region :wk "Evaluate elisp in region"))

  (kc/leader-keys
    "l" '(:ignore t :wk "Languages")
    "l l" '(eglot :wk "Start Eglot")
    "l p" '(prettier-prettify :wk "Prettify buffer")
    "l s" '(ispell :wk "Spell check"))

 (kc/leader-keys
    "m" '(:ignore t :wk "Markdown")
    "m f" '(markdown-insert-footnote :wk "Insert footnote")
    "m l" '(markdown-insert-link :wk "Insert link"))

  (kc/leader-keys
    "o" '(:ignore t :wk "Open")
    "o d" '(dashboard-open :wk "Dashboard")
    "o f" '(make-frame :wk "Open buffer in new frame")
    "o s" '(
      (lambda ()
        (interactive)
        (let ((height (truncate (* 0.3 (frame-height)))))
          (split-window-below (- height))
          (other-window 1)
          (eshell)))
      :wk "Eshell"))

  (kc/leader-keys
    "r" '(:ignore t :wk "Org")
    "r a" '(org-archive-subtree :wk "Archive (subtree)")
    "r r" '(org-refile :wk "Refile")
    "r s" '(org-sort :wk "Sort")
    "r t" '(org-set-tags-command :wk "Set tags")
    "r T" '(org-todo-list :wk "Todo list"))

  (kc/leader-keys
    "r v" '(:ignore t :wk "Org View Settings")
    "r v i" '(org-toggle-inline-images :wk "Toggle inline images in org mode")
    "r v n" '(org-narrow-to-subtree :wk "Narrow to subtree")
    "r v w" '(widen :wk "Widen")
    )

  (kc/leader-keys
    "t" '(:ignore t :wk "Timers")
    "t s" '(org-timer-start :wk "Start Timer")
    "t t" '(org-timer-stop :wk "Stop Timer")
    "t p" '(org-timer-set-timer :wk "Set Timer"))

  (kc/leader-keys
    "w" '(:ignore t :wk "Windows")
    "w f" '(toggle-frame-fullscreen :wk "Enter/Exit fullscreen")
    "w c" '(evil-window-delete :wk "Close window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")))

;;; Custom Commands and Functions

(defun jekyll-insert-front-matter ()
  "Insert Jekyll front matter at point."
  (interactive)
  (insert (format "---
layout: post
title:  \"Title goes here\"
date:   %s 00:00:00 +0000
tags: []
---\n" (format-time-string "%Y-%m-%d"))))

(defun jekyll-insert-post-link (filename link-text)
  "Insert link to other post in Jekyll blog."
  (interactive
    (list
      (let
        (
          (posts-dir
            (expand-file-name "_posts"
              (locate-dominating-file default-directory "_posts"))))
        (if (and (eq major-mode 'dired-mode) (region-active-p))
          ;; If in dired with active region, use selected filename
          (file-name-sans-extension
            (file-name-nondirectory (dired-get-filename)))
          ;; Otherwise prompt with completion
          (let*
            (
              (files
                (and (file-exists-p posts-dir)
                  (directory-files posts-dir
                    nil
                    "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}-.*\\.\\(md\\|markdown\\)$")))
              (names (mapcar #'file-name-sans-extension files)))
            (completing-read "Post file: " names nil t))))
      ;; Get link text from region or prompt
      (if (and (not (eq major-mode 'dired-mode)) (region-active-p))
        (buffer-substring-no-properties
          (region-beginning)
          (region-end))
        (read-string "Link text: "))))
  ;; Delete region if it was active (in markdown-mode)
  (when (and (not (eq major-mode 'dired-mode)) (region-active-p))
    (delete-region (region-beginning) (region-end)))
  ;; Insert the formatted markdown link
  (insert "[" link-text "](" "{% post_url " filename " %}" ")"))

(defun jekyll-insert-image (filename alt-text)
  "Insert image link from the 'img' directory in Jekyll blog."
  (interactive
    (list
      (let ((img-dir "../static/img/posts/"))
        (if (and (eq major-mode 'dired-mode) (region-active-p))
          ;; If in dired with active region, use selected filename
          (file-name-nondirectory (dired-get-filename))
          ;; Otherwise prompt with completion
          (let
            (
              (files
                (and (file-exists-p img-dir)
                  ;; Exclude "." and ".."
                  (directory-files img-dir nil "^[^.]"))))
            (completing-read "Image file: " files nil t))))
      ;; Get alt text from region or prompt
      (if (and (not (eq major-mode 'dired-mode)) (region-active-p))
        (buffer-substring-no-properties
          (region-beginning)
          (region-end))
        (read-string "Alt text: "))))
  ;; Insert Markdown image syntax
  (insert (format "![%s](/static/img/posts/%s)" alt-text filename)))
(defun my-add-org-items-to-shopping-reminders ()
  "Extract all list items from the current Org buffer and add them to the 'Shopping' list in Apple Reminders."
  (interactive)
  (require 'org-element)
  ;; Extract all list items
  (let ((parsed (org-element-parse-buffer))
        (items '()))
    (org-element-map parsed 'item
      (lambda (item)
        (let ((content (org-element-property :contents-begin item))
              (end (org-element-property :contents-end item)))
          (when (and content end)
            (push (buffer-substring-no-properties content end) items)))))
    ;; Add each item to Apple Reminders
    (dolist (item (reverse items))
      (let ((as-command (format
                         "tell application \"Reminders\"
                            tell list \"Shopping\"
                              make new reminder with properties {name:\"%s\"}
                            end tell
                          end tell"
                         item)))
        (message "Adding to Reminders: %s" item)  ;; Debug
        (shell-command (format "osascript -e '%s'" as-command)))))
  (message "All items added to the 'Shopping' list."))

(defun my-add-item-to-shopping-reminders (item)
  "Prompt for ITEM and add it to the ‘Shopping’ list in Apple Reminders."
  (interactive "sAdd item to Shopping: ")
  (let ((as-command (format
	"tell application \"Reminders\"
		tell list \"Shopping\"
			make new reminder with properties {name:\"%s\"}
		end tell
	end tell"
	item)))
    (message "AppleScript: %s" as-command)  ;; Debug
    (shell-command (format "osascript -e '%s'" as-command))))
