;;; Startup - Packages & Custom
;; Load from the Elpaca init file, from https://github.com/progfolio/elpaca
(setq elpaca-init "~/.emacs.d/elpaca-init.el")
(when (file-exists-p elpaca-init)
  (load elpaca-init))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

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
  (mac-right-command-modifier 'meta)
  (mac-right-option-modifier 'none) ; Let me use right-option to enter symbols like []
  
  ;; UI Settings
  (visible-bell nil)
  (ring-bell-function 'ignore)
  (truncate-lines nil)

  ;; Fold/Unfold with <tab> when in outline-minor-mode
  (outline-minor-mode-cycle t)

  (scroll-conservatively 10)
  (scroll-margin 25)

  ;; Corfu
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)
  
  ;; Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not used via M-x.
  ;; This setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :init
  ;; UI tweaks
  (blink-cursor-mode -1)
  ;; Ugly toolbar in macOS window
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  
  ;; Enable context menu if in GUI
  (when (display-graphic-p)
    (context-menu-mode))

  ;; Auto-update buffer when underlying file has been changed
  (global-auto-revert-mode 1)
  ;; Line editing on visible, not logical, lines
  (global-visual-line-mode 1)

  ;; Pixel (not line) scrolling when using trackpad
  ;; BUG: does not play well with scroll-margin
  ;; I'd rather have chill line scrolling via cursor movement
  ;(pixel-scroll-precision-mode 1)

  ;; MINAD-stack configurations
  ;; Vertico
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t))

;; Turn on line numbers while code editing
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq-default mode-line-format
      '("%e" mode-line-front-space
	mode-line-frame-identification
	mode-line-buffer-identification "  "
	mode-line-position
	mode-line-format-right-align
	(project-mode-line project-mode-line-format) "  "
	mode-line-modes
	mode-line-misc-info
	mode-line-end-spaces))
;; Don't show any minor modes in the mode line
(setq minor-mode-alist nil)

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
    (evil-mode-line-format '(before . mode-line-modes))
    (evil-shift-width 2)
    :config
    (evil-mode)
    ;; Unmap keys in 'evil-maps
    ;; if not done, (setq org-return-follows-link t) will not work
    (evil-set-leader 'normal (kbd "SPC"))  ;; Set SPC as leader
    (define-key evil-normal-state-map (kbd "SPC SPC") 'execute-extended-command)
    (define-key evil-normal-state-map (kbd "SPC .") 'project-find-file)
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
  (org-archive-location "::* archive") ; Archive entries under the * archive tree
  ;; Properties are auto-added to archived items
  ;; I'm only interested in archived timestamp
  (org-archive-save-context-info '(time))
  (org-archive-reversed-order t) ; Archive to beginning of header instead of the end
  (org-reverse-note-order t) ; Refile to beginning on headers instead of the end
  (org-agenda-remove-tags t)
  (org-agenda-prefix-format '((todo . " %15c: ")))
  (org-todo-keywords '((sequence "TODO(t)"
				 "|"
				 "DONE(!)" "CANCELED(!)")))
  (org-priority-lowest 4)
  (org-priority-default 3)
  (org-priority-highest 1))

(let ((custom-file (expand-file-name "custom-org-capture-templates.el" user-emacs-directory)))
  (when (file-exists-p custom-file)
    (load custom-file)))

(defun my-org-mode-margins ()
  "Set tasteful margins when in Org mode"
  (setq left-margin-width 1
	right-margin-width 1)
  (set-fringe-mode 3))
(add-hook 'org-mode-hook 'my-org-mode-margins)

;;; Programming

;; Make sure emacs has the correct environment variables loaded
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Ruby completion, documentation, & navigation
(use-package robe
  :ensure t
  :defer t
  :hook
  (ruby-mode-hook . robe-mode)
  (ruby-ts-mode-hook . robe-mode))

(use-package prettier
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :custom
  (markdown-command "/opt/homebrew/bin/multimarkdown"))

;;; Completions

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

;; which-key: show keybindings in the minibuffer
(use-package which-key
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

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  ;; (corfu-popupinfo-mode) ; Shows a documentation pop-up in the window
  (corfu-echo-mode) ; Shows documentation in the echo area
  )

;;; Custom Bindings, Commands, and Functions

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(defun my/open-init-file ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") #'my/open-init-file)

(defun my/jekyll-insert-front-matter ()
  "Insert Jekyll front matter at point."
  (interactive)
  (insert (format "---
layout: post
title:  \"Title goes here\"
date:   %s 00:00:00 +0000
tags: []
---\n" (format-time-string "%Y-%m-%d"))))

(defun my/jekyll-insert-post-link (filename link-text)
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

(defun my/jekyll-insert-image (filename alt-text)
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

(defun my/add-org-items-to-shopping-reminders ()
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

(defun my/add-item-to-shopping-reminders (item)
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
