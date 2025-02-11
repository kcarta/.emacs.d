(use-package idle-highlight-mode
  :ensure t
  :config (setq idle-highlight-idle-time 0.2)
  :hook ((prog-mode text-mode) . idle-highlight-mode))

(set-face-attribute 'default nil
  :font "Fira Code"
  :height 160
  :weight 'regular)

;; Define a variable-pitch face that uses Georgia
(set-face-attribute 'variable-pitch nil :family "Georgia" :height 1.2)

;; Create a hook for markdown-mode to use the variable-pitch face
(add-hook 'markdown-mode-hook
  (lambda ()
    (variable-pitch-mode 1)
    (setq buffer-face-mode-face 'variable-pitch)
    (buffer-face-mode 1)
    (setq-local line-spacing 0.4)))

(use-package all-the-icons :ensure t :if (display-graphic-p))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-flavor 'mocha)
  (catppuccin-reload))

;; Window rules
(setq display-buffer-alist
  '
  (
    ("\\*messages.*"
      (display-buffer-in-side-window)
      (window-height . 0.15)
      (side . bottom))
    ("\\*backtrace.*"
      (display-buffer-in-side-window)
      (window-height . 0.15)
      (side . bottom))
    ("\\*warnings.*"
      (display-buffer-in-side-window)
      (window-height . 0.15)
      (side . bottom))))

;; Highlight delimiters like parenthesis 
(use-package rainbow-delimiters
  :ensure t
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
    (clojure-mode . rainbow-delimiters-mode)))

;; Remember to run M-x nerd-icons-install-fonts to install necessary fonts.
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq
    doom-modeline-height 25 ; sets modeline height
    doom-modeline-bar-width 5 ; sets right bar width
    doom-modeline-buffer-encoding nil ; sets buffer encoding
    doom-modeline-enable-word-count t ; sets the word count dislay
    doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode) ; sets the modes where the word count is made
    doom-modeline-project-name t ; shows project name 
    doom-modeline-modal nil ; sets icon for evil mode modal state
    ))

(provide 'themes)
