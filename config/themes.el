(use-package idle-highlight-mode
  :ensure t
  :config (setq idle-highlight-idle-time 0.2)
  :hook ((prog-mode text-mode) . idle-highlight-mode))

(set-face-attribute 'default nil
  :font "Fira Code"
  :height 160
  :weight 'regular)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; Define a variable-pitch face that uses Georgia
(set-face-attribute 'variable-pitch nil :family "Georgia" :height 1.2)

;; Create a hook for markdown-mode to use variable-pitch face
(add-hook 'markdown-mode-hook
          (lambda ()
            (variable-pitch-mode 1)
            (setq buffer-face-mode-face 'variable-pitch)
            (buffer-face-mode 1)
	    (setq-local line-spacing 0.4)))

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "Fira Code-16"))

(use-package all-the-icons
    :ensure t
    :if (display-graphic-p))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  (use-package all-the-icons-dired
    :ensure t
    :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (let ((hour (string-to-number (format-time-string "%H"))))
    (if (and (>= hour 6) (< hour 18))
        ;;(load-theme 'doom-bluloco-light t)
        (load-theme 'doom-bluloco-dark t) ; baby life requires perma-dark mode
      (load-theme 'doom-bluloco-dark t)))
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Window rules
(setq display-buffer-alist
      '(("\\*messages.*"
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

(use-package rainbow-delimiters
  :ensure t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)))

;; Remember to run M-x nerd-icons-install-fonts to install necessary fonts.
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25      ; sets modeline height
        doom-modeline-bar-width 5    ; sets right bar width
        doom-modeline-buffer-encoding nil  ; sets buffer encoding
	doom-modeline-enable-word-count t ; sets the word count dislay
	doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode) ; sets the modes where the word count is made
	doom-modeline-project-name t ; shows project name 
	doom-modeline-modal nil ; sets icon for evil mode modal state
    )
) ;; adds folder icon next to persp name

(provide 'themes)

