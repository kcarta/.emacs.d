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

(provide 'shells)
