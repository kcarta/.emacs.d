(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH"))

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-ansi-term))

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :config (eshell-syntax-highlighting-global-mode +1))

(setq
  eshell-rc-script (concat user-emacs-directory "eshell/profile")
  eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
  eshell-history-size 5000
  eshell-buffer-maximum-lines 5000
  eshell-hist-ignoredups t
  eshell-scroll-to-bottom-on-input t
  eshell-destroy-buffer-when-process-dies t
  eshell-visual-commands' ("bash" "htop" "ssh" "top" "zsh"))

(provide 'shells)
