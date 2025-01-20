;; Backups
;; By default, Emacs creates automatic backups of files in their original directories, such "file.el" and the backup "file.el~".  This leads to a lot of clutter, so let's tell Emacs to put all backups that it creates in an archive directory.
(setq backup-directory-alist '((".*" . "~/.emacs.d/archive/")))
(setq auto-save-file-name-transforms
  `((".*" "~/.emacs.d/archive/" t)))

;; Set the Opt key on macOS to function as M-
(setq mac-option-key-is-meta t)
;; But free up the right option key for alt
(setq mac-right-option-modifier nil)

;; Right-click shows a context menu instead of doing whatever it did before
(when (display-graphic-p)
  (context-menu-mode))

;; Disable some app UI pieces I don't want
(blink-cursor-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Turn off line numbers in all modes (we can enable it in other modes)
(global-display-line-numbers-mode -1)
;; Turn on visual line mode, which soft-wraps at word boundaries
(global-visual-line-mode t)
;; Truncate long lines at the window edge
(setq truncate-lines nil)

;; Disable the bells
;; Disable the visible bell
(setq visible-bell nil)
;; Disable the audible bell
(setq ring-bell-function 'ignore)

;; Automatically refresh the buffer if the file has changed
(global-auto-revert-mode t)

;; Turns on automatic pairing
(electric-pair-mode 1)

;; Turn off automatic pairing for < in org-mode
(add-hook
  'org-mode-hook
  (lambda ()
    (setq-local electric-pair-inhibit-predicate
      `
      (lambda (c)
        (if (char-equal c ?<)
          t
          (,electric-pair-inhibit-predicate c))))))
(provide 'variables)
