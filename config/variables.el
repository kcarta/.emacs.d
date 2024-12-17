;; Calendar location
;; Set to CPH (close enough)
(setq calendar-latitude 55.6)
(setq calendar-longitude 12.5)
(setq calendar-location-name "Copenhagen, DK")

;; Backups
;; By default, Emacs creates automatic backups of files in their original directories, such "file.el" and the backup "file.el~".  This leads to a lot of clutter, so let's tell Emacs to put all backups that it creates in an archive directory.
(setq backup-directory-alist '((".*" . "~/.emacs.d/archive/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/archive/" t)))

;; Disable the direct option modifier, freeing up the macOS option key
(if (boundp 'ns-option-modifier)
    (setq ns-option-modifier nil))

;; Right-click shows a context menu instead of doing whatever it did before
(when (display-graphic-p)
  (context-menu-mode))

(blink-cursor-mode -1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode -1)
(global-visual-line-mode t)

;; Disable the bells
;; Disable the visible bell
(setq visible-bell nil)
;; Disable the audible bell
(setq ring-bell-function 'ignore)

(setq truncate-lines nil)
(global-auto-revert-mode t)  ;; Automatically show changes if the file has changed
(setq org-edit-src-content-indentation 0) ;; Set src block automatic indent to 0 instead of 2.

(electric-indent-mode 1)    ;; Turn off the weird indenting that Emacs does by default.
(electric-pair-mode 1)       ;; Turns on automatic parens pairing
(add-hook 'org-mode-hook (lambda ()
           (setq-local electric-pair-inhibit-predicate
                   `(lambda (c)
                  (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
(provide 'variables)
