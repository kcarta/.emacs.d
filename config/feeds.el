(use-package elfeed :ensure t
  :config
  (setq elfeed-search-feed-face ":foreground #ffffff :weight bold"
	elfeed-search-filter "@2-weeks-ago +unread"
	)
  )

(use-package elfeed-org
  :ensure t
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/feed-config.org"))
  (elfeed-org))

(use-package elfeed-goodies
  :ensure t
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-position 'top)
  (setq elfeed-goodies/entry-pane-size 0.75))

(use-package elfeed-tube
  :ensure t
  :after elfeed
  :demand t
  :config
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)
              :map elfeed-search-mode-map
              ("F" . elfeed-tube-fetch)
              ([remap save-buffer] . elfeed-tube-save)))

(use-package elfeed-tube-mpv
  :ensure t
  :bind (:map elfeed-show-mode-map
              ("C-c C-f" . elfeed-tube-mpv-follow-mode)
              ("C-c C-w" . elfeed-tube-mpv-where)))

(provide 'feeds)
