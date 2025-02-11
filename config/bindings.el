(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key [escape] 'keyboard-escape-quit)

(use-package general
  :ensure t
  :config (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer kc/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (kc/leader-keys
    "SPC" '(execute-extended-command :wk "Consult")
    "." '(find-file :wk "Find file")
    "g" '(rg :wk "Ripgrep search"))

  (kc/leader-keys
    "a" '(:ignore t :wk "AI")
    "a s" '(gptel-send :wk "Send Prompt")
    "a f" '(gptel-add-file :wk "Add File to Context")
    "a c" '(gptel-add :wk "Add Region/Buffer to Context")
    "a q" '(gptel-abort :wk "Abort")
    )

  (kc/leader-keys
    "b" '(:ignore t :wk "Buffers")
    "b b" '(switch-to-buffer :wk "Switch to buffer")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers"))

  (kc/leader-keys
    "f" '(:ignore t :wk "Files")
    "f c" '(
	    (lambda ()
	      (interactive)
	      (find-file "~/.emacs.d/init.el")) :wk "Open emacs init.el")
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
    "h t" '(load-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))

  (kc/leader-keys
    "h m" '(:ignore t :wk "Manuals")
    "h m e" '((lambda () (interactive) (info "elisp")) :wk "Elisp Manual") ;; Example: "SPC hE" to open the Elisp manual
    )

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
      :wk "Eshell")
    "o F" '(select-frame-by-name :wk "Select frame by name"))

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
    "r b" '(:ignore t :wk "Tables")
    "r b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (kc/leader-keys
    "r d" '(:ignore t :wk "Date/deadline")
    "r d t" '(org-time-stamp :wk "Org time stamp"))

  (kc/leader-keys
    "t" '(:ignore t :wk "Timers")
    "t s" '(org-timer-start :wk "Start Timer")
    "t t" '(org-timer-stop :wk "Stop Timer")
    "t p" '(org-timer-set-timer :wk "Set Timer"))

  (kc/leader-keys
    "v" '(:ignore t :wk "View")
    "v l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "v t" '(visual-line-mode :wk "Toggle truncated lines"))

  (kc/leader-keys
    "w" '(:ignore t :wk "Windows")
    ;; Window sizing
    "w f" '(toggle-frame-fullscreen :wk "Enter/Exit fullscreen")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    ;; Window motions
    "w w" '(evil-window-next :wk "Next window")
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")))

(provide 'bindings)
