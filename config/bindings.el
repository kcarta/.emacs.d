(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(global-set-key [escape] 'keyboard-escape-quit)

(use-package general
  :ensure t
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer kc/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode
  
  (kc/leader-keys
    "SPC" '(execute-extended-command :wk "Consult")
    "." '(find-file :wk "Find file")
    "TAB TAB" '(comment-line :wk "Comment lines")
    "u" '(universal-argument :wk "Universal argument"))

  (kc/leader-keys
    "b" '(:ignore t :wk "Buffers")
    "b b" '(switch-to-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    )

  (kc/leader-keys
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    )

  (kc/leader-keys
    "f" '(:ignore t :wk "Files and Feeds")    
    "f c" '((lambda () (interactive)
              (find-file "~/.emacs.d/init.el")) 
            :wk "Open emacs config.org")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f i" '((lambda () (interactive)
              (find-file "~/.emacs.d/init.el")) 
            :wk "Open emacs init.el")
    )
  
  (kc/leader-keys
    "g" '(:ignore t :wk "Go")
    "g a" '(org-open-at-point :wk "Open link under cursor")
    ;; These are copies of keybindings in spc w
    ;; But this is so deep in my muscle memory it's better than retraining
    ;; Window motions
    "g h" '(evil-window-left :wk "Window left")
    "g j" '(evil-window-down :wk "Window down")
    "g k" '(evil-window-up :wk "Window up")
    "g l" '(evil-window-right :wk "Window right")
    "g w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "g H" '(buf-move-left :wk "Buffer move left")
    "g J" '(buf-move-down :wk "Buffer move down")
    "g K" '(buf-move-up :wk "Buffer move up")
    "g L" '(buf-move-right :wk "Buffer move right")
    )

  (kc/leader-keys
    "h" '(:ignore t :wk "Help")
    "h a" '(counsel-apropos :wk "Apropos")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h i" '(info :wk "Info")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h r" '(:ignore t :wk "Reload")
    "h r r" '((lambda () (interactive)
		(load-file "~/.emacs.d/init.el")
		(ignore (elpaca-process-queues)))
              :wk "Reload emacs config")
    "h t" '(load-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))
  
  (kc/leader-keys
    "l" '(:ignore t :wk "Languages")    
    "l l" '(eglot :wk "Start Eglot")
    "l b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "l d" '(eval-defun :wk "Evaluate defun containing or after point")
    "l e" '(eval-expression :wk "Evaluate an elisp expression")
    "l r" '(eval-region :wk "Evaluate elisp in region")
    )
  
  (kc/leader-keys
    "o" '(:ignore t :wk "Open")
    "o d" '(dashboard-open :wk "Dashboard")
    "o f" '(make-frame :wk "Open buffer in new frame")
    "o s" '((lambda () (interactive)
	      (let ((height (truncate (* 0.3 (frame-height)))))
		(split-window-below (- height))
		(other-window 1)
		(eshell)))
	    :wk "Eshell")
    "o w" '(eww :wk "EWW emacs web wowser")
    "o F" '(select-frame-by-name :wk "Select frame by name"))

  (kc/leader-keys
    "r" '(:ignore t :wk "Org")
    "r a" '(org-archive-subtree :wk "Org archive (subtree)")
    "r r" '(org-refile :wk "Org refile")
    "r T" '(org-todo-list :wk "Org todo list"))

  (kc/leader-keys
    "r b" '(:ignore t :wk "Tables")
    "r b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (kc/leader-keys
    "r d" '(:ignore t :wk "Date/deadline")
    "r d t" '(org-time-stamp :wk "Org time stamp"))

  (kc/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t i" '(org-toggle-inline-images :wk "Toggle inline images in org mode")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(visual-line-mode :wk "Toggle truncated lines")
    )

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
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right")
    )
  )

(provide 'bindings)
