(defun jekyll-insert-front-matter ()
  "Insert Jekyll front matter at point."
  (interactive)
  (insert (format "---
layout: post
title:  \"Title goes here\"
date:   %s 00:00:00 +0000
tags: []
---\n" (format-time-string "%Y-%m-%d"))))

(defun jekyll-insert-post-link (filename link-text)
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

(defun jekyll-insert-image (filename alt-text)
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
(defun my-add-org-items-to-shopping-reminders ()
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

(defun my-add-item-to-shopping-reminders (item)
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

(provide 'custom-commands)
