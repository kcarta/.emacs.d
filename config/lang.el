;; Turn on line numbers while code editing
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package prettier :ensure t)

(use-package json-mode :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

(use-package lsp-dart :ensure t :hook (dart-mode . lsp))

(defun jekyll-insert-top-matter ()
  "Insert Jekyll top matter at point."
  (interactive)
  (let ((current-date (format-time-string "%Y-%m-%d")))
    (insert
      (format
        "---
layout: post
title:  \"Title goes here\"
date:   %s 00:00:00 +0000
tags: []
---\n"
        current-date))))

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

(provide 'lang)
