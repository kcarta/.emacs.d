(use-package prettier
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode))
  :init (setq markdown-command "/usr/local/bin/multimarkdown"))

(use-package lsp-dart
  :ensure t
  :hook (dart-mode . lsp))

(setq treesit-language-source-alist
      '((ruby "https://github.com/tree-sitter/tree-sitter-ruby")))

(setq major-mode-remap-alist
      '((ruby-mode . ruby-ts-mode)))
;	(js2-mode . js-ts-mode)
;	(typescript-mode . typescript-ts-mode)
;	(json-mode . json-ts-mode)
;	(css-mode . css-ts-mode)
;	(yaml-mode . yaml-ts-mode)
;	(dart-mode . dart-ts-mode)))

;(use-package robe
  ;:ensure t
  ;:init 
  ;(add-hook 'ruby-mode-hook 'robe-mode)
  ;(add-hook 'ruby-ts-mode-hook 'robe-mode))

(with-eval-after-load 'eglot
 (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

(provide 'lang)
