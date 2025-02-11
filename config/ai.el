(use-package gptel
  :ensure t
  :defer t
  :custom
  (gptel-model 'phi4:latest)
  :init
  (setq gptel-backend (gptel-make-ollama "Ollama"
			:host "localhost:11434"
			:stream t
			:models '(phi4:latest))))
(provide 'ai)
