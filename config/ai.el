(use-package gptel :ensure t
  :config
  (setq
   gptel-model 'mistral-small ;'phi4:latest
   gptel-backend (gptel-make-ollama "Ollama"
		     :host "localhost:11434"
		     :stream t
		     ;:models '(phi4:latest)))
		     :models '(mistral-small)))
  )

(provide 'ai)
