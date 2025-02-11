(use-package gptel :ensure t
  :config
  (setq
   gptel-model 'phi4:latest
   gptel-backend (gptel-make-ollama "Ollama"
		     :host "localhost:11434"
		     :stream t
		     :models '(phi4:latest)))
  )

(provide 'ai)
