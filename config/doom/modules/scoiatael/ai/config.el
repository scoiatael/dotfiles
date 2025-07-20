;;; modules/scoiatael/ai/config.el -*- lexical-binding: t; -*-

(use-package! gptel
  :config
  (gptel-make-openai "TogetherAI"         ;Any name you want
    :host "api.together.xyz"
    :key (lambda () (password-store-get "together-ai-api-token"))
    :stream t
    :models '(;; has many more, check together.ai
              deepseek-ai/DeepSeek-R1
              deepseek-ai/DeepSeek-V3
              mistralai/Mixtral-8x7B-Instruct-v0.1
              codellama/CodeLlama-13b-Instruct-hf
              codellama/CodeLlama-34b-Instruct-hf))
  (gptel-make-kagi "Kagi"
    :key (lambda () (password-store-get "kagi-api-token")))

  (setq
   gptel-model 'claude-3-7-sonnet-20250219
   gptel-directives '((assistant . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
                      (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
                      (default . "You are a large language model and a careful programmer living inside Emacs editor. Provide code and only code as output without any additional text, prompt or note. Look at last line for additional requests from user.")
                      (writing . "You are a large language model and a writing assistant. Respond concisely.")
                      (chat . "You are a large language model and a conversation partner. Respond concisely."))
   gptel-backend (gptel-make-anthropic "Claude"          ;Any name you want
                   :stream t                             ;Streaming responses
                   :key (lambda () (password-store-get "anthropic-com-api-token"))))

  (map! :localleader
        "g g" #'gptel
        "g r" #'gptel-rewrite
        "g s" #'gptel-send
        "g m" #'gptel-menu))