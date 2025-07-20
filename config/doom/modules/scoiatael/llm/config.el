;;; modules/scoiatael/llm/config.el -*- lexical-binding: t; -*-

(use-package! ellama
  :init
  (setopt ellama-language "German")
  (require 'llm-ollama)
  (setopt ellama-provider
          ;; if provider doesn't work download it via `ollama run <model>` first :)
          ;; otherwise requests fail with 404.
          ;; FIXME: maybe I should port fix upstream?
	  (make-llm-ollama
	   :chat-model "zephyr:7b-alpha-q5_K_M" :embedding-model "zephyr:7b-alpha-q5_K_M")))