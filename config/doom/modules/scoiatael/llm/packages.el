;;; modules/scoiatael/llm/packages.el -*- lexical-binding: t; -*-

(when (modulep! +macher)
  (package! macher :recipe '(:host github :repo "kmontag/macher") :pin "4fa8fbb6b250b207723d380931a463bcbc8da9ca"))
(when (modulep! +claude)
  (package! claude-code-ide :recipe '(:host github :repo "manzaltu/claude-code-ide.el") :pin "32d853e20b9d245a6ee89c4a153a4e568250c62c"))
(when (or (modulep! +shell) (modulep! +agent))
  (package! shell-maker :recipe '(:type git :host github :repo "xenodium/shell-maker")))
(when (modulep! +shell)
  (package! chatgpt-shell :recipe '(:host github :repo "xenodium/chatgpt-shell"  :files ("chatgpt-shell*.el")) :pin "32bf4bf930d7226bc9df876805b80a936813efe5"))
(when (modulep! +agent)
  (package! acp :recipe '(:host github :repo "xenodium/acp.el" :files ("acp*.el")) :pin "4d7d58dc39870e9390e94617e13d7ada175d7945")
  (package! agent-shell :recipe '(:host github :repo "xenodium/agent-shell" :files ("agent-shell*.el")) :pin "833b2a8031a22068c0528a1f7600926b7359154a"))
(when (modulep! +tools)
  (package! llm-tool-collection :recipe '(:host github :repo "skissue/llm-tool-collection")) :pin "a383ccf3df6c86684da77fb61ea4ebe67a21eedb")

(when (modulep! +amp)
  (package! amp
    :type 'local
    :recipe '(:local-repo "../../../packages")))
