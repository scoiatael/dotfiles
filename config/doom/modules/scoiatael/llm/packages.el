;;; modules/scoiatael/llm/packages.el -*- lexical-binding: t; -*-

(when (modulep! +agent)
  (package! acp :recipe '(:host github :repo "xenodium/acp.el" :files ("acp*.el")) :pin "4d7d58dc39870e9390e94617e13d7ada175d7945")
  (package! agent-shell :recipe '(:host github :repo "xenodium/agent-shell" :files ("agent-shell*.el")) :pin "833b2a8031a22068c0528a1f7600926b7359154a"))
