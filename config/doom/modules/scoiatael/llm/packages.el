;;; modules/scoiatael/llm/packages.el -*- lexical-binding: t; -*-

(when (modulep! +gptel)
  (package! gptel :pin "688fcb088b14aafb08bec57263623390d7dc6728"))
(when (modulep! +gptel +macher)
  (package! macher :recipe (:host github :repo "kmontag/macher") :pin "4fa8fbb6b250b207723d380931a463bcbc8da9ca"))
(when (modulep! +claude)
  (package! claude-code-ide :recipe (:host github :repo "manzaltu/claude-code-ide.el") :pin "32d853e20b9d245a6ee89c4a153a4e568250c62c"))
(when (modulep! +shell)
  (package! shell-maker :recipe (:type git :host github :repo "xenodium/shell-maker"))
  (package! chatgpt-shell :recipe (:host github :repo "xenodium/chatgpt-shell"  :files ("chatgpt-shell*.el")) :pin "32bf4bf930d7226bc9df876805b80a936813efe5"))
