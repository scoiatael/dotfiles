(configuration-layer/declare-layers '(
                                      colors
                                      evil-commentary
                                      helm
                                      (auto-completion :variables
                                                       auto-completion-return-key-behavior 'complete
                                                       auto-completion-tab-key-behavior 'cycle
                                                       auto-completion-complete-with-key-sequence "jk"
                                                       auto-completion-enable-help-tooltip t
                                                       auto-completion-enable-sort-by-usage t
                                                       auto-completion-enable-snippets-in-popup t)
                                      better-defaults
                                      ))
