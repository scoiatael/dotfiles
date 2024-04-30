(require-macros :lib.macros)
(require-macros :lib.advice.macros)
(local windows (require :windows))
(local emacs (require :emacs))
(local slack (require :slack))
(local vim (require :vim))

(local {:concat concat
       :logf logf} (require :lib.functional))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn activator
    [app-name]
    "
    A higher order function to activate a target app. It's useful for quickly
    binding a modal menu action or hotkey action to launch or focus on an app.
    Takes a string application name
    Returns a function to activate that app.

    Example:
    (local launch-emacs (activator \"Emacs\"))
    (launch-emacs)
    "
    (fn activate []
       (hs.application.launchOrFocus app-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; If you would like to customize this we recommend copying this file to
;; ~/.spacehammer/config.fnl. That will be used in place of the default
;; and will not be overwritten by upstream changes when spacehammer is updated.
(local music-app "Music")

(local return
       {:key :space
       :title "Back"
       :action :previous})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(local window-move-screens
       [{:key "n, p"
       :title "Move next\\previous screen"}
       {:mods [:shift]
       :key "n, p"
       :title "Move up\\down screens"}
       {:key :n
       :action "windows:move-south"
       :repeatable true}
       {:key :p
       :action "windows:move-north"
       :repeatable true}
       {:mods [:shift]
       :key :n
       :action "windows:move-west"
       :repeatable true}
       {:mods [:shift]
       :key :p
       :action "windows:move-east"
       :repeatable true}])

(local window-bindings
       (concat
        [return
        {:key :w
        :title "Last Window"
        :action "windows:jump-to-last-window"}]
        window-move-screens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Apps Menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local emacs-bindings
       [return
       {:key :c
       :title "Capture"
       :action (fn [] (emacs.capture))}
       {:key :z
       :title "Note"
       :action (fn [] (emacs.note))}
       {:key :v
       :title "Split"
       :action "emacs:vertical-split-with-emacs"}
       {:key :f
       :title "Full Screen"
       :action "emacs:full-screen"}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Menu & Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local menu-items
       [{:key    :space
       :title  "Raycast"
       :action (activator "Raycast")}
       {:key   :w
       :title "Window"
       :enter "windows:enter-window-menu"
       :exit "windows:exit-window-menu"
       :items window-bindings}
       {:key :e
       :title "Emacs"
       :action (activator "Emacs")}
       {:key :g
       :title "Arc"
       :action (activator "Arc")}
       {:key :t
       :title "Term"
       :action (activator "Rio")}
       {:key :s
       :title "Slack"
       :action (activator "Slack")}])

(local common-keys
       [{:mods [:cmd]
       :key :d
       :action "lib.modal:activate-modal"}
       {:mods [:cmd :ctrl]
       :key "`"
       :action hs.toggleConsole}
       {:mods [:cmd :ctrl]
       :key :o
       :action "emacs:edit-with-emacs"}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; App Specific Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(local browser-keys
       [{:mods [:cmd :shift]
       :key :l
       :action "chrome:open-location"}
       {:mods [:alt]
       :key :k
       :action "chrome:next-tab"
       :repeat true}
       {:mods [:alt]
       :key :j
       :action "chrome:prev-tab"
       :repeat true}])

(local browser-items
       (concat
        menu-items
        [{:key "'"
        :title "Edit with Emacs"
        :action "emacs:edit-with-emacs"}]))

(local brave-config
       {:key "Brave Browser"
       :keys browser-keys
       :items browser-items})

(local chrome-config
       {:key "Google Chrome"
       :keys browser-keys
       :items browser-items})

(local firefox-config
       {:key "Firefox"
       :keys browser-keys
       :items browser-items})

(local emacs-config
       {:key "Emacs"
       :activate (fn [] (vim.disable))
       :deactivate (fn [] (vim.enable))
       :launch "emacs:maximize"
       :items []
       :keys []})

(local grammarly-config
       {:key "Grammarly"
       :items (concat
               menu-items
               [{:mods [:ctrl]
               :key :c
               :title "Return to Emacs"
               :action "grammarly:back-to-emacs"}])
       :keys ""})

(local hammerspoon-config
       {:key "Hammerspoon"
       :items (concat
               menu-items
               [{:key :r
               :title "Reload Console"
               :action hs.reload}
               {:key :c
               :title "Clear Console"
               :action hs.console.clearConsole}])
       :keys []})

(local slack-config
       {:key "Slack"
       :keys [{:mods [:cmd]
       :key  :g
       :action "slack:scroll-to-bottom"}
       {:mods [:ctrl]
       :key :r
       :action "slack:add-reaction"}
       {:mods [:ctrl]
       :key :h
       :action "slack:prev-element"}
       {:mods [:ctrl]
       :key :l
       :action "slack:next-element"}
       {:mods [:ctrl]
       :key :t
       :action "slack:thread"}
       {:mods [:ctrl]
       :key :p
       :action "slack:prev-day"}
       {:mods [:ctrl]
       :key :n
       :action "slack:next-day"}
       {:mods [:ctrl]
       :key :e
       :action "slack:scroll-up"
       :repeat true}
       {:mods [:ctrl]
       :key :y
       :action "slack:scroll-down"
       :repeat true}
       {:mods [:ctrl]
       :key :i
       :action "slack:next-history"
       :repeat true}
       {:mods [:ctrl]
       :key :o
       :action "slack:prev-history"
       :repeat true}
       {:mods [:ctrl]
       :key :j
       :action "slack:down"
       :repeat true}
       {:mods [:ctrl]
       :key :k
       :action "slack:up"
       :repeat true}]})

(local apps
       [grammarly-config
       hammerspoon-config
       slack-config])

(local config
       {:title "Main Menu"
       :items menu-items
       :keys  common-keys
       :enter (fn [] (windows.hide-display-numbers))
       :exit  (fn [] (windows.hide-display-numbers))
       :apps  apps
       :hyper {:key :F18}
       :modules {:windows {:center-ratio "80:50"}}})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My custom stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require :my-lua-config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

config
