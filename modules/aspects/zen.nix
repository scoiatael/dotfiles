{ den, inputs, ... }:

{
  flake-file.inputs.zen-browser.url = "github:0xc000022070/zen-browser-flake";
  den.aspects.zen = {
    includes = [ den.aspects.stylix ];
    homeManager =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      {
        imports = [
          inputs.zen-browser.homeModules.beta
        ];
        programs.zen-browser.enable = true;

        programs.zen-browser.policies =
          let
            mkLockedAttrs = import ./_zen/mkLockedAttrs.nix;
          in
          {
            AutofillAddressEnabled = true;
            AutofillCreditCardEnabled = false;
            DisableAppUpdate = true;
            DisableFeedbackCommands = true;
            DisableFirefoxStudies = true;
            DisablePocket = true;
            DisableTelemetry = true;
            DontCheckDefaultBrowser = true;
            NoDefaultBookmarks = true;
            OfferToSaveLogins = false;
            EnableTrackingProtection = {
              Value = true;
              Locked = true;
              Cryptomining = true;
              Fingerprinting = true;
            };
            SanitizeOnShutdown = {
              FormData = true;
              Cache = true;
            };
            Preferences = mkLockedAttrs {
              "browser.aboutConfig.showWarning" = false;
              "browser.tabs.warnOnClose" = false;
              "media.videocontrols.picture-in-picture.video-toggle.enabled" = true;
              # Disable swipe gestures (Browser:BackOrBackDuplicate, Browser:ForwardOrForwardDuplicate)
              "browser.gesture.swipe.left" = "";
              "browser.gesture.swipe.right" = "";
              "browser.tabs.hoverPreview.enabled" = true;
              "browser.newtabpage.activity-stream.feeds.topsites" = false;
              "browser.topsites.contile.enabled" = false;

              "privacy.resistFingerprinting" = true;
              "privacy.resistFingerprinting.randomization.canvas.use_siphash" = true;
              "privacy.resistFingerprinting.randomization.daily_reset.enabled" = true;
              "privacy.resistFingerprinting.randomization.daily_reset.private.enabled" = true;
              "privacy.resistFingerprinting.block_mozAddonManager" = true;
              "privacy.spoof_english" = 1;

              "privacy.firstparty.isolate" = true;
              "network.cookie.cookieBehavior" = 5;
              "dom.battery.enabled" = false;

              "gfx.webrender.all" = true;
              "network.http.http3.enabled" = true;
              "network.socket.ip_addr_any.disabled" = true; # disallow bind to 0.0.0.0
            };
          };

        # Use legacy profile mode to avoid needing machine-specific Install identifier
        home.sessionVariables.MOZ_LEGACY_PROFILES = "1";

        stylix.targets.zen-browser.profileNames = [ config.home.username ];
        programs.zen-browser.profiles.${config.home.username} = rec {
          id = 0; # Profile IDs must be sequential starting from 0
          settings = {
            "zen.workspaces.continue-where-left-off" = true;
            "zen.workspaces.natural-scroll" = true;
            "zen.view.compact.hide-tabbar" = true;
            "zen.view.compact.hide-toolbar" = true;
            "zen.view.compact.animate-sidebar" = true;
            "zen.welcome-screen.seen" = true;
            "zen.urlbar.behavior" = "float";
            "zen.view.use-single-toolbar" = false;
          };

          # Optional: Pin to a specific shortcuts version to detect breaking changes
          # Find the current version at about:config -> zen.keyboard.shortcuts.version
          # If Zen Browser updates and changes shortcuts, activation will fail with a clear error
          keyboardShortcutsVersion = 19;
          keyboardShortcuts = [
            {
              id = "zen-compact-mode-toggle";
              key = "s";
              modifiers.accel = true;
            }
            {
              id = "zen-compact-mode-show-sidebar"; # toggle floating sideboar
              key = "f";
              modifiers.shift = true;
              modifiers.accel = true;
            }
          ];

          spacesForce = true;
          pinsForce = true;

          search = {
            force = true;
            default = "Kagi";
            engines =
              let
                nixSnowflakeIcon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              in
              {
                "Nix Packages" = {
                  urls = [
                    {
                      template = "https://search.nixos.org/packages";
                      params = [
                        {
                          name = "type";
                          value = "packages";
                        }
                        {
                          name = "channel";
                          value = "unstable";
                        }
                        {
                          name = "query";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  icon = nixSnowflakeIcon;
                  definedAliases = [ "p" ];
                };

                "Nix Options" = {
                  urls = [
                    {
                      template = "https://search.nixos.org/options";
                      params = [
                        {
                          name = "channel";
                          value = "unstable";
                        }
                        {
                          name = "query";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  icon = nixSnowflakeIcon;
                  definedAliases = [ "o" ];
                };

                "Home Manager Options" = {
                  urls = [
                    {
                      template = "https://home-manager-options.extranix.com/";
                      params = [
                        {
                          name = "query";
                          value = "{searchTerms}";
                        }
                        {
                          name = "release";
                          value = "master";
                        }
                      ];
                    }
                  ];
                  icon = nixSnowflakeIcon;
                  definedAliases = [ "hm" ];
                };

                "Kagi" = {
                  urls = [
                    {
                      template = "https://kagi.com/search";
                      params = [
                        {
                          name = "q";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = [
                  ];
                };

                "Hoogle" = {
                  urls = [
                    {
                      template = "https://hoogle.haskell.org/";
                      params = [
                        {
                          name = "hoogle";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = [ "h" ];
                };

                "Noogle" = {
                  urls = [
                    {
                      template = "https://noogle.dev/q/";
                      params = [
                        {
                          name = "term";
                          value = "{searchTerms}";
                        }
                      ];
                    }
                  ];
                  definedAliases = [ "n" ];
                };
              };
          };
          mods = [
            "e122b5d9-d385-4bf8-9971-e137809097d0" # No Top Sites
            "253a3a74-0cc4-47b7-8b82-996a64f030d5" # Floating History
            "4ab93b88-151c-451b-a1b7-a1e0e28fa7f8" # No Sidebar Scrollbar
            "7190e4e9-bead-4b40-8f57-95d852ddc941" # Tab title fixes
            "803c7895-b39b-458e-84f8-a521f4d7a064" # Hide Inactive Workspaces
            "906c6915-5677-48ff-9bfc-096a02a72379" # Floating Status Bar
          ];
        };

        # Open files with the browser
        xdg.mimeApps =
          let
            associations = builtins.listToAttrs (
              map
                (name: {
                  inherit name;
                  value = "zen-beta.desktop";
                })
                [
                  "application/x-extension-shtml"
                  "application/x-extension-xhtml"
                  "application/x-extension-html"
                  "application/x-extension-xht"
                  "application/x-extension-htm"
                  "x-scheme-handler/unknown"
                  "x-scheme-handler/mailto"
                  "x-scheme-handler/chrome"
                  "x-scheme-handler/about"
                  "x-scheme-handler/https"
                  "x-scheme-handler/http"
                  "application/xhtml+xml"
                  "application/json"
                  "text/plain"
                  "text/html"
                ]
            );
          in
          lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
            enable = true;
            associations.added = associations;
            defaultApplications = associations;
          };
      };
  };
}
