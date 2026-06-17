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
        system,
        ...
      }:
      {
        imports = [
          inputs.zen-browser.homeModules.beta
        ];
        programs.zen-browser.enable = true;

        programs.zen-browser.policies =
          let
            mkExtensionSettings = builtins.mapAttrs (
              _: pluginId: {
                install_url = "https://addons.mozilla.org/firefox/downloads/latest/${pluginId}/latest.xpi";
                installation_mode = "force_installed";
              }
            );
            mkLockedAttrs = builtins.mapAttrs (
              _: value: {
                Value = value;
                Status = "locked";
              }
            );
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
            ExtensionSettings = mkExtensionSettings {
              "{85860b32-02a8-431a-b2b1-40fbd64c9c69}" = "github-file-icons" ;
              "{446900e4-71c2-419f-a6a7-df9c091e268b}" = "bitwarden-password-manager" ;
              "{52bda3fd-dc48-4b3d-a7b9-58af57879f1e}" = "stylebot-web" ;
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
          spaces = {
            "Music" = {
              id = "4d929899-3c7c-44e3-be00-e1e850836b6f";
              icon = "🎸";
              position = 1000;
              theme = {
                type = "gradient";
                colors = [
                  {
                    algorithm = "floating";
                    type = "explicit-lightness";
                    red = 107;
                    green = 126;
                    blue = 148;
                    lightness = 50;
                    position = {
                      x = 51;
                      y = 97;
                    };
                  }
                ];
                opacity = 0.5;
              };
            };
            "Work" = {
              id = "1aa8cdd7-cf7b-4523-a2aa-20d3f085dfd3";
              icon = "⌘";
              position = 500;
              theme = {
                type = "gradient";
                colors = [
                  {
                    algorithm = "floating";
                    type = "explicit-lightness";
                    red = 84;
                    green = 140;
                    blue = 171;
                    lightness = 50;
                    position = {
                      x = 68;
                      y = 137;
                    };
                  }
                ];
                opacity = 0.5;
              };
            };

          };

          pinsForce = true;
          pins = {
            "GCal" = {
              id = "336445e0-1f54-45cb-a83a-83d0d59f4d74";
              workspace = spaces.Work.id;
              url = "https://calendar.google.com";
              isEssential = true;
            };
            "Gather v1" = {
              id = "097764b9-eae6-4961-8a43-005616d80de4";
              workspace = spaces.Work.id;
              url = "https://app.gather.town/app/oiZp6OnQZKD7RX51/Wooting%20World";
              isEssential = true;
            };
            "Gather v2" = {
              id = "31d894db-1f22-4007-9d0d-b3fa874dd69b";
              workspace = spaces.Work.id;
              url = "https://app.v2.gather.town/app/wooting-officehev2-bfb336c4-e33b-4fc5-aeeb-f5bbc2e4f910";
              isEssential = true;
            };
            "Notion" = {
              id = "40c7b93b-fb81-40ba-b9ce-0241f444cb07";
              workspace = spaces.Work.id;
              url = "https://www.notion.so/60eed1146a9c4240a7b0bd25d68fc240";
              isEssential = false;
            };
            "Uptime Robot" = {
              id = "d8d610e5-dc09-4c52-b6e2-d663635069ec";
              workspace = spaces.Work.id;
              url = "https://dashboard.uptimerobot.com/login?rt=false";
              isEssential = false;
            };
            "Digitalocean" = {
              id = "134b8cbd-f9b0-4b9e-9c5a-a0e5feaade56";
              workspace = spaces.Work.id;
              url = "https://cloud.digitalocean.com/apps";
              isEssential = true;
            };
            "Honeycomb" = {
              id = "379437a9-0674-4b0e-8b1a-064c5ff91e2d";
              workspace = spaces.Work.id;
              url = "https://ui.honeycomb.io/wooting/environments/production/datasets/wooting-server/home?tab=traces";
              isEssential = true;
            };
            "Temporal" = {
              id = "7174abc7-94fa-41b7-8694-aed50668d5dd";
              workspace = spaces.Work.id;
              url = "https://temporal-ui.tail4b94e5.ts.net/";
              isEssential = false;
            };
            "Graphite" = {
              id = "9418efa6-8723-42e8-b745-e7401eae14a2";
              workspace = spaces.Work.id;
              url = "https://app.graphite.com/#needs-your-review";
              isEssential = true;
            };
            "JDSLabs" = {
              id = "747ea73e-8323-452d-baef-55b4a8624fd6";
              workspace = spaces.Music.id;
              url = "https://core.jdslabs.com/";
              isEssential = false;
            };
            "k3s" = {
              id = "0a1665e6-4d85-4c96-81f2-2b77c8d73e56";
              workspace = spaces.Work.id;
              url = "https://wt-k3s.tail4b94e5.ts.net/favorites";
              isEssential = false;
            };
            "Tailscale" = {
              id = "6eb662ba-0a57-4f41-9cb1-f9381c464ec9";
              workspace = spaces.Work.id;
              url = "https://login.tailscale.com/admin/machines";
              isEssential = false;
            };
            "Linear" = {
              id = "58a13bf2-34bf-4631-a3ee-45e6a2ddeecb";
              workspace = spaces.Work.id;
              url = "https://linear.app";
              isEssential = false;
            };
          };

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
