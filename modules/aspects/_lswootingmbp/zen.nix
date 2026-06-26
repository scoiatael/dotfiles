{ config, ... }:

{
  programs.zen-browser.policies =
    let
      mkExtensionSettings = import ../_zen/mkExtensionSettings.nix;
    in
    {
      ExtensionSettings = mkExtensionSettings {
        "{85860b32-02a8-431a-b2b1-40fbd64c9c69}" = "github-file-icons";
        "{446900e4-71c2-419f-a6a7-df9c091e268b}" = "bitwarden-password-manager";
        "{52bda3fd-dc48-4b3d-a7b9-58af57879f1e}" = "stylebot-web";
      };
    };

  programs.zen-browser.profiles.${config.home.username} = rec {
    spaces = {
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
  };
}
