{ config, ... }:

{
  programs.zen-browser.policies =
    let
      mkExtensionSettings = import ../_zen/mkExtensionSettings.nix;
    in
    {
      ExtensionSettings = mkExtensionSettings {
        "78272b6fa58f4a1abaac99321d503a20@proton.me" = "proton-pass";
      };
    };

  programs.zen-browser.profiles.default = rec {
    spaces = {
      "default" = {
        id = "55ca478f-638d-47e2-a723-18a0e9978ba4";
      };
    };

    pins = {
      "Booksy" = {
        id = "b4d8f202-9830-4abe-b1dc-ae3255caa202";
        workspace = spaces.default.id;
        url = "https://booksy.com/pl-pl/";
        isEssential = false;
      };
      "Dashy" = {
        id = "a81f8ecb-0965-4f9a-b318-c5f51e9c3070";
        workspace = spaces.default.id;
        url = "https://dashy.heron-pollux.ts.net/";
        isEssential = false;
      };
      "Home - Warhammer Community" = {
        id = "8009241a-62f5-4f01-af85-fc303bc92372";
        workspace = spaces.default.id;
        url = "https://www.warhammer-community.com/en-gb/";
        isEssential = true;
      };
      "Parrhasius" = {
        id = "b542c05c-7079-4b0f-8808-2efbba371fcb";
        workspace = spaces.default.id;
        url = "https://parrhasius.heron-pollux.ts.net/";
        isEssential = false;
      };
      "yarr!" = {
        id = "61c3a893-6954-47e4-99ee-485d3766aa69";
        workspace = spaces.default.id;
        url = "https://yarr.heron-pollux.ts.net/";
        isEssential = true;
      };
      "Machines - Tailscale" = {
        id = "017ebd94-a415-4cdb-a5dd-14e9b41eb69e";
        workspace = spaces.default.id;
        url = "https://login.tailscale.com/admin/machines";
        isEssential = false;
      };
      "Homepage, Most Recent | GamersNexus" = {
        id = "80eb9659-d4a1-46d5-a494-1f800376387f";
        workspace = spaces.default.id;
        url = "https://gamersnexus.net/";
        isEssential = false;
      };
      "Deluge WebUI" = {
        id = "66c07dfb-a60d-4ff1-95ae-91f77739f69e";
        workspace = spaces.default.id;
        url = "https://deluge.heron-pollux.ts.net/";
        isEssential = false;
      };
      "Jellyfin" = {
        id = "ff7cc80e-c848-4ed1-8377-b7ec4e503f86";
        workspace = spaces.default.id;
        url = "https://jellyfin.heron-pollux.ts.net/";
        isEssential = true;
      };
      "Grafana" = {
        id = "40c8ba01-9693-48ea-b6be-64a0cb4c6047";
        workspace = spaces.default.id;
        url = "https://grafana.heron-pollux.ts.net/";
        isEssential = false;
      };
      "The Big Debate – What colour should you paint your base rims?" = {
        id = "6e29d46d-1ff6-4a92-b3ea-5d5707d79a34";
        workspace = spaces.default.id;
        url = "https://www.warhammer-community.com/en-gb/articles/rbhohtyd/the-big-debate-what-colour-should-you-paint-your-base-rims/";
        isEssential = false;
      };
      "LsAir.bagend | Syncthing" = {
        id = "7c505f6c-200b-4ac3-aaf4-44dff628ecbe";
        workspace = spaces.default.id;
        url = "http://127.0.0.1:8384/";
        isEssential = false;
      };
      "https://kite.kagi.com/" = {
        id = "4a137337-e42e-4887-b39a-d8b010ab23c1";
        workspace = spaces.default.id;
        url = "https://kite.kagi.com/";
        isEssential = false;
      };
      "Warhammer TV" = {
        id = "ea14caba-3840-4840-9c10-53646aa2886b";
        workspace = spaces.default.id;
        url = "https://warhammertv.com/";
        isEssential = true;
      };
      "InfluxDB" = {
        id = "7178cab9-cb9a-4d12-8445-10e5d9d4cda4";
        workspace = spaces.default.id;
        url = "https://influxdb.heron-pollux.ts.net/orgs/3ed1416881e8055b";
        isEssential = false;
      };
      "Home | Patreon" = {
        id = "7265ba94-2e8b-49ea-9b37-c3dd9daabdf8";
        workspace = spaces.default.id;
        url = "https://www.patreon.com/home";
        isEssential = true;
      };
      "scrutiny" = {
        id = "4224f9d1-b7dc-4d2f-b7a3-62d7ccd51b95";
        workspace = spaces.default.id;
        url = "https://scrutiny.heron-pollux.ts.net/web/dashboard";
        isEssential = false;
      };
    };
  };
}
