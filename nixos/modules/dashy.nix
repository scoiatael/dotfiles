{ config, lib, pkgs, ... }:

{
  services.dashy = {
    enable = true;
    settings = {
      appConfig = {
        faviconApi = "local";
        cssThemes = [ ];
        enableFontAwesome = true;
        fontAwesomeKey = "e9076c7025";
        theme = "nord";
        layout = "horizontal";
        webSearch = {
          searchEngine = "custom";
          customSearchEngine = "https://kagi.com/search?q=";
        };
      };
      pageInfo = { title = "Dashy"; };
      sections = [
        {
          displayData = {
            collapsed = false;
            cols = 2;
          };
          items = [
            {
              description = "All RSS in one place";
              icon = "sh-yarr";
              title = "Yarr";
              url = "https://yarr.heron-pollux.ts.net";
            }
            {
              description = "All the pretty charts";
              icon = "sh-grafana";
              title = "Grafana";
              url = "https://grafana.heron-pollux.ts.net";
            }
            {
              description = "Wanna watch something?";
              icon = "sh-jellyfin";
              title = "Jellyfin";
              url = "https://jellyfin.heron-pollux.ts.net";
            }
            {
              description = "Grab a movie for tonight";
              icon = "sh-deluge";
              title = "Deluge";
              url = "https://deluge.heron-pollux.ts.net";
            }
            {
              description = "Are the disks fine?";
              icon = "sh-scrutiny";
              title = "Scrutiny";
              url = "https://scrutiny.heron-pollux.ts.net";
            }
            {
              description = "Nothing interesting here.";
              icon = "sh-influxdb";
              title = "InfluxDB";
              url = "https://influxdb.heron-pollux.ts.net";
            }
            {
              description = "Pretty pics";
              icon = "mdi-image-album";
              title = "Parrhasius";
              url = "https://parrhasius.heron-pollux.ts.net";
            }
          ];
          name = "Internal";
        }
        {
          displayData = {
            collapsed = false;
            cols = 2;
          };
          items = [
            {
              description = "Main";
              icon = "favicon";
              title = "scoiatael.dev";
              url = "https://scoiatael.dev";
            }
            {
              description = "Encrypt secrets for Github users";
              icon = "generative";
              title = "octocrypt";
              url = "https://octocrypt.scoiatael.dev";
            }
            {
              description = "Grab your current public IP";
              icon = "generative";
              title = "IP";
              url = "https://ip.scoiatael.dev";
            }
            {
              description = "Generate encrypted webhook URL";
              icon = "generative";
              title = "Webhooks";
              url = "https://wh.scoiatael.dev/";
            }
            {
              description = "Send file over TCP";
              icon = "generative";
              title = "Magic wormhole";
              url = "https://hole.scoiatael.dev/";
            }
          ];
          name = "scoiatael.dev";
        }
        {
          displayData = {
            collapsed = false;
            cols = 2;
          };
          items = [
            {
              description = "Nix Hoogle";
              icon = "mdi-lambda";
              title = "Noogle";
              url = "https://noogle.dev/";
            }
            {
              icon = "mdi-language-go";
              title = "DevDocs - Go";
              url = "https://devdocs.io/go";
            }
          ];
          name = "Documentation";
        }
        {
          displayData = {
            collapsed = false;
            cols = 2;
          };
          items = [{
            description = "Temporary file hoster.";
            icon = "generative";
            title = "0x0";
            url = "https://0x0.st/";
          }];
          name = "Tools";
        }
      ];
    };
  };

  services.caddy.virtualHosts."dashy.heron-pollux.ts.net".extraConfig = ''
    root * ${config.services.dashy.finalDrv}
    file_server {
      index index.html
    }
  '';
}
