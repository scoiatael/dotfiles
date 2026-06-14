{ ... }:

{
  services.magic-wormhole-mailbox-server.enable = true;

  services.nginx.virtualHosts."hole.scoiatael.dev" = {
    forceSSL = true;
    enableACME = true;
    locations = {
      "= /" = {
        extraConfig = ''
          add_header Content-Type text/html;
          return 200 '<html><body><p>For file sharing run <code>, wormhole --relay-url wss://hole.scoiatael.dev/v1 send FILENAME</code></p></body></html>';
        '';
      };
      "/" = {
        proxyPass = "http://localhost:4000";
        proxyWebsockets = true;
      };
    };
  };
}
