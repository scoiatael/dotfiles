{
  pkgs,
  ...
}:

let
  prism-tools = pkgs.fetchFromGitHub {
    owner = "blgardner";
    repo = "prism.tools";
    rev = "fc88d7aec14b0d42a5791524fb6b341867e27a02";
    sha256 = "sha256-+R24UZbIZUtcrtK0SRenwV4niHybt+1KXUy/og5ajqA=";
  };
in
{
  services.nginx.virtualHosts."prism.scoiatael.dev" = {
    forceSSL = true;
    enableACME = true;
    root = "${prism-tools}";
    locations = {
      "/" = {
        index = "index.html";
        tryFiles = "$uri $uri/ =404";
      };
    };
  };
}
