{ lib, pkgs, ...}:
{
programs.qutebrowser = {
      # enable = true; broken on macOS -> enable via ./home-manager/linux.nix
      searchEngines = {
        w =
          "https://en.wikipedia.org/wiki/Special:Search?search={}&go=Go&ns0=1";
        aw = "https://wiki.archlinux.org/?search={}";
        nw = "https://nixos.wiki/index.php?search={}";
        g = "https://www.google.com/search?hl=en&q={}";
        nix = "https://search.nixos.org/packages?query={}";
        k = "https://kagi.com/search?q={}";
        DEFAULT = "https://kagi.com/search?q={}";
        m = "https://melpa.org/#/?q={}";
        b = "https://search.brave.com/search?q={}";
      };
      settings = {
        url.start_pages = "https://kagi.com";
        url.default_page = "https://kagi.com";
        # colors.hints.bg = "qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 rgb(255, 247, 133), stop:1 rgb(255, 197, 66))";
        # colors.webpage.darkmode.enabled = true;
        content.cookies.accept = "no-3rdparty";
        content.default_encoding = "utf-8";
        hints.uppercase = true;
        zoom.default = "135%";
      };
    };
}
