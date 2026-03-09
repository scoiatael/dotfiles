{ lib, pkgs, ...}:
{
    programs.helix = {
      enable = true;
      settings = {
        theme = "monokai_pro_machine";
        keys.normal = {
          space.space = "file_picker";
          space.w = ":w";
          space.q = ":q";
        };
      };
    };
}
