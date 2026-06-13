{ inputs, ... }:

let
  mkStylix = pkgs: {
    stylix = {
      enable = true;
      base16Scheme = "${pkgs.base16-schemes}/share/themes/catppuccin-frappe.yaml";
    };
  };
in
{
  den.aspects.stylix = {
    nixos =
      { pkgs, ... }:
      {
        imports = [ inputs.stylix.nixosModules.stylix ];
        inherit (mkStylix pkgs) stylix;
      };
    darwin =
      { pkgs, ... }:
      {
        imports = [ inputs.stylix.darwinModules.stylix ];
        inherit (mkStylix pkgs) stylix;
      };
  };
}
