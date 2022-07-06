## My configuration files.

Managed by [home-manager](https://github.com/nix-community/home-manager).

Simply add import to your [~/.config/nixpkgs/home.nix](~/.config/nixpkgs/home.nix):

    imports = [
        "~/dotfiles/home.nix"
        "~/dotfiles/home-manager/linux.nix"
    ];

and `home-manager switch` :)
