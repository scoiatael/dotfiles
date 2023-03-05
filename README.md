## My configuration files.

Managed by [home-manager](https://github.com/nix-community/home-manager).

Now using [flakes!](https://nix-community.github.io/home-manager/index.html#sec-flakes-standalone)

Simply `home-manager switch --flake 'path:dotfiles#framework'` - or roll your own :)


# macOS Clean install

1. Clone this repo.
2. Install nix from https://nixos.org/download.html#nix-install-macos
3. Install brew from https://docs.brew.sh/Installation
4. `nix --extra-experimental-features nix-command --extra-experimental-features flakes build ~/dotfiles#darwinConfigurations.LsMBP.system`
5. Edit your hostname via https://support.apple.com/en-gb/guide/mac-help/mchlp2322/mac
5. Reboot :)
5. `./result/sw/bin/darwin-rebuild switch --flake ~/dotfiles`
6. Set Spotlight to ⌘D ;)
7. `nix build --extra-experimental-features flakes --extra-experimental-features nix-command .#homeConfigurations.lukaszczaplinski@LsGamingDarwin.activationPackage`
8. `./result/activate`