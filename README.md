## My configuration files.

Managed by [home-manager](https://github.com/nix-community/home-manager).

Now using [flakes!](https://nix-community.github.io/home-manager/index.html#sec-flakes-standalone)

Simply `home-manager switch --flake 'path:dotfiles#framework'` - or roll your own :)


# macOS Clean install

1. Clone this repo.
1. Install nix from https://nixos.org/download.html#nix-install-macos
1. Install brew from https://docs.brew.sh/Installation
1. Edit your hostname via https://support.apple.com/en-gb/guide/mac-help/mchlp2322/mac
1. Reboot :)
1. `nix --extra-experimental-features nix-command --extra-experimental-features flakes build ~/dotfiles#darwinConfigurations.LsMBP.system`
1. `./result/sw/bin/darwin-rebuild switch --flake ~/dotfiles`
1. Set Spotlight to ⌘D ;)
1. `nix build --extra-experimental-features flakes --extra-experimental-features nix-command .#homeConfigurations.lukaszczaplinski@LsGamingDarwin.activationPackage`
1. `./result/activate`

# Yubikey transfer

Source: https://github.com/drduh/YubiKey-Guide#switching-between-two-or-more-yubikeys
```
gpg-connect-agent "scd serialno" "learn --force" /bye
```
