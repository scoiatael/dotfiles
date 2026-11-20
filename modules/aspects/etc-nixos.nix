{ inputs, ... }:
{
  # Lets a host rebuild itself from the flake it was deployed with, e.g. after `nixos-enter` from a rescue USB.
  # The link is absolute and read-only: resolve it inside `nixos-enter`, and `cp -rL` it out to edit.
  den.aspects.etc-nixos.nixos.environment.etc."nixos/dotfiles".source = inputs.self;
}
