{ config, ... }:
{
  # See https://github.com/rastislavcore/osx-compose-key/blob/4741efbc3f8659978eab5fd6266b2a91a696bf5b/DefaultKeyBinding.dict
  # You need to manually bind a key to § on your keyboard.
  home-manager.users.${config.system.primaryUser}.imports = [
    {
      home.file."Library/KeyBindings/DefaultKeyBinding.dict".source = ./DefaultKeyBinding.dict;
    }
  ];
}
