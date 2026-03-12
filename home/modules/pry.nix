{ config, lib, pkgs, ... }:

{
  home.packages = [ pkgs.pry ];

  home.file.".pryrc".text = ''
    Pry::Prompt.add(
      :vterm,
      "A simple `>>` w/ vterm ending.",
      ['>> ', ' | ']
    ) do |_, _, _, sep|
      whoami = ENV['USER']
      pwd = ENV['pwd']
      sep + "\e]51;A#{whoami}@:#{pwd}\e\\"
    end

    Pry.config.prompt = Pry::Prompt[:vterm]
  '';
}
