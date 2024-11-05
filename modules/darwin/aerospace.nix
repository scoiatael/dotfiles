{ config, lib, pkgs, ... }:

# TODO: https://www.raycast.com/limonkufu/aerospace

let
  on-workspace-change = pkgs.writeShellScript "on-workspace-change.sh" ''
    ${pkgs.sketchybar}/bin/sketchybar --trigger aerospace_workspace_change \
      FOCUSED_WORKSPACE=$AEROSPACE_FOCUSED_WORKSPACE
  '';
in {
  services.aerospace = {
    enable = true;
    settings = let
      toml = builtins.fromTOML (builtins.readFile ../../config/aerospace.toml);
    in toml // {
      exec-on-workspace-change = [ "${on-workspace-change}" ];

    };
  };
  services.sketchybar.config = ''
    sketchybar --add event aerospace_workspace_change

    export FOCUSED_WORKSPACE="$(aerospace list-workspaces --focused)"
    for sid in $(aerospace list-workspaces --all); do
        sketchybar --add item space.$sid left \
            --subscribe space.$sid aerospace_workspace_change \
            --set space.$sid \
            background.color=''${BACKGROUND_1} \
            background.border_color=''${BACKGROUND_2} \
            background.corner_radius=5 \
            background.height=20 \
            background.drawing=off \
            label="$sid" \
            click_script="aerospace workspace $sid" \
            script="$PLUGIN_DIR/aerospace.sh $sid"
    done
  '';

}
