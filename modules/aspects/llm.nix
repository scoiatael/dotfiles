{ inputs, ... }: {
  flake-file.inputs.llm-agents.url = "github:numtide/llm-agents.nix";

  den.aspects.llm.homeManager =
    {
      config,
      pkgs,
      self',
      ...
    }:
    let
      configHome = if pkgs.stdenv.isDarwin then "Library/Application Support" else config.xdg.configHome;
      pdf-convert = pkgs.writeShellScriptBin "pdf-to-jpg" ''
        convert -density 300 "$1" -quality 100 "''${1%.*}.jpg"
        echo "''${1%.*}.jpg"
      '';
      llm-invoice = pkgs.writeShellScriptBin "llm-invoice" ''
        llm -t invoice -a "$1" "$1"
      '';
    in
    {
      home.packages =
        with pkgs;
        [
          self'.packages.llm-with-plugins
          self'.packages.markitdown
          # for PDF conversion
          ghostscript
          pdf-convert
          llm-invoice
        ]
        ++ (with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
          (claude-code.overrideAttrs { disableTelemetry = true; })
        ]);

      home.file."${configHome}/io.datasette.llm/default_model.txt".text = "anthropic/claude-sonnet-4-5";

      home.activation.linkLLMTemplates = config.lib.dag.entryAfter [ "writeBoundary" ] ''
        [ ! -L "${configHome}/io.datasette.llm/templates" ] && ln -sf "${config.home.homeDirectory}/dotfiles/config/llm/templates" "${configHome}/io.datasette.llm/templates"
      '';
    };
}
