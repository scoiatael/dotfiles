{
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (inputs) uv2nix pyproject-nix pyproject-build-systems;
  pname = "kagimcp";

  src = pkgs.fetchFromGitHub {
    owner = "kagisearch";
    repo = pname;
    rev = "55b38d20c67f1406f2c284af776de395297a75cc";
    sha256 = "sha256-GBXAxYjWFkvCBcYyxf2ObqzMjJSPREU3fFHCqyPGnyE=";
  };

  workspace = uv2nix.lib.workspace.loadWorkspace { workspaceRoot = src; };

  overlay = workspace.mkPyprojectOverlay {
    sourcePreference = "wheel";
  };

  # Single-tenant deployment; we don't need to validate Authorization header
  patchAuthorization = final: prev: {
    kagimcp = prev.kagimcp.overrideAttrs (old: {
      postFixup = ''
        substituteInPlace $out/lib/python3.12/site-packages/kagimcp/server.py \
            --replace-fail 'mcp = FastMCP("kagimcp", auth=_KagiKeyPassthroughVerifier())' 'mcp = FastMCP("kagimcp")'
      '';
    });
  };

  pythonSets =
    let
      python = pkgs.python312;
    in
    (pkgs.callPackage pyproject-nix.build.packages {
      inherit python;
    }).overrideScope
      (
        lib.composeManyExtensions [
          pyproject-build-systems.overlays.wheel
          overlay
          patchAuthorization
        ]
      );
in
pythonSets.mkVirtualEnv "kagimcp-env" workspace.deps.default
