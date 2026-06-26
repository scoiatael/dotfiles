builtins.mapAttrs (
  _: pluginId: {
    install_url = "https://addons.mozilla.org/firefox/downloads/latest/${pluginId}/latest.xpi";
    installation_mode = "force_installed";
  }
)
