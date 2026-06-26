builtins.mapAttrs (
  _: value: {
    Value = value;
    Status = "locked";
  }
)
