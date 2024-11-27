{
  description = "Python application packaged using poetry2nix";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.poetry2nix.url = "github:nix-community/poetry2nix";

  outputs = { self, nixpkgs, poetry2nix }:
    let
      system = "x86_64-linux";
      xonsh = { poetry2nix, lib }: poetry2nix.mkPoetryApplication {
        projectDir = ./.;
        overrides = poetry2nix.overrides.withDefaults (final: super:
          lib.mapAttrs
            (attr: systems: super.${attr}.overridePythonAttrs
              (old: {
                nativeBuildInputs =
                  (old.nativeBuildInputs or [ ]) ++ (
                    map (a: super.${a}) systems
                  );
              })
            ) {
              xontrib-powerline3 = [ "setuptools" "setuptools-scm" "poetry-core" "xonsh" ];
              xonsh = [ "setuptools" "setuptools-scm" "poetry-core" ];
            }
        );
      };
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          poetry2nix.overlays.default
          (final: _: {
            xonsh = final.callPackage xonsh { };
          })
        ];
      };
    in
    {
      apps.${system}.default = {
        type = "app";
        # replace <script> with the name in the [tool.poetry.scripts] section of your pyproject.toml
        program = "${pkgs.xonsh}/bin/xonsh";
      };
    };
}
