{
  description = "A soothing pastel theme for SDDM";

  # Nixpkgs / NixOS version to use.
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";

  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };

  outputs =
    { self, nixpkgs, ... }:
    let
      # Generate a user-friendly version number.
      version = builtins.substring 0 1 self.lastModifiedDate;

      # System types to support.
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      # Nixpkgs instantiated for supported system types.
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in
    {
      # Provide some binary packages for selected system types.
      packages = forAllSystems (system: {
        xmoctrl = nixpkgsFor.${system}.stdenvNoCC.mkDerivation {
          pname = "xmoctrl";
          inherit version;

          src = nixpkgs.lib.cleanSourceWith {
            filter = name: type: type != "regular" || !nixpkgs.lib.hasSuffix ".nix" name;
            src = nixpkgs.lib.cleanSource ./.;
          };

          buildInputs = [
            nixpkgsFor.${system}.xmonadctl
            (nixpkgsFor.${system}.haskellPackages.ghcWithPackages (
              pkgs: with pkgs; [
                cabal-install
                (pkgs.mkDerivation {
                  pname = "xmolib";
                  version = "0.0.1";
                  src = ./xmolib;
                  libraryHaskellDepends = with pkgs; [
                    base prettyprinter prettyprinter-ansi-terminal process text
                    transformers transformers-compat
                    aeson xmonad xmonad-contrib xmonad-dbus xmonad-extras
                  ];
                  license = nixpkgs.lib.licenses.bsd3;
                })
              ]
            ))
          ];
          propagatedBuildInputs = [
            nixpkgsFor.${system}.xmonadctl
          ];

          dontConfigure = true;
          buildPhase = ''
            ghc main.hs
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp main $out/bin/xmoctrl
          '';

          meta = {
            description = "Soothing pastel theme for SDDM";
            homepage = "https://github.com/mitchdzugan/sddm-dz";
            license = nixpkgs.lib.licenses.mit;
            maintainers = with nixpkgs.lib.maintainers; [ mitchdzugan ];
            platforms = nixpkgs.lib.platforms.linux;
          };
        };
      });

      # The default package for 'nix build'. This makes sense if the
      # flake provides only one package or there is a clear "main"
      # package.
      defaultPackage = forAllSystems (system: self.packages.${system}.xmoctrl);

      devShell = forAllSystems (
        system:
        let
          pkgs = nixpkgsFor.${system};
        in
        pkgs.mkShell { buildInputs = with pkgs; [ ]; }
      );
    };
}
