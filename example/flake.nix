{
  description = "Example";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs.url = "path:..";
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { nixpkgs, emacs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations."example" = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          { home.stateVersion = "24.11";
            home.username = "example";
            home.homeDirectory = "/home/example";
          }
          (emacs.lib.home-manager-module { emacsUserDir = "/home/example/dev/emacs.d"; }) ];
      };
    };
}
