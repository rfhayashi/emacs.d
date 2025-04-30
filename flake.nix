{
  description = "My emacs flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      flake = {
        lib = {
          home-manager-module = {
            programs.emacs.enable = true;

            home.file.".emacs.d/early-init.el".text = ''
              (menu-bar-mode -1)
              (tool-bar-mode -1)
              (scroll-bar-mode -1)

              (add-to-list 'default-frame-alist '(fullscreen . maximized))
            '';
          };
        };
      };
      perSystem = { pkgs, ... }: {
        devShells.default = pkgs.mkShell { packages = [ pkgs.home-manager ]; };
      };
    };
}
