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
          home-manager-module = { config, lib, ... }: {
            options = {
              programs.emacs.userDir = lib.mkOption { type = lib.types.str; };
            };
            config = {
              programs.emacs.enable = true;

              home.file.".emacs.d/early-init.el".text = ''
                (defun ensure-git-repo (dir repo-url)
                  (let ((expanded-dir (expand-file-name dir)))
                    (if (file-directory-p expanded-dir)
                        (message "Directory %s already exists." expanded-dir)
                      (let ((default-directory (file-name-directory expanded-dir)))
                        (message "Cloning %s into %s..." repo-url expanded-dir)
                        (let ((exit-code (call-process "git" nil "*git-clone-output*" t "clone" repo-url expanded-dir)))
                          (if (zerop exit-code)
                              (message "Cloned %s successfully." repo-url)
                            (message "Failed to clone %s. See *git-clone-output* buffer for details." repo-url)))))))

                (ensure-git-repo "${config.programs.emacs.userDir}" "git@github.com:rfhayashi/emacs.d")

                (setq user-emacs-directory "${config.programs.emacs.userDir}")

                (let ((early-init-file (expand-file-name "early-init.el" user-emacs-directory)))
                  (load early-init-file t t))
              '';

              home.file.".emacs.d/init.el".text = ''
                ;; install straight.el
                (defvar bootstrap-version)
                (let ((bootstrap-file
                       (expand-file-name
                        "straight/repos/straight.el/bootstrap.el"
                        (or (bound-and-true-p straight-base-dir)
                            user-emacs-directory)))
                      (bootstrap-version 7))
                  (unless (file-exists-p bootstrap-file)
                    (with-current-buffer
                        (url-retrieve-synchronously
                         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
                         'silent 'inhibit-cookies)
                      (goto-char (point-max))
                      (eval-print-last-sexp)))
                  (load bootstrap-file nil 'nomessage))

                (let ((init-file (expand-file-name "init.el" user-emacs-directory)))
                  (load init-file t t))
              '';
            };
          };
        };
      };
      perSystem = { pkgs, ... }: {
        devShells.default = pkgs.mkShell { packages = [ pkgs.home-manager ]; };
      };
    };
}
