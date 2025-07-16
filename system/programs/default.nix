{ pkgs
, ...
}:
{
  # Core pakages for system
  environment = {
    # NOTE: By explicitly setting `environment.systemPackages`, we override the
    # default set of packages that NixOS would otherwise install.
    # If this option were left undefined, NixOS would include a base set
    # roughly equivalent to the following (actual list might vary slightly
    # between NixOS versions):
    #
    # # Essential command-line tools and libraries
    # pkgs.acl
    # pkgs.attr
    # pkgs.bashInteractive # bash with ncurses support
    # pkgs.bzip2
    # pkgs.coreutils-full # Includes common utils like ls, cp, mv, rm, etc.
    # pkgs.cpio
    # pkgs.curl # Already explicitly added below
    # pkgs.diffutils
    # pkgs.findutils
    # pkgs.gawk
    # pkgs.stdenv.cc.libc # Standard C library
    # pkgs.getent
    # pkgs.getconf
    # pkgs.gnugrep
    # pkgs.gnupatch
    # pkgs.gnused
    # pkgs.gnutar
    # pkgs.gzip
    # pkgs.xz
    # pkgs.less # Pager
    # pkgs.libcap # Linux capabilities management library
    # pkgs.ncurses # Terminal handling library
    # pkgs.netcat # Networking utility
    # config.programs.ssh.package # SSH client/server package (usually OpenSSH)
    # pkgs.mkpasswd # Utility from whois package
    # pkgs.procps # Utilities for process monitoring (ps, top, etc.)
    # pkgs.su # From shadow package usually
    # pkgs.time # /usr/bin/time command
    # pkgs.util-linux # Many essential utilities (mount, fdisk, dmesg, etc.)
    # pkgs.which
    # pkgs.zstd # Zstandard compression
    #
    # # Plus packages defined by their names (less common now)
    # # "perl"
    # # "rsync"
    # # "strace"
    #
    # Explicit list starts here:
    systemPackages = with pkgs; [
      wget
      git
    ];

    # Default editor for minor things
    variables.EDITOR = "nvim";
  };

  imports = [
    ./cachix
    ./docker.nix
    ./fish.nix
    ./fonts.nix
    ./qemu.nix
    ./steam.nix
  ];

  programs = {
    # Allow non-root users to specify the allow_other or allow_root mount options
    fuse.userAllowOther = true;
    # Nano is enabled by default, but not anymore...
    nano.enable = false;
  };
}
