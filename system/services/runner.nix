{ config
, lib
, pkgs
, ...
}:
let
  cfg = config.services.githubRunner;

  runnerTarball = pkgs.fetchurl {
    url = "https://github.com/actions/runner/releases/download/v${cfg.version}/actions-runner-linux-x64-${cfg.version}.tar.gz";
    inherit (cfg) sha256;
  };

  actionsRunnerPkg = pkgs.stdenv.mkDerivation {
    pname = "actions-runner";
    inherit (cfg) version;
    src = runnerTarball;

    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp -r ./* $out/
      chmod +x $out/config.sh $out/run.sh $out/svc.sh
      runHook postInstall
    '';

    meta = {
      description = "GitHub Actions self-hosted runner";
      homepage = "https://github.com/actions/runner";
      license = lib.licenses.mit;
      platforms = [ "x86_64-linux" ];
    };
  };

  setupScript = pkgs.writeShellScriptBin "github-runner-setup" ''
        set -euo pipefail

        WORK_DIR="${cfg.workDir}"
        RUNNER_USER="${cfg.user}"
        RUNNER_GROUP="${cfg.group}"
        PACKAGE_DIR="${actionsRunnerPkg}"

        echo "Ensuring GitHub Actions runner work directory $WORK_DIR exists..."
        mkdir -p "$WORK_DIR"

        echo "Copying runner files from $PACKAGE_DIR to $WORK_DIR..."
        ${pkgs.rsync}/bin/rsync -a --delete \
          --exclude='_work/' \
          --exclude='.runner' \
          --exclude='.credentials' \
          --exclude='.env' \
          "$PACKAGE_DIR/" "$WORK_DIR/"
        chown -R "$RUNNER_USER:$RUNNER_GROUP" "$WORK_DIR"

        CONFIG_SH_PATH="$WORK_DIR/config.sh"
        TOKEN_PATH="${cfg.tokenPath}"
        RUNNER_URL="${cfg.url}"
        RUNNER_NAME="${cfg.name}"
        RUNNER_LABELS="${lib.concatStringsSep "," cfg.labels}"
        EPHEMERAL_FLAG=${if cfg.ephemeral then "--ephemeral" else ""}
        REPLACE_FLAG=${if cfg.replace then "--replace" else ""}

        if [ ! -f "$TOKEN_PATH" ]; then
          echo "Error: Token file '$TOKEN_PATH' not found." >&2
          exit 1
        fi

        if [ ! -s "$TOKEN_PATH" ]; then
            echo "Error: Token file '$TOKEN_PATH' is empty." >&2
            exit 1
        fi

        ${pkgs.su}/bin/su -s ${pkgs.bash}/bin/bash "$RUNNER_USER" <<EOF
        set -euo pipefail
        cd "$WORK_DIR"
        if [ -f .runner ] && ! ${builtins.toJSON cfg.forceReconfigure}; then
          echo "Runner already configured in $WORK_DIR and forceReconfigure is false. Skipping configuration."
        else
          if [ -f .runner ]; then
            echo "Runner configuration file .runner exists, but forceReconfigure is true. Re-configuring."
          else
            echo "Configuring runner in $WORK_DIR..."
          fi

          RUNNER_TOKEN="$(<"$TOKEN_PATH")"
          if [ -z "$RUNNER_TOKEN" ]; then
            echo "Error: Token read from '$TOKEN_PATH' is empty (ran as $RUNNER_USER)." >&2
            exit 1
          fi

          echo "Executing: $CONFIG_SH_PATH --unattended --url '$RUNNER_URL' --token '***' --name '$RUNNER_NAME' --labels '$RUNNER_LABELS' --work '_work' $EPHEMERAL_FLAG $REPLACE_FLAG"
          "$CONFIG_SH_PATH" \
            --unattended \
            --url "$RUNNER_URL" \
            --token "$RUNNER_TOKEN" \
            --name "$RUNNER_NAME" \
            --labels "$RUNNER_LABELS" \
            --work "_work" \
            $EPHEMERAL_FLAG \
            $REPLACE_FLAG
        fi
    EOF
        if [ $? -ne 0 ]; then
          echo "Error during runner configuration executed by $RUNNER_USER." >&2
          exit 1
        fi
        echo "GitHub Actions runner setup script completed successfully."
  '';
in
{
  options.services.githubRunner = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable email configuration.";
    };

    version = lib.mkOption {
      type = lib.types.str;
      description = "Version of the GitHub Actions runner (e.g., \"2.315.0\"). Find releases at https://github.com/actions/runner/releases";
      example = "2.315.0";
    };

    sha256 = lib.mkOption {
      type = lib.types.str;
      description = "SHA256 hash of the actions-runner-linux-x64-VERSION.tar.gz for the specified version.";
      example = "15ixlwzkdyqv3c57crq8vj5pcpb92z8gpx27dgh2g0bvyyvjmxq7";
    };

    url = lib.mkOption {
      type = lib.types.str;
      description = "URL of the repository or organization for the runner (e.g., https://github.com/myorg/myrepo or https://github.com/myorg).";
      example = "https://github.com/your-username/your-repo";
    };

    tokenPath = lib.mkOption {
      type = lib.types.path;
      description = "Path to a file containing the GitHub Actions runner registration token. Ensure this file is readable by the runner user.";
      example = "/run/secrets/github-runner-token";
    };

    name = lib.mkOption {
      type = lib.types.str;
      default = config.networking.hostName;
      description = "Name of the runner to register with GitHub.";
    };

    labels = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "self-hosted" "linux" "x64" ];
      description = "List of labels to assign to the runner.";
      example = [ "ci" "production" ];
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "github-runner";
      description = "User to run the GitHub Actions runner as.";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "github-runner";
      description = "Group to run the GitHub Actions runner as.";
    };

    workDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/github-runner";
      description = "Working directory for the runner. Stores configuration, logs, and job workspaces.";
    };

    ephemeral = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Configure the runner as ephemeral. It will run one job and then unregister itself.";
    };

    replace = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Pass --replace to config.sh. This replaces an existing runner with the same name on GitHub. Set to false to attempt to register as a new runner (may fail if name conflicts).";
    };

    forceReconfigure = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Force local re-configuration by running config.sh even if a .runner file already exists in workDir. Useful if runner parameters (URL, name, labels) changed in Nix config and need to be re-applied to an existing runner setup.";
    };

    environment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "Environment variables to set for the runner process.";
      example = { DOTNET_SKIP_FIRST_TIME_EXPERIENCE = "true"; };
    };

    serviceDependencies = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "network.target" ];
      description = "Systemd services that this runner service depends on.";
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.user} = {
      isSystemUser = true;
      inherit (cfg) group;
      home = cfg.workDir;
      description = "GitHub Actions runner user";
    };
    users.groups.${cfg.group} = { };

    systemd.services.github-runner = {
      description = "GitHub Actions Self-hosted Runner";
      wantedBy = [ "multi-user.target" ];
      after = cfg.serviceDependencies;

      path = [ pkgs.coreutils pkgs.gnutar pkgs.gzip pkgs.bash pkgs.rsync pkgs.su ];

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        WorkingDirectory = cfg.workDir;
        ExecStartPre = "${setupScript}/bin/github-runner-setup";
        ExecStart = "${cfg.workDir}/run.sh";
        Restart = "always";
        RestartSec = "30s";
        Environment = lib.mapAttrsToList (name: value: "${name}=${value}") cfg.environment;
        ReadWritePaths = [ cfg.workDir ];
      };
    };
  };

  # HOWTO use:
  # In your sops-nix configuration
  # sops.secrets.github-runner-token = {
  #   # path = /path/to/sops/encrypted/token/file; # if not using default sops file
  #   # key = "github_runner_token_key_in_yaml"; # if using a specific key
  #   owner = config.services.githubRunner.user; # User defined in the githubRunner module
  #   mode = "0400"; # Read-only by owner
  # };

  # Then set in githubRunner config:
  # services.githubRunner.tokenPath = config.sops.secrets.github-runner-token.path;


  #  services.githubRunner = {
  #    enable = true;
  #    version = "2.315.0"; # Check for the latest version on GitHub Runner releases page
  #    sha256 = "15ixlwzkdyqv3c57crq8vj5pcpb92z8gpx27dgh2g0bvyyvjmxq7"; # SHA256 for v2.315.0 linux-x64
  #    url = "https://github.com/your-organization/your-repository"; # Or "https://github.com/your-organization"
  #    tokenPath = "/run/secrets/github-runner-token"; # Path to the file containing your runner token

  #    # Optional settings:
  #    # name = "my-nixos-runner"; # Defaults to hostname
  #    # labels = [ "nixos" "docker" ]; # Customize labels
  #    # ephemeral = true; # For single-job runners
  #    # environment = { MY_ENV_VAR = "value"; };
  #  };

  # The token is optained when activating a new github runner on github UI
}
