let
  appConfig = ../app.config;
  mkDockerImage = pkgs: package:
    pkgs.dockerTools.buildImage {
      name = "dex-faucet";
      tag = "latest";
      contents = [ package pkgs.bash pkgs.bashInteractive pkgs.coreutils ];
      runAsRoot = ''
        mkdir -p /etc/faucet/
        cp ${appConfig} /etc/faucet/app.config
      '';
      config = { Cmd = [ "/bin/faucet-app" "start" "--config" "/etc/faucet/app.config" ]; };
    };
in
{
  inherit mkDockerImage;
}
