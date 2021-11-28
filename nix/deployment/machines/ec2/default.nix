{
  imports = [
    (builtins.fromJSON ''
      {
        "fileSystems": {
          "/": {
            "device": "tank/system/root",
            "fsType": "zfs"
          },
          "/home": {
            "device": "tank/user/home",
            "fsType": "zfs"
          },
          "/nix": {
            "device": "tank/local/nix",
            "fsType": "zfs"
          },
          "/var": {
            "device": "tank/system/var",
            "fsType": "zfs"
          }
        }
      }
    '')
    ({ modulesPath, pkgs, ... }:
     {
       imports = [
         "${modulesPath}/virtualisation/amazon-image.nix"

         ./roles/common # common configuration for any instance
         ./users/dex    # `dex` user setup
       ];

       # ec2 specific configuration
       ec2.hvm             = true;
       ec2.zfs.enable      = true;
       networking.hostId   = "00000000";
       networking.hostName = "ec2-backend-byron-network";
     })
  ];
}
