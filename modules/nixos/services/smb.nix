{ pkgs, ... }: {
  boot.initrd.kernelModules = [
    "dm-snapshot" # when you are using snapshots
    "dm-raid" # e.g. when you are configuring raid1 via: `lvconvert -m1 /dev/pool/home`
    "dm-cache-default" # when using volumes set up with lvmcache
  ];
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      hinfo = true;
      userServices = true;
      workstation = true;
    };
    extraServiceFiles = {
      smb = ''
        <?xml version="1.0" standalone='no'?><!--*-nxml-*-->
        <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
        <service-group>
          <name replace-wildcards="yes">%h</name>
          <service>
            <type>_smb._tcp</type>
            <port>445</port>
          </service>
        </service-group>
      '';
    };
  };
  networking.firewall.allowPing = true;
  services.samba.openFirewall = true;
  users = {
    groups.bilbo = { };
    groups.samba-users = { };
    users.bilbo = {
      isSystemUser = true;
      description = "Samba login account";
      group = "bilbo";
      home = "/var/empty";
      createHome = false;
      shell = pkgs.shadow;
      extraGroups = [ "samba-users" ];
    };
    users.samba-user = {
      isSystemUser = true;
      description = "Samba forced account";
      group = "bilbo";
      home = "/var/empty";
      createHome = false;
      shell = pkgs.shadow;
      extraGroups = [ "samba-users" ];
    };
  };
  services.samba-wsdd.enable =
    true; # make shares visible for windows 10 clients
  networking.firewall.allowedTCPPorts = [
    5357 # wsdd
  ];
  networking.firewall.allowedUDPPorts = [
    3702 # wsdd
  ];
  services.samba = {
    enable = true;
    settings = {
      global = {
        security = "user";
        "log level" = 3;
        workgroup = "WORKGROUP";
        "server string" = "smbnix";
        "netbios name" = "smbnix";
        #use sendfile = yes
        #max protocol = smb2
        # note: localhost is the ipv6 localhost ::1
        "hosts allow" = "192.168.1. 127.0.0.1 localhost";
        "hosts deny" = "0.0.0.0/0";
        "guest account" = "nobody";
        "map to guest" = "bad user";
        "server min protocol" = "SMB2_02";
        "server smb encrypt" = "desired";
        "use sendfile" = "yes";
        "min receivefile size" = 16384;
        "fruit:aapl" = "yes";
        "fruit:time machine" = "yes";
        "vfs objects" = "catia fruit streams_xattr";
      };
      private = {
        path = "/srv/nfs/";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "samba-user";
        "force group" = "samba-users";
      };
    };
  };
}
