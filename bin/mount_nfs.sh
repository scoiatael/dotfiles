cryptsetup open /dev/disk/by-uuid/127162ee-cb94-4d6f-a313-15dd7607fdbb cryptHdd_mirror4
cryptsetup open /dev/disk/by-uuid/1c9c6c95-effb-4e8f-8df0-bb024bd81502 cryptHdd_mirror5
cryptsetup open /dev/disk/by-uuid/2c48ce2e-2aa6-4557-b71c-14d47805eb87 cryptHdd_mirror3
vgchange --refresh
mount /dev/mapper/SrvVG-nfs /srv/nfs
