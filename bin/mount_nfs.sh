cryptsetup open /dev/disk/by-uuid/185303df-9a82-49b7-936a-d3114f66f37c cryptHdd_mirror1
cryptsetup open /dev/disk/by-uuid/1c9c6c95-effb-4e8f-8df0-bb024bd81502 cryptHdd_mirror2
cryptsetup open /dev/disk/by-uuid/c5ff78fd-65af-403c-a64b-d80d8192fbf6 cryptHdd_mirror3
vgchange --refresh
mount /dev/mapper/SrvVG-nfs /srv/nfs
