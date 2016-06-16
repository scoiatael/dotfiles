updatemirrors.sh
#!/bin/sh

[ "$UID" != 0 ] && su=sudo

country='PL'
url="https://www.archlinux.org/mirrorlist/?country=$country&protocol=https&protocol=http&ip_version=4&use_mirror_status=on"

tmpfile=$(mktemp --suffix=-mirrorlist)

# Get latest mirror list and save to tmpfile
wget -qO- "$url" | sed 's/^#Server/Server/g' > "$tmpfile"

# Backup and replace current mirrorlist file (if new file is non-zero)
if [ -s "$tmpfile" ]
then
  { echo " Backing up the original mirrorlist..."
    $su mv -i /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.orig; } &&
  { echo " Rotating the new list into place..."
    $su mv -i "$tmpfile" /etc/pacman.d/mirrorlist; }
else
  echo " Unable to update, could not download list."
fi

# allow global read access (required for non-root yaourt execution)
chmod +r /etc/pacman.d/mirrorlist