#!/bin/sh
## MANUAL:
# add wallpaper to /usr/share/backgrounds/default.jpg
# add 'nosplash' to boot options
##

#packages ->
cat etc/pkglist.txt | pacman -Syu --needed -
#lightdm ->
chown -hR lightdm:lightdm /var/lib/lightdm
tar czf lightdm.tgz.bp /etc/lightdm/* 
tar xzf lightdm.tgz -C /
#rsyslog -> 
chown 0644 /var/log/messages
tar czf rsyslog.tgz.bp /etc/rsyslog.conf
tar xzf rsyslog.tgz -C /
#net names -> 
ln -s /dev/null /etc/polkit-1/rules.d/80-net-name-slot.rules
#volume -> 
#modprobe snd_mixer_oss
echo 'snd_mixer_oss' > /etc/modules-load.d/snd_mixer_oss.conf
#copy/paste -> 
cp urxvtclip /usr/lib/urxvt/perl/clipboard
#fonts ->
ln -s /etc/fonts/conf.avail/70-no-bitmaps.conf /etc/fonts/conf.d/
