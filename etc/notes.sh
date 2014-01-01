#!/bin/sh
## MANUAL:
# add wallpaper to /usr/share/backgrounds/default.jpg
# add 'nosplash' to boot options
##

#lightdm ->
chown -hR lightdm:lightdm /var/lib/lightdm
tar czf etc/lightdm.tgz.bp /etc/lightdm/* 
tar xzf etc/lightdm.tgz -C /
systemctl enable lightdm
#rsyslog -> 
chown 0644 /var/log/messages
tar czf etc/rsyslog.tgz.bp /etc/rsyslog.conf
tar xzf etc/rsyslog.tgz -C /
systemctl enable rsyslog
#net names -> 
ln -s /dev/null /etc/polkit-1/rules.d/80-net-name-slot.rules
#volume -> 
#modprobe snd_mixer_oss
echo 'snd_mixer_oss' > /etc/modules-load.d/snd_mixer_oss.conf
#copy/paste -> 
cp etc/urxvtclip /usr/lib/urxvt/perl/clipboard
#fonts ->
ln -s /etc/fonts/conf.avail/70-no-bitmaps.conf /etc/fonts/conf.d/
#unicode support
cp etc/locale.* /etc/
