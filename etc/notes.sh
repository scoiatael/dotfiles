#!/bin/sh
## MANUAL:
# add wallpaper to /usr/share/backgrounds/default.jpg
# add 'nosplash' to boot options
##

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
modprobe snd_mixer_oss
#copy/paste -> 
cp urxvtclip /usr/lib/urxvt/perl/clipboard

