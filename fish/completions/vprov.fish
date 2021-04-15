#!/usr/bin/env fish

complete -c vprov -f
complete -c vprov -a "(vboxmanage list vms | cut -d_ -f 2)"
