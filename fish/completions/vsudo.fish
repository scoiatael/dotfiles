#!/usr/bin/env fish

complete -c vsudo -f
complete -c vsudo -a "(vboxmanage list vms | cut -d_ -f 2)"
