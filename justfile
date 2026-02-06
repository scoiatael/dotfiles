# See flake.nix (just-flake)
import 'just-flake.just'

# Display the list of recipes
default:
    @just --list

# Works fell with Fedora 42 64bits on Scaleway (https://console.online.net/en/vps/list)
# Install nixOS on target
infect target:
    @echo 'Infecting {{target}} with nixOS…'
    test -f nixos-infect || wget https://raw.githubusercontent.com/elitak/nixos-infect/master/nixos-infect
    echo "0fb4fe42249b7d5bbb205a45f9466ed542a3095c1cbe4452f2b60d9adf8f3375  nixos-infect" | sha256sum -c
    scp nixos-infect {{target}}:nixos-infect
    ssh -t {{target}} bash -c 'NIX_CHANNEL=nixos-25.05 NO_SWAP=1 doNetConf=y bash -ex ./nixos-infect'

deploy target flake build=target:
    @echo 'Deploying {{flake}} to {{target}} using {{build}}'
    nixos-rebuild --build-host {{build}} --target-host {{target}} --flake {{flake}} switch

deploy-sd-161581: (deploy "root@sd-161581.scoiatael.omg.lol" ".#sd-161581" )
deploy-framework: (deploy "root@192.168.180.153" ".#LsFramework" )
