set-env STARSHIP_SHELL "elvish"
set-env STARSHIP_SESSION_KEY (to-string (randint 10000000000000 10000000000000000))

use github.com/zzamboni/elvish-modules/util

# Define Hooks
var cmd-status-code = 0

fn starship-after-command-hook {|m|
    var error = $m[error]
    if (is $error $nil) {
        set cmd-status-code = 0
    } else {
        set cmd-status-code = (util:path-in $error [reason exit-status] &default=-1)
    }
}

# Install Hooks
set edit:after-command = [ $@edit:after-command $starship-after-command-hook~ ]

# Install starship
set edit:prompt = {
    var cmd-duration = (printf "%.0f" (* $edit:command-duration 1000))
    ~/.nix-profile/bin/starship prompt --jobs=$num-bg-jobs --cmd-duration=$cmd-duration --status=$cmd-status-code --logical-path=$pwd
}

set edit:rprompt = {
    var cmd-duration = (printf "%.0f" (* $edit:command-duration 1000))
    ~/.nix-profile/bin/starship prompt --right --jobs=$num-bg-jobs --cmd-duration=$cmd-duration --status=$cmd-status-code --logical-path=$pwd
}
