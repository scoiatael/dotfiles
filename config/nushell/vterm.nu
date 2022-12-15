def-env __vterm_printf [...rest:string] {
    if ($env.TERM =~ "tmux") {
        printf '\ePtmux;\e\e]%s\007\e\\' $rest
    } else {
        printf '\e]%s\e\\' $rest
    }
}

alias vterm_printf = __vterm_printf

let-env PROMPT_INDICATOR = {
    let whoami = (^whoami | str trim -r -c "\n")
    let hostname = (^hostname | str trim -r -c "\n")
    let pwd = (^pwd | str trim -r -c "\n")
    vterm_printf $"51;A($whoami)@($hostname):($pwd)"
}
