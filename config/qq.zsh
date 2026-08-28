# Based on https://github.com/dzervas/dotfiles/blob/0e52489/home/fish-functions/qq.fish
function qq() {
    local ANSWER cmd
    # { "commandline": "ss -tuln", "comments": "alternatively 'netstat -tuln" }
    ANSWER="$(echo "${@}" | llm -t qq -p system "$(uname -a)")"
    if ! echo "$ANSWER" | jq -e . &> /dev/null; then
        print -P "%F{red}qq error:%f $ANSWER" >&2
        return 1
    fi
    if echo "$ANSWER" | jq -e 'has("comments")' &>/dev/null; then
        print -P "$(echo "$ANSWER" | jq -r '.comments')"
    fi
    if echo "$ANSWER" | jq -e 'has("commandline")' &>/dev/null; then
        CMD="$(echo "$ANSWER" | jq -r '.commandline')"
        print -z "$CMD"
    fi
}

