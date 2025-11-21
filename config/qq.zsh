# Based on https://github.com/dzervas/dotfiles/blob/0e52489/home/fish-functions/qq.fish
function qq() {
    # { "commandline": "ss -tuln", "comments": "alternatively 'netstat -tuln" }
    ANSWER="$(echo "${@}" | llm -t qq -p system "$(uname -a)")"
    if [[ "$ANSWER" == *comments* ]];then
        print -P "$(echo "$ANSWER" | jq -r '.comments')"
    fi
    if [[ "$ANSWER" == *commandline* ]];then
        CMD="$(echo "$ANSWER" | jq -r '.commandline')"
        print -z "$CMD"
    fi
}

