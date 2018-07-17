## Wrapper around cURL call to cht.sh
function cht -d "Search for documentation on given topic"
    set -l base_url 'https://cht.sh'
    set -l lang
    set -l self (basename -s .fish (status -f))

    getopts $argv | while read -l 1 2
        switch $1
            case _
                set args $args $2
            case l lang
                set lang $2
            case h help
                printf "Usage: $self [-lh] query\n\n"
                printf "        -l --lang Answer query in given language\n"
                printf "        -h --help Show help"
                return 0
            case \*
                printf "$self: '%s' is not a valid option\n" $1
                cht --help
                return 1
        end
    end
    set -l query (string join "+" $args)
    if test "" != "$lang"
        set query "$lang/$query"
    end
    set -l url "$base_url"/"$query"
    spin "curl $url"
end
