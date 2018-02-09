set -g pad " "

## Function to show a segment
function prompt_segment -d "Function to show a segment"
  # Get colors
  set -l bg $argv[1]
  set -l fg $argv[2]

  # Set 'em
  set_color -b $bg
  set_color $fg

  # Print text
  if [ -n "$argv[3]" ]
    echo -n -s $argv[3]
  end
end

## Function to show current status
function show_status -d "Function to show the current status"
  if [ $RETVAL -ne 0 ]
    prompt_segment red white " ▲ "
    set pad ""
    end
  if [ -n "$SSH_CLIENT" ]
      prompt_segment blue white " SSH: "
      set pad ""
    end
  set_color normal
end

## Show user if not default
function show_user -d "Show user"
  if [ "$USER" != "$default_user" -o -n "$SSH_CLIENT" ]
    set -l host (hostname -s)
    set -l who (whoami)
    prompt_segment normal yellow " $who"

    # Skip @ bit if hostname == username
    if [ "$USER" != "$HOST" ]
      prompt_segment normal white "@"
      prompt_segment normal green "$host "
      set pad ""
    end
    end
end

# Show directory
function show_pwd -d "Show the current directory"
  set -l pwd (prompt_pwd)
  prompt_segment normal blue "$pad$pwd "
end

# Show prompt w/ privilege cue
function show_prompt -d "Shows prompt with cue for current priv"
  set -l uid (id -u $USER)
    if [ $uid -eq 0 ]
    prompt_segment red white "! "
    set_color normal
    echo -n -s " "
  else
    prompt_segment normal white "\$ "
    end

  set_color normal
end

function show_vi_status -d "Shows vi mode"
  switch $fish_bind_mode
    case default
        prompt_segment blue black '[N]'
    case insert
        prompt_segment yellow black '[I]'
    case visual
        prompt_segment magenta black '[V]'
  end
  set_color normal
end

function show_git_status -d "Shows the current git status"
    if command git rev-parse --is-inside-work-tree >/dev/null 2>&1
        set -l dirty (command git status -s --ignore-submodules=dirty | wc -l | sed -e 's/^ *//' -e 's/ *$//' 2> /dev/null)
        set -l ref (command git describe --tags --exact-match ^/dev/null ; or command git symbolic-ref --short HEAD 2> /dev/null ; or command git rev-parse --short HEAD 2> /dev/null)
        set -l fish_prompt_git_status_ref_length 4
        set -l short_ref (string replace -ar '(\.?[^/]{'"$fish_prompt_git_status_ref_length"'})[^/]*/' '$1/' $ref)

        if [ "$dirty" != "0" ]
            set_color -b red
            set_color black
        else
            set_color -b cyan
            set_color black
        end

        echo -n "$short_ref"
        set_color normal
    end
end

function show_short_git_status -d "Shows the current git status with two dots"
    if command git rev-parse --is-inside-work-tree >/dev/null 2>&1
        set -l staged (command git diff --ignore-submodules=dirty --shortstat --staged | tee /dev/null | cut -c 1,2)
        set -l dirty (command git diff --ignore-submodules=dirty --shortstat | tee /dev/null | cut -c 1,2)

        if test -n "$dirty"
            set_color red
        else
            set_color cyan
        end
        echo -n "○"

        if test -n "$staged"
            set_color green
        else
            set_color cyan
        end
        echo -n "○"
        set_color normal
    else
        echo -n "··"
    end
end


## SHOW PROMPT
function fish_prompt
  set -g RETVAL $status
  # show_status
  echo -n ' '
#  show_user
  if test "$COLUMNS" -gt 90
      show_pwd
      show_git_status
      echo '' # Break to multiline
  end
  show_vi_status
  show_short_git_status
  show_prompt
end
