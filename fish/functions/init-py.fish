function init-py -d "init .envrc to work on Python"
  printf "\
use asdf
layout python
  " | cat > .envrc
  direnv allow
end
