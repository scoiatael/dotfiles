function rvm --description "Ruby enVironment Manager"
  bash -c 'source $HOME/.rvm/scripts/rvm; rvm "$@"' '' $argv
end
