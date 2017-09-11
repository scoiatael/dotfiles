function rvm --description "Ruby enVironment Manager"
  # run RVM and capture the resulting environment
  set --local env_file (mktemp -t rvm.fish.XXXXXXXXXX)

  bash -c 'source $HOME/.rvm/scripts/rvm; rvm "$@"; rvm env > $0' $env_file $argv
  # apply rvm_* and *PATH variables from the captured environment
  and bass eval (cat $env_file)
  # needed under fish >= 2.2.0
  and set -xg GEM_PATH (echo $GEM_PATH | sed 's/ /:/g')

  # clean up
  rm -f $env_file
end
