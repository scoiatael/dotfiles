function c -d "Quick change CWD" -w cd
  echo $argv | grep -q -E '^\.\.\.*'
  and begin
    set -l CD (echo $argv | ruby -e 'puts STDIN.read.sub(".", "").gsub(".", "../")')
    cd $CD
  end
  or begin
    echo
    cd $argv
  end
end
