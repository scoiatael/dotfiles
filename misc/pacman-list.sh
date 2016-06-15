TYPE=m
OUT="foreign"
if [[ $1 == "native" ]];
then
  OUT=$1
  TYPE="n"
fi
if [[ $1 == "foreign" ]];
then
  OUT=$1
  TYPE="m"
fi
exec pacman -Qqet$TYPE > pacman-$OUT.txt
