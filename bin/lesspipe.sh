#!/bin/sh
PYGMENTIZE_BIN=$(which pygmentize)
if test -z "$PYGMENTIZE_BIN";
then
  exit 1
fi
PYGMENTIZE="${PYGMENTIZE_BIN} -O encoding='utf-8'"

case "$1" in
    *.awk|*.groff|*.java|*.js|*.m4|*.php|*.pl|*.pm|*.pod|*.sh|\
    *.ad[asb]|*.asm|*.inc|*.[ch]|*.[ch]pp|*.[ch]xx|*.cc|*.hh|\
    *.lsp|*.l|*.pas|*.p|*.xml|*.xps|*.xsl|*.axp|*.ppd|*.pov|\
    *.coffee|*.json|*.js|*.html|\
    *.diff|*.patch|*.py|*.rb|*.sql|*.ebuild|*.eclass)
      $PYGMENTIZE "$1"
      exit 0
      ;;
    .bashrc|.bash_aliases|.bash_environment)
        $PYGMENTIZE -l sh "$1"
        exit 0
        ;;
esac

# Decode directories:
if [ -d "$1" ]; then
  echo "$1:"; ls -l $1
  exit 0
fi

grep "#\!/bin/bash" "$1" > /dev/null
if [ "$?" -eq "0" ]; then
    $PYGMENTIZE -l sh "$1"
    exit 0
fi

# guess based on filetype
if [ -x "$1" ]; then
    type=$(file "$1")
    case "$type" in
        *Python* )
            $PYGMENTIZE -l python "$1"
            exit 0
            ;;
        *executable* )
            echo -e "$type\n"
            strings "$1"
            exit 0
            ;;
    esac
fi
