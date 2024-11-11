EXTENSIONS=(
raycast://extensions/loris/safari
raycast://extensions/the-browser-company/arc
raycast://extensions/rolandleth/kill-process
raycast://extensions/yug2005/mail
raycast://extensions/asubbotin/shell
raycast://extensions/sawyerh/type-snob
raycast://extensions/mmazzarolo/unicode-symbols
raycast://extensions/fedevitaledev/music
)

for ext in ${EXTENSIONS[@]}; do
	open "$ext"
	printf "$(basename $ext): installed?"
	read
done
