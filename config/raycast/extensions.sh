EXTENSIONS=(
raycast://extensions/loris/safari
raycast://extensions/the-browser-company/arc
raycast://extensions/rolandleth/kill-process
raycast://extensions/yug2005/mail
raycast://extensions/asubbotin/shell
raycast://extensions/sawyerh/type-snob
raycast://extensions/mmazzarolo/unicode-symbols
raycast://extensions/fedevitaledev/music
raycast://extensions/sjdonado/idonthavespotify
raycast://extensions/petra/prisma-docs-search
raycast://extensions/limonkufu/aerospace
raycast://extensions/thomas/color-picker
raycast://extensions/djpowers/devdocs
)

for ext in ${EXTENSIONS[@]}; do
	open "$ext"
	printf "$(basename $ext): installed?"
	read
done
