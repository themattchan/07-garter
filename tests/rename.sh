for file in input/*.fdl
do
  mv "$file" "${file/.fdl/.gtr}"
done
