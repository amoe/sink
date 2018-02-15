#! /bin/sh

for file in test/noninteractive/*; do
    script=$(printf '(load "%s")\n' "$file")
    echo $script | PLTCOLLECTS=$(pwd): plt-r6rs script.sps
done
    
