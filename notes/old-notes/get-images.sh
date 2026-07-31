#!/usr/bin/env bash

mkdir -p images-raw

for file in *.docx; do 
    base=$(basename "$file" .docx)

    mkdir -p "images-raw/$base"

    for image in $(unzip -Z1 "$file" "word/media/*"); do
        filename=$(basename "$image")
        unzip -p "$file" "$image" > "images-raw/$base/${base}_${filename}"
    done
done