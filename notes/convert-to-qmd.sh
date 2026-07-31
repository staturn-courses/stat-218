#!/usr/bin/env bash

for file in old-notes/*.docx; do
    base=$(basename "$file" .docx)
    pandoc \
        "$file" \
        -f docx \
        -t markdown \
        -s \
        --wrap=none \
        -o "${base}.qmd"
done