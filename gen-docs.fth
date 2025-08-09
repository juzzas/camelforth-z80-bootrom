fpath  path+  tools

INCLUDE ./tools/glossary.fth

open-glossary: glossary.md

add-source-file: camel80.asm.m4
add-source-file: camel80d.asm.m4
add-source-file: camel80h.asm.m4
add-source-file: camel80r.asm.m4
add-source-file: camel80u.asm.m4
add-source-file: camel80x.asm.m4

gen-glossary

output-glossary-core
output-glossary-extensions
output-glossary-utils
output-glossary-private

close-glossary



