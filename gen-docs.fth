fpath  path+  tools

INCLUDE ./tools/glossary.fth

open-glossary: glossary.txt

add-source-file: camel80.asm.m4
add-source-file: camel80d.asm.m4
add-source-file: camel80h.asm.m4
add-source-file: camel80r.asm.m4
add-source-file: camel80u.asm.m4
add-source-file: camel80x.asm.m4

gen-glossary

output-text" CamelForth BootROM Glossary"
output-text" ==========================="
output-text" "
output-text" Guide to stack diagrams:  R: = return stack,"
output-text"   c = 8-bit character, flag = boolean (0 or -1), 
output-text"   n = signed 16-bit, u = unsigned 16-bit,"
output-text"   d = signed 32-bit, ud = unsigned 32-bit,"
output-text"   +n = unsigned 15-bit, x = any cell value, "
output-text"   i*x j*x = any number of cell values,"
output-text"   a-addr = aligned adrs, c-addr = character adrs"
output-text"   p-addr = I/O port adrs, sys = system-specific."
output-text"   Refer to ANS Forth document for more details."
output-text" "
output-text" ANS Forth Core words"
output-text" --------------------"
output-text" These are required words whose definitions are"
output-text" specified by the ANS Forth document.""
output-text" Supported in CamelForth 8K and 16K ROM."
output-text" "
output-glossary-core

output-text" "
output-text" ANS Forth Extension words"
output-text" -------------------------"
output-text" These are a selection of wordes that are defined"
output-text" in the core-ext and double ANS Forth wordset."
output-text" Supported in CamelForth 16K ROM only."
output-text" "
output-glossary-extensions

output-text" "
output-text" CamelForth Private words"
output-text" ------------------------"
output-glossary-private

close-glossary

bye

