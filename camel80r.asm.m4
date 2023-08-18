; RC2014 Extension Words
;
; ===============================================
; CamelForth for the Zilog Z80
; Copyright (c) 1994,1995 Bradford J. Rodriguez
; Copyright (c) 2020 Justin Skists
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

; Commercial inquiries should be directed to the author at 
; 115 First St., #105, Collingwood, Ontario L9Y 4W3 Canada
; or via email to bj@camelforth.com
;
; ===============================================
; RC2014 Extension Words
; 
;   Forth words are documented as follows:
;*   NAME     stack -- stack    description
;   Word names in upper case are from the ANS
;   Forth Core word set.  Names in lower case are
;   "internal" implementation words & extensions.
; ===============================================

EXTERN _hexload

;C HEXLOAD           --    call Hexloader
    head(HEXLOAD,HEXLOAD,docode)
        push bc
        push de
        call _hexload
        pop de
        pop bc
        next
        
        
;C CALL       a-addr --    call machine code at address
    head(CALL,CALL,docode)
        push ix
        push iy
        push hl
        push de

        ; protect against some stack abuse
        ld (call_stack_save), sp

        ld hl, call_exit  ; return address
        push hl
        ld h,b
        ld l,c
        jp (hl)

call_exit:
        ld sp, (call_stack_save)
        pop de
        pop hl
        pop iy
        pop ix

        pop bc   ; DROP the address from TOS
        next


SECTION data_user

call_stack_save:
        DEFW  call_stack_save

SECTION code_user
