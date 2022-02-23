;      <<<< symass 3.13 >>>>
;       symbolic assembler
; robert huehn june, october 1985
;           august 1986
;             sm3.13
; updates to v3.1 on lines 245, 4170
; 4270, 6090, 6180, 6200/10/40/60/80
; 6420, 6520, & 6550
; a "change/symptr/findptr/,6200-6550"
; will do the last 9 mods
;
;
;zero page equates
                  stasrc =$50       ;start of source
                  stavar =$2d       ;end of source
                  memsiz = $37      ;top of symbol table
                  link = $4e        ;basic line link
                  line = $39        ;current line number
                  ad = $7a          ;current source address
                  symptr = $52      ;symbol value pointer
                  findptr = $62     ;find symbol pointer
                  symend = $57      ;bottom of symbol table
                  ptr = $59         ;current object address
                  opptr = $5b       ;opcode table pointer
                  len = $5d         ;length of word
                  t1 = $26          ;temporary number
                  t2 = $28          ;storage for eval
                  ss = $2a          ;sign save
                  t3 = $5e
                  t4 = $5f
                  t5 = $60
                  flag = $02        ;first or second pass
;constants
                  nops =56         ;number of instructions
                  ready = $a474     ;basic ready
                  inline = $bdc2    ;print 'in line'
                  contbas = $a7ae   ;continue basic
                  list = $a6c9      ;list line
                  findline = $a613  ;find basic line
;
;main program
;
                  init = *          ;begin first pass
;
                  lda #0
                  sta flag
                  ldx #<messstar    ;start
                  ldy #>messstar
                  jsr printmsg
                  ldx $3a
                  inx 
                  bne it1
                  jmp ready         ;since in direct mode
it1               ldx #<messfir     ;first pass
                  ldy #>messfir
                  jsr printmsg
                  lda memsiz        ;init symbol table
                  sta symend
                  lda memsiz+1
                  sta symend+1
                  inc ad
                  bne it2
                  inc ad+1
it2               lda ad
                  sta stasrc
                  sta link
                  lda ad+1
                  sta stasrc+1
                  sta link+1
;
                  newline = *       ;start next line
;
                  jsr nextline
                  bne getword
                  jmp secpass
;
                  getword = *       ;process word
;
                  jsr word
                  bne gw1
                  cmp #$b2          ;= token
                  bne next
                  jmp addval
gw1               ldx #0            ;check for *=*+1
                  lda (ad,x)
                  cmp #$ac          ;* token
                  bne gw2
                  jsr doptr
                  jmp next
gw2               lda (ad),y
                  cmp #$b2          ;=
                  bne gw3
                  jmp addsym
gw3               jsr findop
                  bcc gw4
                  jmp doop
gw4               ldy #0
                  lda (ad),y
                  cmp #"."
                  bne label
                  jmp custop
;
                  label = *         ;save word, current address
;
                  jsr crsym
                  ldy #0
                  lda ptr
                  sta (symptr),y
                  iny 
                  lda ptr+1
                  sta (symptr),y
;
                  next = *          ;get ready for next word
;
                  ldy len
                  lda (ad),y
                  cmp #" "
                  beq n1
                  cmp #":"
                  beq n1
                  jmp newline
n1                iny
                  tya 
                  clc 
                  adc ad
                  sta ad
                  bcc n
                  inc ad+1
n                 jmp getword
;
                  secpass =*       ;begin second pass
;
                  inc flag
                  ldx #<messsec     ;second pass
                  ldy #>messsec
                  jsr printmsg
                  lda stasrc        ;put link at start
                  sta link
                  lda stasrc+1
                  sta link+1
;
                  newline2 = *      ;start next line
;
                  jsr nextline
                  bne getword2
                  jmp finish
;
                  getword2 = *      ;process word
;
                  jsr word
                  beq next2
                  ldx #0
                  lda (ad,x)
                  cmp #$ac          ;*
                  bne g2w1
                  jsr doptr
                  jmp next2
g2w1              jsr findop
                  bcc g2w2
                  jmp putop
g2w2              ldy #0
                  lda (ad),y
                  cmp #"."
                  bne next2
                  jmp custop
;
                  next2 = *         ;get ready for next word
;
                  ldy len
                  lda (ad),y
                  cmp #$20
                  beq n2x1
                  cmp #$3a
                  beq n2x1
                  jmp newline2
n2x1              iny
                  tya 
                  clc 
                  adc ad
                  sta ad
                  bcc n2x
                  inc ad+1
n2x               jmp getword2
;
                  finish = *        ;end
;
                  ldx #<messac      ;assembly complete
                  ldy #>messac
                  jsr printmsg
                  lda ptr
                  sta line
                  lda ptr+1
                  sta line+1
                  jsr inline
                  jmp ready
;
;subroutines used by main program
;
                  addsym = *        ;save symbol with value
;
                  jsr crsym
                  jsr newword
                  jsr eval
                  ldy #0
                  sta (symptr),y
                  iny 
                  txa 
                  sta (symptr),y
                  jmp next
;
                  addval = *        ;change label into symbol
;
                  jsr newword
                  jsr eval
                  ldy #0
                  sta (symptr),y
                  iny 
                  txa 
                  sta (symptr),y
                  jmp next
;
                  crsym = *         ;create symbol table entry
;
                  lda symend        ;lower symend to
                  sec               ;make room
                  sbc #10
                  sta symend
                  bcs cs1
                  dec symend+1
cs1               cmp stavar        ;check for
                  lda symend+1
                  sbc stavar+1
                  bcs cs2
                  ldx #<messsto     ;symbol table
                  ldy #>messsto     ;overflow
                  jsr printmsg
                  jsr inline
                  jmp listline
cs2               clc               ;point symptr to
                  lda symend        ;symbol value address
                  adc #8
                  sta symptr
                  lda symend+1
                  adc #0
                  sta symptr+1
                  ldy #8            ;erase space for name
                  lda #0
cs4               dey
                  sta (symend),y
                  bne cs4
                  ldy len           ;max length is 8
cs5               dey               ;copy symbol name
                  lda (ad),y
                  sta (symend),y
                  tya 
                  bne cs5
                  rts 
;
                  doop = *          ;move ptr past instruction
;
                  ldy #0
                  lda (ad),y
                  cmp #"j"
                  beq do3
                  cmp #"b"
                  bne do1
                  cpx #$21          ;brk op#
                  beq do1
                  cpx #$20          ;bit op#
                  beq do1
                  jsr nextword
dol2              lda #2
                  bne do
do3               jsr nextword
                  lda #3
                  bne do
do1               jsr nextword
                  bne do2
                  lda #1
                  bne do
do2               ldy #0
                  lda (ad),y
                  cmp #"#"
                  beq dol2
                  cmp #"("
                  beq dol2
                  ldy len
                  dey 
                  beq do5
                  dey 
                  beq do5
                  lda (ad),y
                  cmp #","
                  bne do5
                  iny               ;recognize intended absolute
                  lda (ad),y
                  ldy #7
                  cmp #"x"
                  beq do7
                  iny 
do7               lda (opptr),y
                  cmp #$fa
                  beq dol3
                  ldy len
                  dey 
                  dey 
                  sty len
                  jsr eval
                  inc len
                  inc len
                  cpx #0
                  jmp do6
do5               jsr eval
do6               beq dol2
dol3              lda #3
do                clc
                  adc ptr
                  sta ptr
                  bcc do4
                  inc ptr+1
do4               jmp next
;
                  doptr = *         ;change ptr eg. *=*+2
;
                  jsr nextword
                  jsr newword
                  jsr eval
                  sta ptr
                  stx ptr+1
                  rts 
;
;eval routines begin here
;
                  literal = *       ;return single ascii value
;
                  iny 
                  lda (ad),y
                  sta t1
                  lda #0
                  sta t1+1
                  iny 
                  iny 
                  jmp last
;
                  sym = *           ;find end and call findsym
;
sy1               iny
                  cpy len
                  beq sy
                  lda (ad),y
                  cmp #$aa          ;+
                  beq sy
                  cmp #$ab          ;-
                  bne sy1
sy                sty t1
                  jsr findsym
                  ldy t1
                  sta t1
                  stx t1+1
                  jmp last
;
                  eval = *          ;evaluate single expression
;
                  lda #0
                  sta t2
                  sta t2+1
                  sta ss
                  sta t4
ev1               ldy #0
                  lda (ad),y
                  cmp #"$"
                  bne ev8
                  jmp hex
ev8               cmp #$22          ;"
                  beq literal
                  cmp #$ac          ;*
                  beq ptrval
                  cmp #$b1          ;>
                  beq hilo
                  cmp #$b3          ;<
                  beq hilo
                  cmp #"%"
                  bne ev9
                  jmp bin
ev9               sec
                  sbc #$30
                  bcc sym
                  cmp #$0a
                  bcs sym
                  jmp deci
;
                  hilo = *          ;> or < byte extractions
;
                  sta t4
                  inc ad
                  bne hl
                  inc ad+1
hl                dec len
                  bne ev1
;
                  ptrval = *        ;give current address
;
                  iny 
                  lda ptr
                  sta t1
                  lda ptr+1
                  sta t1+1
;
                  last = *          ;perform last sign
;
                  lda ss
                  bne ev3
                  lda t1            ;no sign
                  sta t2
                  lda t1+1
six               sta t2+1
                  jmp sign
ev3               cmp #$aa          ;+
                  bne ev4
                  clc 
                  lda t1
                  adc t2
                  sta t2
                  lda t1+1
                  adc t2+1
                  bcc six
                  jmp iq
ev4               sec               ;- (default)
                  lda t2
                  sbc t1
                  sta t2
                  lda t2+1
                  sbc t1+1
                  sta t2+1
                  bcc iq
;
                  sign = *          ;save sign or stop
;
                  cpy len
                  beq ev
                  lda (ad),y
                  sta ss
                  iny 
                  tya 
                  clc 
                  adc ad
                  sta ad
                  bcc ev5
                  inc ad+1
ev5               sec
                  lda len
                  sty len
                  sbc len
                  sta len
                  jmp ev1
ev                lda t4
                  bne ev6
                  lda t2
                  ldx t2+1
                  rts 
ev6               cmp #$b1          ;>
                  bne ev7
                  lda t2+1
                  ldx #0
                  rts 
ev7               lda t2            ;<
                  ldx #0
                  rts 
;
                  hex = *           ;convert hex number
;
                  iny 
                  lda #0
                  sta t1
                  sta t1+1
hx1               lda (ad),y
                  sec 
                  sbc #$30
                  bcc hx
                  cmp #$0a
                  bcc hx2
                  sbc #$11
                  bcc hx
                  cmp #$06
                  bcs hx
                  adc #$0a
hx2               asl t1
                  rol t1+1
                  bcs iq
                  asl t1
                  rol t1+1
                  bcs iq
                  asl t1
                  rol t1+1
                  bcs iq
                  asl t1
                  rol t1+1
                  bcs iq
                  adc t1
                  sta t1
                  lda t1+1
                  adc #0
                  sta t1+1
                  bcs iq
                  iny 
                  bne hx1
hx                jmp last
;
                  iq = *            ;illegal quanity
                  ldx #<messiq
                  ldy #>messiq
                  jsr printmsg
                  jsr inline
                  jmp listline
;
                  deci =*          ;convert decimal
;
                  lda #0
                  sta t1
                  sta t1+1
de1               lda (ad),y
                  sec 
                  sbc #$30
                  bcc de
                  cmp #$0a
                  bcs de
                  pha 
                  lda t1
                  ldx t1+1
                  asl t1
                  rol t1+1
                  bcs iq
                  asl t1
                  rol t1+1
                  bcs iq
                  adc t1
                  sta t1
                  txa 
                  adc t1+1
                  sta t1+1
                  bcs iq
                  asl t1
                  rol t1+1
                  bcs iq
                  pla 
                  adc t1
                  sta t1
                  lda t1+1
                  adc #0
                  sta t1+1
                  bcs iq
                  iny 
                  bne de1
de                jmp last
;
                  bin = *           ;convert binary
;
                  iny 
                  lda #0
                  sta t1
                  sta t1+1
bn1               lda (ad),y
                  sec 
                  sbc #$30
                  bcc bn
                  cmp #2
                  bcs bn
                  asl t1
                  rol t1+1
                  bcs iq
                  adc t1
                  sta t1
                  lda t1+1
                  adc #0
                  sta t1+1
                  iny 
                  bne bn1
bn                jmp last
;
                  findop = *        ;set carry if opcode,
;opptr points to position,63999
;.x holds opcode number
;
                  lda #<optab       ;opcode table
                  sta opptr
                  lda #>optab
                  sta opptr+1
                  ldx #0
fo1               ldy #0
fo2               lda (opptr),y
                  beq fo3
                  cmp (ad),y
                  bne fo4
                  iny 
                  cpy #3
                  bcc fo2
fo3               cpy len
                  bne fo4
                  sec 
                  rts 
fo4               inx
                  lda opptr
                  clc 
                  adc #$0d
                  sta opptr
                  bcc fo5
                  inc opptr+1
fo5               cpx #nops+1
                  bne fo1
                  clc 
                  rts 
;
                  findsym = *       ;find symbol, return
;          value in .a .x
;
                  lda memsiz
                  sta findptr
                  lda memsiz+1
                  sta findptr+1
fs1               lda findptr
                  sec 
                  sbc #10
                  sta findptr
                  bcs fs2
                  dec findptr+1
fs2               cmp symend
                  lda findptr+1
                  sbc symend+1
                  bcs fs3
                  lda flag
                  bne fs8
                  lda ptr           ;return ptr on 1st pass
                  ldx ptr+1
                  rts 
fs8               ldx #<messus      ;undefined symbol
                  ldy #>messus
                  jsr printmsg
                  jsr inline
                  jmp listline
fs3               ldy #0
fs4               lda (findptr),y
                  beq fs7
                  cmp (ad),y
                  bne fs1
                  iny 
                  cpy #8
                  bcc fs4
fs7               cpy t1            ;length
                  bne fs1
fs9               ldy #9            ;found it
                  lda (findptr),y
                  tax 
                  dey 
                  lda (findptr),y
                  rts 
;
                  listline = *      ;list offending line
;
                  lda line
                  sta $14
                  lda line+1
                  sta $15
                  jsr findline
                  jsr list
                  jmp ready
;
                  nextline = *      ;ready for next line
;
                  lda link          ;move ad to next line
                  sta ad
                  lda link+1
                  sta ad+1
                  ldy #0            ;new link
                  lda (ad),y
                  sta link
                  iny 
                  lda (ad),y
                  sta link+1
                  beq nl            ;end of source return z set
                  iny               ;new line number
                  lda (ad),y
                  sta line
                  iny 
                  lda (ad),y
                  sta line+1
                  clc               ;move ad over link and line
                  lda ad
                  adc #4
                  sta ad
                  bcc nl
                  inc ad+1          ;return z clear
nl                rts
;
                  newword = *       ;get next word past =
;
                  ldy len
                  iny 
                  .byte $2c
;
                  nextword = *      ;get next word
;
                  ldy len
                  tya 
                  clc 
                  adc ad
                  sta ad
                  bcc nw
                  inc ad+1
nw                jmp word
;
                  printmsg = *      ;print message
;
                  stx t1
                  sty t1+1
                  ldy #0
pm1               lda (t1),y
                  beq pm
                  jsr $ffd2         ;print character
                  iny 
                  bne pm1
pm                rts
;
;putop routines begin here
;
                  relative = *      ;calculate offset
;
                  ldy #3
                  lda (opptr),y
                  jsr putoutop
                  jsr nextword
                  jsr eval
                  sec 
                  sbc #1
                  bcs rl1
                  dex 
rl1               sec
                  sbc ptr
                  sta t1
                  txa 
                  sbc ptr+1
                  tax 
                  clc 
                  lda t1
                  adc #$80
                  txa 
                  adc #0
                  beq rl
                  ldx #<messboor    ;branch out of
                  ldy #>messboor    ;range
                  jsr printmsg
                  jsr inline
                  jmp listline
rl                lda t1
                  jsr putout
                  jmp next2
;
                  imm = *           ;do immediate mode '#'
;
                  inc ad
                  bne im1
                  inc ad+1
im1               ldy #$0a
                  lda (opptr),y
                  jsr putoutop
                  dec len
                  jsr eval
                  jsr putout
                  jmp next2
;
                  indirect = *      ;do (,x) else (),y
;
                  inc ad
                  bne ind1
                  inc ad+1
ind1              lda len
                  sec 
                  sbc #4
                  tay 
                  sty len
                  lda (ad),y
                  ldy #11
                  cmp #","
                  beq ind2
                  iny 
ind2              lda (opptr),y
                  jsr putoutop
                  jsr eval
                  jsr putout
                  inc len
                  inc len
                  inc len
                  jmp next2
;
                  putop = *         ;generates machine code
;
                  ldy #0
                  lda (ad),y
                  cmp #"j"
                  bne pop5
                  jmp jump
pop5              cmp #"b"
                  bne pop1
                  cpx #$21          ;brk op#
                  beq pop1
                  cpx #$20          ;bit op#
                  beq pop1
                  jmp relative
pop1              jsr nextword
                  bne pop2
                  ldy #9
                  lda (opptr),y
                  jsr putoutop
                  jmp next2
pop2              ldy #0
                  lda (ad),y
                  cmp #"#"
                  bne pop3
                  jmp imm
pop3              cmp #"("
                  bne pop4
                  jmp indirect
pop4              cmp #"!"
                  bne absolute
; forced absolute by ! prefix
                  inc ad
                  bne fr
                  inc ad+1
fr                dec len
                  .byte $2c
;
                  absolute = *      ;three byte mode
;
                  lda #0
                  sta t5
                  ldx #3
                  ldy len
                  dey 
                  beq ab1
                  dey 
                  beq ab1
                  lda (ad),y
                  cmp #","
                  bne ab1
                  sty len
                  inx 
                  iny 
                  lda (ad),y
                  cmp #"x"
                  beq ab1
                  inx 
ab1               stx t3
                  jsr eval
                  beq ab2
ab4               ldy t3
                  lda (opptr),y
                  jsr putoutop
                  lda t2
                  jsr putout
                  txa 
                  jsr putout
                  jmp ab3
ab2               lda t5
                  bne ab4
                  ldy t3
                  iny 
                  iny 
                  iny 
                  lda (opptr),y
                  cmp #$fa
                  beq ab4
                  jsr putoutop
                  lda t2
                  jsr putout
ab3               ldy t3
                  dey 
                  dey 
                  dey 
                  beq ab
                  inc len
                  inc len
ab                jmp next2
;
                  jump = *          ; jmp, jsr and jmp ()
;
                  jsr nextword
                  ldy #0
                  lda (ad),y
                  cmp #"("
                  beq jp1
                  ldy #3
                  lda (opptr),y
                  jsr putoutop
                  jsr eval
                  jsr putout
                  txa 
                  jsr putout
                  jmp next2
jp1               inc ad
                  bne jp2
                  inc ad+1
jp2               dec len
                  dec len
                  ldy #4
                  lda (opptr),y
                  jsr putoutop
                  jsr eval
                  jsr putout
                  txa 
                  jsr putout
                  inc len
                  jmp next2
;
                  putoutop = *      ;verify op mode
;
                  cmp #$fa
                  bne putout
                  ldx #<messim      ;illegal mode
                  ldy #>messim
                  jsr printmsg
                  jsr inline
                  jmp listline
;
                  putout = *        ;output object code
;
                  ldy #0
                  sta (ptr),y
                  inc ptr
                  bne pt
                  inc ptr+1
pt                rts
;
                  word = *          ;basic routine to get word
;(ad) must point to start
;ignores leading spaces
;"
;":;= " copied only in quote mode
;return .y-length, stop char in .a
;
                  ldx #0
                  ldy #0
w1                lda (ad),y
                  beq w5            ;end of line
                  cmp #$22          ;"
                  beq w4
                  cpx #$80
                  beq w2
                  cmp #":"
                  beq w5
                  cmp #";"
                  beq w5
                  cmp #$b2          ; =
                  beq w5
                  cmp #" "
                  beq w3
w2                iny               ;copy
                  bne w1
w3                cpy #0            ;leading space
                  bne w5
                  inc ad
                  bne w1
                  inc ad+1
                  bne w1
w4                txa               ;toggle
                  eor #$80
                  tax 
                  jmp w2
w5                sty len
                  cpy #0
                  rts 
;
                  custop = *        ;custom pseudo-ops
;
                  iny 
                  lda (ad),y
                  cmp #"b"
                  bne cp1
                  jmp byte
cp1               cmp #"w"
                  bne cp2
                  jmp byte+2
cp2               cmp #$c6          ;asc
                  bne cp3
                  jmp asc
cp3               cmp #$80          ;end
                  bne cp4
                  jmp end
cp4               cmp #"p"
                  bne cp5
                  jmp pad
cp5               ldx #<messip      ;illegal
                  ldy #>messip      ;pseudo-op
                  jsr printmsg
                  jsr inline
                  jmp listline
;
cp                lda flag
                  bne cp6
                  jmp next
cp6               jmp next2
;
                  byte = *          ;.byte and .word
;
                  lda #0
                  sta t5
                  jsr nextword
                  sty t3
by1               ldy #0
by2               lda (ad),y
                  cmp #","          ;split up expressions
                  beq by3
by9               iny
                  cpy t3
                  bne by2
by3               lda flag
                  beq by6
                  sty len
                  iny 
                  lda t3
                  sty t3
                  sec 
                  sbc t3
                  sta t3
                  bcs by4
                  lda #0
                  sta t3
by4               jsr eval          ;eval and
                  jsr putout        ;putout one byte
                  lda t5
                  beq by5
                  txa 
                  jsr putout        ;or one word
by5               lda t3
                  beq by
                  ldy len
                  iny 
                  tya 
                  clc 
                  adc ad
                  sta ad
                  bcc by1
                  inc ad+1
                  bne by1
by6               clc
                  lda t5
                  beq by7
                  lda #1
by7               adc #1
                  adc ptr           ;inc ptr on first pass
                  sta ptr
                  bcc by8
                  inc ptr+1
by8               cpy t3
                  bne by9
by                jmp cp
;
                  asc = *           ;.text
;
                  jsr nextword
                  ldy #1
as1               lda (ad),y
                  cmp #$22          ;"
                  beq as
                  ldx flag
                  beq as3
                  sty t3
                  jsr putout
                  ldy t3
as2               iny
                  cpy len
                  bne as1
as                jmp cp
as3               inc ptr
                  bne as2
                  inc ptr+1
                  bne as2
;
                  end = *           ;.end
;
                  lda flag
                  bne en
                  jmp secpass
en                jsr nextword
                  ldx #<messac
                  ldy #>messac
                  jsr printmsg
                  lda ptr
                  sta line
                  lda ptr+1
                  sta line+1
                  jsr inline
                  jmp contbas
;
                  pad = *           ;pad object with a 0 if at
;odd byte to keep jmp tables safe
                  lda ptr
                  and #1
                  beq pa
                  lda flag
                  beq pa1
                  lda #0
                  jsr putout
pa                jmp cp
pa1               inc ptr
                  bne pa
                  inc ptr+1
                  bne pa
;
                  optab = *         ;opcode table
;
                  .text "lda"
                  .byte $ad,$bd,$b9,$a5,$b5,$fa,$fa,$a9,$a1,$b1
                  .text "sta"
                  .byte $8d,$9d,$99,$85,$95,$fa,$fa,$fa,$81,$91
                  .text "bne"
                  .byte $d0,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa
                  .text "beq"
                  .byte $f0,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa
                  .text "cmp"
                  .byte $cd,$dd,$d9,$c5,$d5,$fa,$fa,$c9,$c1,$d1
                  .text "jsr"
                  .byte $20,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa
                  .text "ldx"
                  .byte $ae,$fa,$be,$a6,$fa,$b6,$fa,$a2,$fa,$fa
                  .text "rts"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$60,$fa,$fa,$fa
                  .text "ldy"
                  .byte $ac,$bc,$fa,$a4,$b4,$fa,$fa,$a0,$fa,$fa
                  .text "bmi"
                  .byte $30,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa
                  .text "dec"
                  .byte $ce,$de,$fa,$c6,$d6,$fa,$fa,$fa,$fa,$fa
                  .byte $af,0,0,$2d,$3d,$39,$25,$35,$fa,$fa,$29,$21,$31 ;and
                  .text "bcs"
                  .byte $b0,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa
                  .text "inc"
                  .byte $ee,$fe,$fa,$e6,$f6,$fa,$fa,$fa,$fa,$fa
                  .text "bcc"
                  .byte $90,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa
                  .text "tya"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$98,$fa,$fa,$fa
                  .text "bpl"
                  .byte $10,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa
                  .text "asl"
                  .byte $0e,$1e,$fa,$06,$16,$fa,$0a,$fa,$fa,$fa
                  .text "clc"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$18,$fa,$fa,$fa
                  .text "adc"
                  .byte $6d,$7d,$79,$65,$75,$fa,$fa,$69,$61,$71
                  .byte $45,$b0,0,$4d,$5d,$59,$45,$55,$fa,$fa,$49,$41,$51                 ;eor
                  .text "txa"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$8a,$fa,$fa,$fa
                  .text "cpx"
                  .byte $ec,$fa,$fa,$e4,$fa,$fa,$fa,$e0,$fa,$fa
                  .text "jmp"
                  .byte $4c,$6c,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa
                  .text "tax"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$aa,$fa,$fa,$fa
                  .text "iny"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$c8,$fa,$fa,$fa
                  .text "sty"
                  .byte $8c,$fa,$fa,$84,$94,$fa,$fa,$fa,$fa,$fa
                  .byte $b0,$41,0,$0d,$1d,$19,$05,$15,$fa,$fa,$09,$01,$11                 ;ora
                  .text "dey"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$88,$fa,$fa,$fa
                  .text "dex"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$ca,$fa,$fa,$fa
                  .text "stx"
                  .byte $8e,$fa,$fa,$86,$fa,$96,$fa,$fa,$fa,$fa
                  .text "sbc"
                  .byte $ed,$fd,$f9,$e5,$f5,$fa,$fa,$e9,$e1,$f1
                  .text "bit"
                  .byte $2c,$fa,$fa,$24,$fa,$fa,$fa,$fa,$fa,$fa
                  .text "brk"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$00,$fa,$fa,$fa
                  .text "bvc"
                  .byte $50,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa
                  .text "bvs"
                  .byte $70,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa,$fa
                  .text "cld"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$d8,$fa,$fa,$fa
                  .text "cli"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$58,$fa,$fa,$fa
                  .text "clv"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$b8,$fa,$fa,$fa
                  .text "cpy"
                  .byte $cc,$fa,$fa,$c4,$fa,$fa,$fa,$c0,$fa,$fa
                  .text "inx"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$e8,$fa,$fa,$fa
                  .text "lsr"
                  .byte $4e,$5e,$fa,$46,$56,$fa,$4a,$fa,$fa,$fa
                  .text "nop"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$ea,$fa,$fa,$fa
                  .text "pha"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$48,$fa,$fa,$fa
                  .text "php"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$08,$fa,$fa,$fa
                  .text "pla"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$68,$fa,$fa,$fa
                  .text "plp"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$28,$fa,$fa,$fa
                  .text "rol"
                  .byte $2e,$3e,$fa,$26,$36,$fa,$2a,$fa,$fa,$fa
                  .byte $52,$b0,0,$6e,$7e,$fa,$66,$76,$fa,$6a,$fa,$fa,$fa                 ;ror
                  .text "rti"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$40,$fa,$fa,$fa
                  .text "sec"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$38,$fa,$fa,$fa
                  .text "sed"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$f8,$fa,$fa,$fa
                  .text "sei"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$78,$fa,$fa,$fa
                  .text "tay"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$a8,$fa,$fa,$fa
                  .text "tsx"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$ba,$fa,$fa,$fa
                  .text "txs"
                  .byte $fa,$fa,$fa,$fa,$fa,$fa,$9a,$fa,$fa,$fa
;
;symass messages
;
messstar          .text "{rvon}symass 3.10 robert huehn feb 1986"
                  .byte 13,0
messfir           .byte 13
                  .null "first pass..."
messsec           .null "second pass..."
messac            .byte 13
                  .null "assembly complete"
messsto           .byte 13
                  .null "symbol table overflow"
messiq            .byte 13
                  .null "illegal quantity"
messus            .byte 13
                  .null "undefined symbol"
messboor          .byte 13
                  .null "branch out of range"
messim            .byte 13
                  .null "illegal mode"
messip            .byte 13
                  .null "illegal pseudo-op"
