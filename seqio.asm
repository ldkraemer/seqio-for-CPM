

  ;      sequential file i/o library
  ;
  filerr set     0000h   ;reboot after error
  @bdos  equ     0005h   ;bdos entry point
  @tfcb  equ     005ch   ;default file control block
  @tbuf  equ     0080h   ;default buffer address
  ;
  ;      bdos functions
  @msg   equ     9       ;send message
  @opn   equ     15      ;file open
  @cls   equ     16      ;file close
  @dir   equ     17      ;directory search
  @del   equ     19      ;file delete
  @frd   equ     20      ;file read operation
  @fwr   equ     21      ;file write operation
  @mak   equ     22      ;file make
  @ren   equ     23      ;file rename
  @dma   equ     26      ;set dma address
  ;
  @sect  equ     128     ;sector size
  eof    equ     1ah     ;end of file
  cr     equ     0dh     ;carriage return
  lf     equ     0ah     ;line feed
  tab    equ     09h     ;horizontal tab
  ;
  @key   equ     1       ;kevboard
  @con   equ     2       ;console display
  @rdr   equ     3       ;reader
  @Pun   equ     4       ;Punch
  @lst   equ     5       ;list device
  ;
  ;      keywords for "file" macro
  infile equ     1       ;input file
  outfile        equ     2       ;outputfile
  setfile        equ     3       ;setup name only
  ;
  ;      the following macros define simple sequential
  ;      file operations:
  ;
  fillnam        macro   fc,c
  ;;     fill the file name/type given bY fc for c characters
  @cnt   set     c       ;;max length
         irpc    ?fc,fc  ;;fill each character
  ;;     may be end of count or nul name
         if      @cnt=O or nul ?fc
         exitm
         endif
         db      '&?FC'  ;;fill one more
  @cnt   set     @cnt-1  ;;decrement max length
        endm             ;;of irPc ?fc
  ;;
  ;;    Pad remainder
        rept     @cnt    ;;@cnt is remainder
        db       ' '     ;;pad one more blank
        endm             ;;of rept
        endm
  ;
  filldef        macro   fcb,?fl,?1n
  ;;    fill the file name from the default fcb
  ;;    for length ?1n (9 or 12)
        local    psub
        jmp      psub    ;;jump past the subroutine
  @def: ;;this subroutine fills from the tfcb (+16)
        mov      a,m     ;;get next character to a
        stax     d       ;;store to fcb area
        inx      h
        inx      d
        dcr      c       ;;count length down to 0
        jnz      @def
        ret
 ;;     end of fill subroutine
 psub:
 filldef         macro   ?fcb,?f,?1
        lxi     h,@tfcb+?f      ;;either @tfcb or @tfcb+16
        lxi     d,?fcb
        mvi     c,?1            ;;length = 9,12
        call    @def
        endm
        filldef fcb,?f1,?1n
        endm
 ;
 fillnxt        macro
 ;;      initialize buffer and device numbers
 @nxtb   set     0       ;;next buffer location
 @nxtd   set     @1st+1  ;;next device number
 fillnxt         macro
         endm
         endm
 fillfcb         macro   fid,dn,fn,ft,bs,ba
 ;;      fill the file control block with disk name
 ;;      fid is an internal name for the file,
 ;;      dn is the drive name (a,b..), or blank
 ;;      fn is the file name, or blank
 ;;      ft is the file type
 ;;      bs is the buffer size
 ;;      ba is the buffer address
         local   pfcb
 ;;
 ;;      set uo the file control block for the file
 ;;      look for file name = 1 or 2
 @c      set     1       ;;assume true to begin with
         irpc    ?c,fn   ;;look through characters of name
         if      not ('&?C' = '1' or '&?C' = '2')
 @c      set     0       ;;clear if not 1 or 2
         endm
 ;;      @c is true if fn = 1 or 2 at this point
         if      @c      ;;then fn = 1 or 2
 ;;      fill from default area
         if      nul ft  ;;type specified?
 @c      set     12      ;;both name and type
         else
 @c      set     9       ;;name only
         endif
         filldef fcb&fid,(fn~1)*16,@c    ;;to select the fcb
         jmp     pfcb    ;;past fcb definition
         ds      @c      ;;space for drive/filename/type
         fillnam ft,12-@c        ;;series of db's
         else
         jmp     pfcb    ;;past initialized fcb
         if      nul dn
         db      0       ;;use default drive if name is zero
         else
         db      '&DN'-'A'+1     ;;use specified drive
         endif
         fillnam fn,8    ;;fill file name
  ;;     now generate the file type with padded blanks
         fillnam ft,3    ;;and three character type
         endif
  fcb&fid        equ     $-12    ;;beginning of the fcb
         db      0       ;;extent field 00 for setfile
  ;;     now define the 3 byte field, and disk map
         ds      20      ;;x,x,rc,dm0...dm15,cr fields
  ;;
         if      fid&typ<=2       ;;in/outfile
  ;;     generate constants for infile/outfile
         fillnxt         ;;@nxtb=0 on first call
         if      bs+0<@sect
  ;;     bs not supplied, or too small
  @bs    set     @sect   ;;default to one sector
         e1se
  ;;     compute even buffer address
  @bs    set     (bs/@sect)*@sect
         endif
  ;;
  ;;     now define buffer base address
         if      nul ba
  ;;     use next address after @nxtb
  fid&buf        set     buffers+@nxtb
  ;;     count past this buffer
  @nxtb  set     @nxtb+ bs
         else
  fid&buf        set     ba
         endif
  ;;     fid&buf is buffer address
  fid&adr:
        dw       fid&buf
 ;;
 fid&siz        equ      @bs     ;;literal size
 fid&len:
        dw      @bs      ;;buffer size
 fid&ptr:
        ds      2        ;;set in infile/outfile
 ;;     set device number
 @&fid  set     @nxtd    ;;next device
 @nxtd  set     @nxtd+1
        endif   ;;of fid&typ<=2 test
 pfcb:  endm
 ;;
 file   macro   md,fid,dn,fn,ft,bs,ba
 ;;     create file using mode md:
 ;;             infile = 1      input file
 ;;             outfile = 2     output file
 ;;             setfile = 3     setup fcb
 ;;     (see fillfcb for remaining parameters)
        local   psub,msg,pmsg
        local   pnd,eod,eob,pnc
 ;;     construct the file control block
 ;;
                                                                                                           .
 fid&typ        equ     md      ;;set mode for later ref's
        fillfcb fid,dn,fn,ft,bs,ba
        if      md=3    ;;setup fcb only, so exit
        exitm
        endif
 ;;     file control block and related parameters
 ;;     are created inline, now create io function
        jmp     psub    ;;Past inline subroutine
        if      md=1    ;;input file
 get&fid:
        else
 put&fid:
        push     psw    ;;save output character
        endif
        lhld     fid&len ;;load current buffer length
        xchg             ;;de is length
        lhld     fid&ptr ;;load next to get/put to hl
        mov      a,1     ;;compute cur-len
        sub      e
        mov      a,h
        sbb      d       ;;carry if next<length
        jc       pnc     ;;carry if len gtr current
 ;;     end of buffer, fill/empty buffers
        lxi      h,0
        shld     fid&ptr ;;clear next to get/put
 pnd:
 ;;      process next disk sector:
         xchg            ;;fid&ptr to de
         lhld    fid&len ;;do not exceed length
 ;;      de is next to fill/empty, hl is max len
         mov     a,e     ;;compute next-len
         sub     1       ;;to get carry if more
         mov     a,d
         sbb     h       ;;to fill
         jnc     eob
 ;;      carry gen'ed, hence more to fill/empty
         lhld    fid&adr ;;base of buffers
         dad     d       ;;hl is next buffer addr
         xchg
         mvi     c,@dma  ;;set dma address
         call    @bdos   ;;dma address is set
         lxi     d,fcb&fid       ;;fcb address to de
         if      md=1    ;;read buffer function
         mvi     c,@frd  ;;file read function
         else
         mvi     c,@fwr  ;;file write function
         endif
         call    @bdos   ;;rd/wr to/from dma address
         ora     a       ;;check return code
         jnz     eod     ;;end of file/disk?
  ;;     not end of file/disk, increment length
         lxi     d,@sect ;;sector size
         lhld    fid&ptr ;;next to fill
         dad     d
         shld    fid&ptr ;;back to memory
         jmp     pnd     ;;process another sector
  ;;
  eod:
  ;;     end of file/disk encountered
         if      md=1    ;;input file
         lhld    fid&ptr ;;length of buffer
         shld    fid&len ;;reset length
         else
  ;;     fatal error, end of disk
         local   emsg
         mvi     c,@msg  ;;write the error
         lxi     d,emsg
         call    @bdos   ;;error to console
         pop     psw     ;;remove stacked character
         jmp     filerr  ;;usuallY reboots
  emsg:  db      cr,lf
         db      'disk full: &FID'
         db      '$'
         endif
  ;;
  eob:
  ;;     end of buffer, reset dma and pointer
         lxi     d,@tbuf
         mvi     c,@dma
         call    @bdos
         lxi     h,0
         shld    fid&ptr ;;next to get
  ;;
  pnc:
  ;;     process the next character
         xchg            ;;index to get/put in de
         lhld    fid&adr ;;base of buffer
         dad     d       ;;address of char in hl
         xchg            ;;address of char in de
         if      md=1    ;;input processing differs
         lhld    fid&len ;;for eof check
         mov     a,l     ;;0000?
         ora     h
         mvi     a,eof   ;;end of file?
         rz              ;;zero flag if so
         ldax    d       ;;next char in accum
         else
  ;;     store next character from accumulator
         pop     psw     ;;recall saved char
         stax    d       ;;character in buffer
         endif
         lhld    fid&ptr ;;index to get/put
         inx     h
         shld    fid&ptr ;;pointer updated
  ;;     return with non zero flag if get
         ret
  ;;
  psub:  ;;past inline subroutine
         xra     a               ;;zero to acc
         sta     fcb&fid+12      ;;clear extent
         sta     fcb&fid+32      ;;clear cur rec
         lxi     h,fid&siz       ;;buffer size
         shld    fid&len         ;;set buff len
         if      md=1    ;;input file
         shld    fid&ptr ;;cause immediate read
         mvi     c,@opn  ;;open file function
         else            ;;output file
         lxi     h,0     ;;set next to fill
         shld    fid&ptr ;;pointer initialized
         mvi     c,@del
         lxi     d,fcb&fid       ;;delete file
         call    @bdos   ;;to clear existing file
         mvi     c,@mak  ;;create a new file
         endif
  ;;     now open (if input), or make (if output)
         lxi     d,fcb&fid
         call    @bdos   ;;open/make ok?
         inr     a       ;;255 becomes 00
         jnz     pmsg
         mvi     c,@msg  ;;print message function
         lxi     d,msg   ;;error message
         call    @bdos   ;;printed at console
         jmp     filerr  ;;to restart
  msg:   db      cr,lf
         if      md=1    ;;input message
         db      'no &FID file'
         else
         db      'no dir space: &FID'
         endif
         db      '$'
  pmsg:
         endm
  ;;
  finis  macro   fid
  ;;     close the file(s) given by fid
         irp     ?f,<fid>
  ;;     skip all but output files
         if      ?f&typ=2
         local   eob?,peof,msg,pmsg
  ;;     write all partially filled buffers
  eob?:  ;;are we at the end of a buffer?
         lhld    ?f&ptr  ;;next to fill
         mov     a,l     ;;on buffer boundarY?
         ani     (@sect-1) and 0ffh
         jnz     peof    ;;put eof if not 00
         if      @sect>255
  ;;     check high order byte also
         mov     a,h
         ani     (@sect-1) shr 8
         jnz     peof    ;;put eof if not 00
         endif
  ;;     arrive here if end of buffer, set length
  ;;     and write one more byte to clear buffs
         shld    ?f&len  ;;set to shorter length
  peof:  mvi     a,eof   ;;write another eof
         push    psw     ;;save zero flag
         call    put&?f
         pop     psw     ;;recall zero flag
         jnz     eob?    ;;non zero if more
  ;;     buffers have been written, close file
         mvi     c,@cls
         lxi     d,fcb&?f        ;;ready for call
         call    @bdos
         inr     a       ;;255 if err becomes 00
         jnz     pmsg
  ;;     file cannot be closed
         mvi     c,@msg
         lxi     d,msg
         call    @bdos
         jmp     pmsg    ;;error message printed
  msg:   db      cr,lf
         db      'cannot close &?F'
         db      '$'
  pmsg:
         endif
         endm    ;;of the irp
         endm
  ;
  erase  macro   fid
  ;;     delete the file(s) given bY fid
         irp     '?f,<fid>
         mvi     c,@del
         lxi     d,fcb&?f
         call    @bdos
         endm    ;;of the irp
         endm
  ;
  direct macro   fid
  ;;     perform directory search for file
  ;;     sets zero flag if not present
         lxi     d,fcb&fid
         mvi     c,@dir
         call    @bdos
         inr     a       ;00 if not Present
         endm
   ;
  rename macro   new,old
  ;;     rename file given by "old" to "new"
         local   psub,ren0
  ;;     include the rename subroutine once
         jmp     psub
  @rens: ;;rename subroutine, hl is address of
         ;;old fcb, de is address of new fcb
         push    h       ;;save for rename
         lxi     b,16    ;;b=00,c=16
         dad     b       ;;hl = old fcb+16
  ren0:  ldax    d       ;;new fcb name
         mov     m,a     ;;to old fcb+16
         inx     d       ;;next new char
         inx     h       ;;next fcb char
         dcr     c       ;;count down from 16
         jnz     ren0
  ;;     old name in first half, new in second half
         pop     d       ;;recall base of old name
         mvi     c,@ren  ;;rename function
         call    @bdos
         ret             ;;rename complete
  psub:
  rename macro   n,o     ;;redefine rename
         lxi     h,fcb&o ;;old fcb address
         lxi     d,fcb&n ;;new fcb address
         call    @rens   ;;rename subroutine
         endm
         rename  new,old
         endm
  ;
  get    macro   dev
  ;;     read character from device
         if      @&dev <=@1st       
  ;;     simple input
         mvi     c,@&dev
         call    @bdos
         else
         call    get&dev
         endm
 ;
 ;
 put    macro   dev
 ;;     write character from accum to device
        if      @&dev <= @1st
 ;;     simple output
        push    psw     ;;save character
        mvi     c,@&dev ;;write char function
        mov     e,a     ;;ready for output
        call    @bdos   ;;write character
        pop     psw     ;;restore for testing
        else
        call    put&dev
        endm


