	SECTION	APPLEII,CODE
	EVEN
**************** OPEN EVERYTHING *******************
OpenAll	move.l	a0,StartA0			;Command line info...
	move.l	d0,StartD0

	move.l	#VarList,a0			;clear val's for resident run!
	move.l	#EndVarList,a1
	moveq.l	#0,d0
.loop	move.l	d0,(a0)+
	cmp.l	a1,a0
	blo.b	.loop	

	CALLEXEC CacheClearU			;stop crashing after recompile?
	
	lea	dosname,a1			;Open DOS Library!
	moveq	#0,d0
	CALLEXEC OpenLibrary
	move.l	d0,_DOSBase
	beq	exit	;cant print, so exit

	CALLDOS	Output				;and get channel for console
	move.l	d0,MyChannel
	beq	exit				;no channel? WB?	


	move.l	4.w,a6				;check for at least 68020 status!
	move.w	AttnFlags(a6),d0
	btst.l	#1,d0
	bne.b	.020
	jmp	Fail020

.020	lea	grafname,a1			;open Graphics Lib!
	moveq.l	#0,d0
	CALLEXEC OpenLibrary
	move.l	d0,_GfxBase
	beq	fail

	lea	intname,a1			;open Intuition Lib!
	moveq	#0,d0
	CALLEXEC OpenLibrary
	move.l	d0,_IntuitionBase
	beq	fail

	lea	reqname,a1			;open reqtools lib!
	moveq	#0,d0
	CALLEXEC OpenLibrary
	move.l	d0,_REQBase
	beq	fail

	lea	potgoname,a1			;open "potgo.resource" lib, no need to close...
	CALLEXEC OpenResource
	move.l	d0,_PotgoBase
	beq	fail
						;Set up for Joystick 2nd button...
	move.l	#$c000,d0			;we want bits #14 & bit #15... (DATRY & OUTRY)
	CALLPOTGO AllocPotBits		
	move.l	d0,PotBits

	move.l	#$c000,d0
	move.l	PotBits,d1			;mask
	CALLPOTGO WritePotgo			;set output lines hi to read joy 2nd btn...


	move.l	#InstLookUpTableMem,InstTbl_Var	;alloc 256k ram!+4 bytes for inst lookup!!!

	move.l	#HiResDrawTableMem,HiResDrawTbl	;alloc 64k hires draw tbl lookup!!!

	move.l	#DiskBufferMem,disk_Buffer	;alloc 215K ram for disk buffer
	move.l	disk_Buffer,disk_TkStrt

	move.l	#RT_REQINFO,d0			;alloc RT_REQINFO structure...
	move.l	#0,a0
	CALLREQ	rtAllocRequestA
	move.l	d0,EasyReq
	beq	exit	;(failed)

	move.l	#RT_FILEREQ,d0			;alloc RT_FILEREQ structure...
	move.l	#0,a0
	CALLREQ	rtAllocRequestA
	move.l	d0,FileReq
	beq	exit	;(failed)

	move.l	FileReq,a1
	lea	.TagLst,a0
	CALLREQ	rtChangeReqAttrA

	lea	.StartBody,a1
	lea	.StartGadg,a2
	move.l	EasyReq,a3
	move.l	#0,a4
	lea	.StartTag,a0
	CALLREQ	rtEZRequestA	
	
	bra	.nxt

.Pat	dc.b	"(#?.disk|#?.prog|#?>)",0
.TagLst	dc.l	RTFI_MatchPat,.Pat,TAG_DONE	

.StartBody	dc.b	"******* APPLE 2000 v1.3 *******",10,10
   IIF   DEBUG	dc.b	"***DEBUGGING***",10
		dc.b	"The Premier Apple II emulator",10
		dc.b	"Copyright © 1994 by Kevin Kralian",10,10

		dc.b	"(No, this is not part of the Emplant",10
		dc.b	"package as many have been led to believe.)",0
.StartGadg	dc.b	"_Ok",0
.StartTag	dc.l	RT_ReqPos,REQPOS_TOPLEFTSCR,RT_Underscore,'_'
		dc.l	RTEZ_Flags,EZREQF_NORETURNKEY!EZREQF_LAMIGAQUAL!EZREQF_CENTERTEXT
		dc.l	TAG_DONE

		dc.b	"NOTICE: Although this program is distributed as Freeware, "
		dc.b	"copyright laws & protection still apply. "
		dc.b	"As such, *any* infringement upon this code, especially as applied "
		dc.b	"towards other 6502 or Apple II emulations will be rigorously "
		dc.b	"pursued via legal channels. -Kevin Kralian",0,0,0

	EVEN
.nxt	move.l	#65545,d0			;alloc 64k ram!+9 bytes (for PCount overflow)
	move.l	#MEMF_PUBLIC!MEMF_CLEAR,d1
	CALLEXEC AllocMem
	move.l	d0,Mem_PtrVar
	beq	fail

	move.l	d0,a0

	move.l	#$c100,d0
.FF	move.b	#$ff,(a0,d0.l)			;put FF's in slot Rom areas (so look empty)
	addq.l	#1,d0
	cmp.l	#$c800,d0
	blo.b	.FF

	move.b	#$81,(a0,$c020.l)		;Beyond Wolfenstein hangs w/o it
	move.b	#$81,(a0,$c060.l)		;Serpentine hangs w/o it
;	move.b	#$60,(a0,$c100.l)		;an RTS when attempts pr#1
;	move.b	#$60,(a0,$c300.l)		;an RTS when attempts pr#3

	move.l	#$7000,d0			;alloc 28K for Rom & Ram banks
	move.l	#MEMF_PUBLIC!MEMF_CLEAR,d1
	CALLEXEC AllocMem
	move.l	d0,Bank_PtrVar
	beq	fail

;----- Open & set up Con device... ----
	move.l	#IOSTD_SIZE,d0			;alloc mem for IOStdReq...
	move.l	#MEMF_PUBLIC!MEMF_CLEAR,d1
	CALLEXEC AllocMem
	move.l	d0,ConReq
	beq	fail

	lea	conname,a0
	move.l	#-1,d0
	move.l	ConReq,a1
	move.l	#0,d1
	CALLEXEC OpenDevice
	tst.l	d0
	bne	fail				;0 is successful
	
	move.l	ConReq,a0
	move.l	IO_DEVICE(a0),_ConBase		;get vector base... (unlike libraries)

	lea	InputStrct,a0
	move.b	#IECLASS_RAWKEY,ie_Class(a0)	;set up static struct...



* Note: Passing args from Devpac supresses last $0A character (NL)... Debugger/Shell don't.
* Normally, a "-2" command is passed as length: 3, data: '-2' + $0a

	cmp.l	#2,StartD0			;# of chars after filename... (inc. NL char)
	blo.b	.noArg

	move.l	StartA0,a0

	cmp.w	#'-2',(a0)+			;is there a -2 option???
	bne.b	.noArg
	cmp.l	#2,StartD0
	beq.b	.SecondDrive			;no more chars... is "-2"
	cmp.b	#$0a,(a0)
	beq.b	.SecondDrive			;next char is NL char... is "-2"
	cmp.b	#' ',(a0)
	beq.b	.SecondDrive			;next char is Space... is "-2"
	bra	.noArg				;else NOT "-2"

.SecondDrive
	lea	.2ndmsg,a0			;diagnostic "2nd drive" msg
	jsr	DB_String

	move.l	#disk_TrackLen*35,d0		;alloc 215K ram for disk buffer
	move.l	#MEMF_PUBLIC!MEMF_CLEAR,d1
	CALLEXEC AllocMem
	move.l	d0,disk_Buffer2
	beq	fail

	bra	.noArg

.2ndmsg	dc.b	"2nd drive option selected.",10,13,0
;DiskBufferMem		ds.b	disk_TrackLen*35	;alloc 215K ram for disk buffer


*-----------------------------------------------------*

.noArg	move.l	#RomName,d1			;Load Rom Image
	move.l	#MODE_OLDFILE,d2
	CALLDOS	Open
	move.l	d0,d4	;temp
	beq	failrom

	move.l	d4,d1				;file handle
	move.l	InstTbl_Var,d2			;Load Image temporarily into InstTbl buffer
	move.l	#$3080,d3			;read $3080 bytes ($d000-$ffff + $80 prodos hdr)
	CALLDOS	Read
	
	move.l	d4,d1				;Done loading Rom Image
	CALLDOS	Close

	move.l	InstTbl_Var,a0
	cmp.w	#$0a47,(a0)			;first 2 bytes in ProDos header...
	bne	.NotProDos
	cmp.b	#$4c,2(a0)			;Third byte in ProDos header...
	bne	.NotProDos

.ProDos	lea	$80(a0),a0			;Rom image begins $80 bytes later...
	bra	.DoIt

.NotProDos
	cmp.w	#$00d0,(a0)			;Dos 3.3 test (check is A$=$d000)
	bne	.NotDos3			;if neither, assume raw image...

.Dos33	lea	$4(a0),a0			;Rom image begins after 4 byte header

.NotDos3					;assume raw image, no header...

						;a0 -> Rom Image	
.DoIt	move.l	Mem_PtrVar,a1	
	lea	(a1,$d000.l),a1			;a1 -> $d000 in apple memory
	move.l	Bank_PtrVar,a2
	lea	(a2,$4000.l),a2			;a2 -> Rom Buffer (backup)

	move.l	#$3000/4-1,d0
.CpyRom	move.l	(a0),(a1)+	
	move.l	(a0)+,(a2)+
	dbf	d0,.CpyRom

*-----------------------------------------------------*

LoadDiskRom:
	move.l	#DiskRomName,d1			;Load Disk Rom Image (If available)
	move.l	#MODE_OLDFILE,d2
	CALLDOS	Open
	move.l	d0,d4	;temp

	lea	MyDiskRom,a0			;if no file, copy MyDiskRom image instead...
	beq	.CopyMem

	move.l	d4,d1				;file handle
	move.l	InstTbl_Var,d2			;Load Image temporarily into InstTbl buffer
	move.l	#$0180,d3			;read $180 bytes ($c600-$c6ff + $80 prodos hdr)
	CALLDOS	Read	
		
	move.l	d4,d1				;Done loading Disk Rom Image
	CALLDOS	Close

.CheckProDos
	move.l	InstTbl_Var,a0
	cmp.w	#$0a47,(a0)			;first 2 bytes in ProDos header...
	bne	.CheckDos33
	cmp.b	#$4c,2(a0)			;Third byte in ProDos header...
	bne	.CheckDos33

.ProDos	lea	$80(a0),a0			;Rom image begins $80 bytes later...
	bra	.CopyMem

.CheckDos33
	cmp.w	#$00c6,(a0)			;Dos 3.3 test (check if A$=$c600)
	bne	.NotDos3			;if neither, assume raw image...

.Dos33	lea	$4(a0),a0			;Rom image begins after 4 byte header

.NotDos3					;assume raw image, no header...

.CopyMem:					;a0 -> Rom Image	
	move.l	Mem_PtrVar,a1
	lea	(a1,$c600.l),a1			;a1 -> $c600 in apple memory

	move.l	#$100/4-1,d0
.CpyR2	move.l	(a0)+,(a1)+
	dbf	d0,.CpyR2

*-----------------------------------------------------*

	lea	MyNewScreen,a0			;Open my screen!
	CALLINT	OpenScreen
	move.l	d0,MyScreen
	beq	fail

;	move.l	MyScreen,a0				;Set main screen color table to WB defaults
;	move.l	sc_ViewPort+vp_ColorMap(a0),a2
;	lea	MainColorTable,a3
;	move.l	a2,a0
;	moveq.l	#0,d0
;	CALLGRAF GetRGB4
;	move.w	d0,(a3)+
;	move.l	a2,a0
;	moveq.l	#1,d0
;	CALLGRAF GetRGB4
;	move.w	d0,(a3)+
;	move.l	a2,a0
;	moveq.l	#2,d0
;	CALLGRAF GetRGB4
;	move.w	d0,(a3)+
;	move.l	a2,a0
;	moveq.l	#3,d0
;	CALLGRAF GetRGB4
;	move.w	d0,(a3)+

	move.l	MyScreen,a0
	moveq.l	#0,d0					;We have to hide screen title bar so we can receive
	CALLINT ShowTitle				;all mouse button clicks into our window...
	
*-----------------------------------------------------*
	move.l	MyScreen,a0
	lea	sc_BitMap(a0),a0			;a1 -> Screen BitMap
	move.l	bm_Planes(a0),OrigPlane1		;plug in BitMap Plane Ptrs
	move.l	bm_Planes+4(a0),OrigPlane2
	move.l	bm_Planes+8(a0),OrigPlane3
	move.l	bm_Planes+12(a0),OrigPlane4
	move.l	bm_Planes+16(a0),OrigPlane5

	move.w	bm_Rows(a0),OrigRows
	move.b	bm_Depth(a0),OrigDepth

*-----------------------------------------------------*

;	CALLEXEC Forbid
;	
;	move.l	MyScreen,a0			;Show my new screen
;	CALLINT ScreenToFront			;and help ourselves to view fields...
;
;*  Compute screen refresh (hz) with following formula:
;*
;*    1 Line    1 Screen     1 CClk    1000000000 uSecs     zz Screens
;*   -------- * -------- * --------- * ----------------  =  ----------
;*   xx CClks   yy Lines   280 uSecs       1 Second          1 second
;
;	moveq.l	#0,d4
;	move.l	_GfxBase,a0
;	move.w	gb_current_tot_cclks(a0),d4	;d4 = CurrentTotColorClks (1 clk=280 uSec)
;	moveq.l	#0,d5
;	move.w	gb_MaxDisplayRow(a0),d5		;d5 = Current Tot Rows
;
;	mulu.w	#280,d5				;*** Compute FrameRate (Hz) ***
;	mulu.l	d4,d5				;Hertz = 1,000,000,000/(CClks*TotRow*280)
;	move.l	#1000000000,d0
;	divu.l	d5,d0				;d0 = hertz
;	
;	move.l	#1026000,d1			;Apple cycles/frame = 1,024,000 / Hz (+some)
;	divu.l	d0,d1				;d1 = Apple CyclesPer Frame!
;
;	move.l	d1,RealCycPerFrame
;	jsr	DB_HexL
;
;	CALLEXEC Permit

*---------------------------  Open CIA Timer & setup  --------------------------*

CIASetUp:
	move.l	#-1,CIAAllocBit

	lea	ciaBname,a1			;Open CIA-B Resource...
	CALLEXEC OpenResource
	move.l	d0,_CIABase
	beq	fail				;should *never* fail!

; We attempt opening either CIA timer with the VBlankInterrupt server, because
; some int server is needed in case it gets called! VBlank server wont hurt anything.

.tryB	move.l	#VBlankIntServer,a1		;Can we allocate timer B?
	move.l	#CIAICRB_TB,d0
	move.l	_CIABase,a6
	jsr	_LVOAddICRVector(a6)
	tst.l	d0
	bne	.tryA
	
	move.l	#$bfd600,CIATimerLo		;set up all TimerA addresses
	move.l	#$bfd700,CIATimerHi
	move.l	#$bfdf00,CIAControlReg
	move.l	#CIAICRB_TB,CIAAllocBit		;we NEED this to de-alloc later
	moveq.b	#%00000010,d0			;to clear timer A intrpt
	bra	.gotCIA	

.tryA	move.l	#VBlankIntServer,a1		;Can we allocate timer A?
	move.l	#CIAICRB_TA,d0
	move.l	_CIABase,a6
	jsr	_LVOAddICRVector(a6)
	tst.l	d0
	bne	.failCIA

	move.l	#$bfd400,CIATimerLo		;set up all TimerA addresses
	move.l	#$bfd500,CIATimerHi
	move.l	#$bfde00,CIAControlReg
	move.l	#CIAICRB_TA,CIAAllocBit
	moveq.b	#%00000001,d0			;to clear timer A intrpt
	bra	.gotCIA	

.failCIA
	move.l	MyChannel,d1
	move.l	#FailCIAMsg,d2
	move.l	#EndFailCIAMsg-FailCIAMsg,d3
	CALLDOS	Write
	bra	exit

.gotCIA
	move.l	_CIABase,a6			;and disable ICR interrupts for timer
	jsr	_LVOAbleICR(a6)

	move.b	([CIAControlReg.l]),d0
	or.b	#%00001000,d0			;one-shot
	and.b	#%11011101,d0			;Cnt 02 pulses, PB6 normal (no extrnl output)
	move.b	d0,([CIAControlReg.l])

TIME	EQU	96				;microSecs (PAL & NTSC)

	move.b	#(TIME&$ff),([CIATimerLo.l])
	move.b	#(TIME>>8),([CIATimerHi.l])


*-----------------------------------------------------*

LINES	equ	200
	
	move.w	#320,d0				;Get -4- Gr1 BitPlanes (320*193)
	move.w	#LINES*4,d1
	CALLGRAF AllocRaster
	move.l	d0,Gr1_Planes
	beq	fail
	move.l	Gr1_Planes,a1			;And Clear them!
	move.l	#40*LINES*4,d0			;320/8*193*4
	clr.l	d1				;   7720 byte offset to each plane!
	CALLGRAF BltClear	

	move.w	#320,d0				;Get -4- Gr2 BitPlanes (320*193)
	move.w	#LINES*4,d1
	CALLGRAF AllocRaster
	move.l	d0,Gr2_Planes
	beq	fail
	move.l	Gr2_Planes,a1			;And Clear them!
	move.l	#40*LINES*4,d0			;320/8*193*4
	clr.l	d1
	CALLGRAF BltClear	

	move.w	#320,d0				;Get -3- Hgr1 BitPlanes (320*193)
	move.w	#LINES*3,d1
	CALLGRAF AllocRaster
	move.l	d0,Hgr1_Planes
	beq	fail
	move.l	Hgr1_Planes,a1			;And Clear them!
	move.l	#40*LINES*3,d0			;320/8*193*3
	clr.l	d1
	CALLGRAF BltClear	

	move.w	#320,d0				;Get -3- Hgr2 BitPlanes (320*193)
	move.w	#LINES*3,d1
	CALLGRAF AllocRaster
	move.l	d0,Hgr2_Planes
	beq	fail
	move.l	Hgr2_Planes,a1			;And Clear them!
	move.l	#40*LINES*3,d0			;320/8*193*3
	clr.l	d1
	CALLGRAF BltClear	

	move.w	#320,d0				;Get -1- "BackDrop" BitPlane (320*193)
	move.w	#LINES,d1
	CALLGRAF AllocRaster
	move.l	d0,BackDrop_Plane
	beq	fail
	move.l	BackDrop_Plane,a0		;and completely fill it!
	move.l	#$ffffffff,d0
	move.w	#192*40/4-1,d1
.fill	move.l	d0,(a0)+
	dbf	d1,.fill
	moveq.l	#0,d0
	move.w	#(LINES-192)*40/4-1,d1
.fill2	move.l	d0,(a0)+
	dbf	d1,.fill2
		
	lea	MyNewWindow,a0			;Open my window!
	move.l	MyScreen,nw_Screen(a0)		;(into my screen)
	CALLINT OpenWindow
	move.l	d0,MyWindow
	beq	fail

	move.l	MyWindow,a0
	move.l	wd_RPort(a0),WinRastPort

	move.l	WinRastPort,a1
	moveq.l	#1,d0				;---------------------
	CALLGRAF SetAPen

	move.l	WinRastPort,a1
	moveq.l	#1,d0
	CALLGRAF SetBPen
	
	move.l	WinRastPort,a1			;BLUE BACKGROUND TO FAKE TITLE BAR...
	move.w	#0,d0
	move.w	#0,d1
	move.w	#320,d2
	move.w	#10,d3
	CALLGRAF RectFill

	move.l	WinRastPort,a1
	moveq.l	#2,d0
	CALLGRAF SetAPen
	bra	.skip

.Msg	dc.b	"APPLE 2000  "			;extra 2 spaces to center it to "APPLE ]["
.MsgLen	dc.w	.MsgLen-.Msg		

.skip	move.l	WinRastPort,a1
	lea	.Msg,a0
	move.w	.MsgLen,d0
	CALLGRAF TextLength

	move.l	WinRastPort,a1
	lsr.w	d0				; / 2
	neg.w	d0				; -d0
	add.w	#320/2,d0			;X pos to center string
	move.w	#7,d1
	CALLGRAF Move
	
	move.l	WinRastPort,a1
	lea	.Msg,a0
	move.w	.MsgLen,d0
	CALLGRAF Text
	
	lea	StartMsg,a0
	jsr	DB_String

.Sprite	move.l	MyWindow,a0			;redefine sprite?
	lea	SpriteImage,a1
	move.l	#4,d0		;height
	move.l	#16,d1		;width
	moveq.l	#0,d2
	moveq.l	#0,d3
	CALLINT SetPointer
	bra	.skip2

	
	even	
.skip2
****************** Create HiRes Draw Lookup Table **********************
* Use a 14 bit counter to create a 2 plane response (9 bits each in
* separate words). Table will be indexed via a 14 bit index (scaled by 4),
* and return a LONG with the 1st 9 bits of plane info in the low order
* bits of the low word, and the 2nd 9 bits of plane info in the low order
* bits of the high word (accessibly via a SWAP).
*
* The 14 bits used as an index consist of 11 pixel bits, and 3 "color" bits.
* They are NOT in a very useful or logical order, this routine is made
* so these bit locations are defined.
*
* screen/pixel order: (not actual bit order)
*
* A   B        C11	- ColorSet Bit
*  12  3456789  01	- Pixel Data
*
* Rules:
* Any pixel 10 (even only) forms a green pixel (plane 1)
* Any pixel 01 (even only) forms a purple pixel (plane 2)
* Green plane saturates single 0 bits between 1 bits (ie: 10101 = 11111)
* Purple plane saturates single 0 bits between 1 bits
* Saturation does NOT apply on color bits at byte edges (pixel 2,3,9,10)
*	where A != B   or   B !=  C  (black or white okay though)
* Any 11 (anywhere) bit pairs form WHITE (both planes on)
* Extreme bits (pixel 1 in byte A & pixel 2 byte C) are discarded...


	;as reference... xxxx xxxx xxxx xxxx xxA6 5B65 4321 0C10
     	;               ^         ^         ^         ^
	;for equates, p1 -> p11 is pixel bit #  (#0 -> #31)  (left->Right as apple maps)
	;             c1, c2, c3 = Color bit for byte 1,2, &3
p1	equ	11
p2	equ	12
p3	equ	3
p4	equ	4
p5	equ	5
p6	equ	6
p7	equ	7
p8	equ	8
p9	equ	9
p10	equ	0
p11	equ     1

c1	equ	13
c2	equ	10
c3	equ	2


		;d2 = 14 bit counter, d3 = Plane1 (grn), d4= Plane2 (prp)
		;                     d5 = Pl1 Saturatd, d6= Pl2 Saturatd
		;d7 = Pixel data in visual order... (bits 10 -> 0)
                ;a2 = address to write next entry to...

	move.l	HiResDrawTbl,a2
	moveq.l	#0,d2
		
BuildHiResTbl:
	moveq.l	#0,d3
	moveq.l	#0,d4   			;clear planes... set as needed...

	   * First build normal PIXEL data in visual order... *
           * ie: d7 = %0000 0xxx  xxxx xxxx
.build	moveq.l	#0,d7
.p1	btst.l	#p1,d2
	beq     .p2
	bset	#10,d7
.p2	btst.l	#p2,d2
	beq     .p3
	bset	#9,d7
.p3	btst.l	#p3,d2
	beq	.p4
	bset	#8,d7
.p4	btst.l	#p4,d2
	beq	.p5
	bset	#7,d7
.p5	btst.l	#p5,d2
	beq	.p6
	bset	#6,d7
.p6	btst.l	#p6,d2
	beq	.p7
	bset	#5,d7
.p7	btst.l	#p7,d2
	beq	.p8
	bset	#4,d7
.p8	btst.l	#p8,d2
	beq	.p9
	bset	#3,d7
.p9	btst.l	#p9,d2
	beq	.p10
	bset	#2,d7
.p10	btst.l	#p10,d2
	beq	.p11
	bset	#1,d7
.p11	btst.l	#p11,d2
	beq     .Plane1
	bset	#0,d7

.Plane1	move.l	#%10101010101,d3
	and.l   d7,d3				;only green bits in here...

.Plane2	move.l	#%01010101010,d4
	and.l	d7,d4				;only purple bits here...


.Sat1	move.l	d3,d0				;start = 001010010100
	move.l	d3,d5
	lsl.l	#1,d0				;	 010100101000	(left shifted)
	lsr.l	#1,d5				;  AND   000101001010   (right shifted)
						;       --------------
	and.l	d0,d5				;	 000100001000  (Saturation Bits
						;			to be OR'd later)

.Sat2	move.l	d4,d0
	move.l	d4,d6
	lsl.l	#1,d0
	lsr.l	#1,d6
	and.l	d0,d6				; (Saturation bits for Pl2 to be OR'd
						;  later with Unsaturated Plane2 info)

c1_vs_c2:
	btst.l	#c1,d2				;Chk c1 HiBit...
	beq	.c1Lo
.c1Hi	btst.l	#c2,d2				;c1 Hi, chk c2 Bit...
	bne	c2_vs_c3			;c1 & c2 Hi, no worries here...
	bclr.l	#8,d5				;c1 != c2, different color bits so
	bclr.l	#9,d5				;	   don't saturate between bytes!
	bclr.l	#8,d6
	bclr.l	#9,d6
	bra	c2_vs_c3
.c1Lo	btst.l	#c2,d2				;c1 Lo, chk c2...
	beq	c2_vs_c3			;c1 & c2 Lo, no worries...
	bclr.l	#8,d5
	bclr.l	#9,d5
	bclr.l	#8,d6
	bclr.l	#9,d6

c2_vs_c3:
	btst.l	#c2,d2				;chk c2...
	beq	.c2Lo
.c2Hi	btst.l	#c3,d2				;c2 Hi, chk c3...
	bne	doneCvs				;c2 & c3 Hi, no worries...
	bclr.l	#1,d5
	bclr.l	#2,d5
	bclr.l	#1,d6
	bclr.l	#2,d6
	bra	doneCvs
.c2Lo	btst.l	#c3,d2				;c2 Lo, chk c3...
	beq	doneCvs
	bclr.l	#1,d5
	bclr.l	#2,d5
	bclr.l	#1,d6
	bclr.l	#2,d6

doneCvs	or.l	d5,d3				;take (filtered) saturation bits & insert...
	or.l	d6,d4				; ""

          * remember, d7 = %0000 0xxx  xxxx xxxx

.White	moveq.l	#0,d1				;ignore c1->c3, look for "11" bits combos
	move.l	d7,d0
	and.w	#%11000000000,d0
	cmp.w	#%11000000000,d0
	bne	.wh2
        or.w	#%11000000000,d1		;build white bits in D1...
.wh2	move.l	d7,d0
	and.w	#%01100000000,d0
	cmp.w	#%01100000000,d0
	bne	.wh3
        or.w	#%01100000000,d1
.wh3	move.l	d7,d0
	and.w	#%00110000000,d0
	cmp.w	#%00110000000,d0
	bne	.wh4
        or.w	#%00110000000,d1
.wh4	move.l	d7,d0
	and.w	#%00011000000,d0
	cmp.w	#%00011000000,d0
	bne	.wh5
        or.w	#%00011000000,d1
.wh5	move.l	d7,d0
	and.w	#%00001100000,d0
	cmp.w	#%00001100000,d0
	bne	.wh6
        or.w	#%00001100000,d1
.wh6	move.l	d7,d0
	and.w	#%00000110000,d0
	cmp.w	#%00000110000,d0
	bne	.wh7
        or.w	#%00000110000,d1
.wh7	move.l	d7,d0
	and.w	#%00000011000,d0
	cmp.w	#%00000011000,d0
	bne	.wh8
        or.w	#%00000011000,d1
.wh8	move.l	d7,d0
	and.w	#%00000001100,d0
	cmp.w	#%00000001100,d0
	bne	.wh9
        or.w	#%00000001100,d1
.wh9	move.l	d7,d0
	and.w	#%00000000110,d0
	cmp.w	#%00000000110,d0
	bne	.wh10
        or.w	#%00000000110,d1
.wh10	move.l	d7,d0
	and.w	#%00000000011,d0
	cmp.w	#%00000000011,d0
	bne	.doneWhite
        or.w	#%00000000011,d1

.doneWhite:
	or.w	d1,d3
	or.w	d1,d4				;OR white bits to both planes...

	lsr.w	#1,d3
	lsr.w	#1,d4				;eliminate last bit on each...
	and.w	#%111111111,d3
	and.w	#%111111111,d4        	  ;only want these 9 bits!

	move.w	d3,(a2)+			;1st word in table...
	move.w	d4,(a2)+			;2nd word in table... !

	addq.w	#1,d2
	cmp.w	#16384,d2			;0 -> 16383  (16384 entries for 14 bit indx)
	blo	BuildHiResTbl

.DoneHGRTable	
	move.l	MyWindow,a0
	CALLINT ActivateWindow

**************************** Init INTrpt servers **********************

	move.l	#INTB_VERTB,d0
	lea	VBlankIntServer,a1
	CALLEXEC AddIntServer
	move.w	#1,VBlankIntActive

**************************** Init & Start 6502 emulation as subtask! ***************

	move.l	#-1,d0
	CALLEXEC AllocSignal
	move.l	d0,ParentSigBit
	bmi	fail				;or for byte?should never fail anyways...
	moveq.l	#0,d1
	bset	d0,d1
	move.l	d1,ParentSigMask
	
	move.l	#0,a1				;Find Parent TaskPtr for signalling later...
	CALLEXEC FindTask
	move.l	d0,ParentTaskPtr

	move.l	#8192,d0			;alloc 8k ram for SubTask Stack...
	move.l	#MEMF_CLEAR,d1
	CALLEXEC AllocMem
	move.l	d0,MySubStack
	beq	fail

	move.l	#TC_SIZE,d0			;alloc task control structure
	move.l	#MEMF_PUBLIC!MEMF_CLEAR,d1
	CALLEXEC AllocMem
	move.l	d0,MySubTask
	beq	fail

	move.l	MySubTask,a0			;init Task Cntl Struct  (priority set -1)
	move.b	#NT_TASK,LN_TYPE(a0)
	move.l	#SubTaskName,LN_NAME(a0)
	move.b	#-1,LN_PRI(a0)			;Must be low Pri since hogs CPU...!
	move.l	MySubStack,TC_SPLOWER(a0)	;start of stack...
	move.l	MySubStack,a1
	lea	8188(a1),a1			;4 bytes less than top of stack.. ?
	move.l	a1,TC_SPUPPER(a0)
	move.l	a1,TC_SPREG(a0)
	
	move.l	a0,a1				;Task Control struct...
	move.l	#Start6502,a2			;PC beginning
	move.l	#0,a3				;Finalize code... (shouldn't quit!)
	CALLEXEC AddTask			;start the 6502 module!!!

	move.l	ParentSigMask,d0
	CALLEXEC Wait				;Wait until 6502 task is initialized...
	
	jmp	Main				;Main program!!!!!

***************** VBlank-INT server **********************
	CNOP	0,4
VBlankServerCode:	
	move.w	$dff014.l,PotPosition		;Read Joystick X/Y positions

	move.l	PotBits,d1			;mask
	btst.l	#0,d1				;is the start bit owned?
	beq.b	.done
	move.l	#$1,d0				;Yes, then restart counters...
	CALLPOTGO WritePotgo

.done	move.l	#$dff000,a0
	moveq.l	#0,d0
	rts

PotPosition	dc.w	0
VBlankIntActive	dc.w	0			;boolean- is this intrpt installed?

PotMinX		dc.b	0			;used for dynamic joystick range setting...
PotMaxX		dc.b	0
PotMinY		dc.b	0
PotMaxY		dc.b	0
PdlMode		dc.b	0			;boolean, TRUE if Paddle, FALSE if Analog Joystick
	EVEN
;---------- and Static Structure -----------

VBlankIntServer	dc.l	0,0			;STATIC Interrupt-Server Struct
		dc.b	NT_INTERRUPT
		dc.b	15
		dc.l	.name
		dc.l	0				;IS_Data
		dc.l	VBlankServerCode

.name		dc.b	"Apple2000 VBlank Interrupt Server",0
	EVEN

*************** Some Copper SetUp Subs ********************
*
* SetCop- Does a CMove & CBump, no need to re-load CopperPtr every time...
*         Enter: a3= CopperList, d3 = Register, d4 (word)= Data

SetCop	move.l	a3,a1				;cop ptr
	move.l	d3,d0				;register
	move.w	d4,d1				;data
	CALLGRAF CMove
	move.l	a3,a1				;cop ptr
	CALLGRAF CBump
	rts

	dc.b	"Apple 2000, Copyright 1994 by Kevin Kralian",0
	even


*************** Do "turn off screen" Effect! ********************
FadeEffect:
	move.l	MyScreen,a0			;*** restore orig screen ptrs ***
	lea	sc_BitMap(a0),a0		;a1 -> Screen BitMap
	move.l	OrigPlane1,bm_Planes(a0)	;plug in BitMap Plane Ptrs
	move.l	OrigPlane2,bm_Planes+4(a0)
	move.l	OrigPlane3,bm_Planes+8(a0)
	move.l	OrigPlane4,bm_Planes+12(a0)
	move.l	OrigPlane5,bm_Planes+16(a0)

	move.w	OrigRows,bm_Rows(a0)
	move.b	OrigDepth,bm_Depth(a0)

.Sprite	move.l	MyWindow,a0			;setup an invisible sprite!
	lea	SpriteImage,a1

	move.l	#4,d0				;height
	move.l	#16,d1				;width
	moveq.l	#0,d2
	moveq.l	#0,d3
	CALLINT SetPointer

	move.l	MyScreen,a0			;Rebuild local VPort copper list (but not display)
	CALLINT	MakeScreen
	CALLINT	RethinkDisplay			;and update screen!

	lea	.OffClr,a5
	move.l	a5,a1				;a1 -> ColorList
	move.l	MyScreen,a0			;Set Colors... (IF NEEDED!)
	lea	sc_ViewPort(a0),a0		;a0 -> Screen ViewPort
	move.w	#4,d0				;d0 = # of colors
	CALLGRAF LoadRGB4			;(takes effect immed, but doesn't ReThink display)

	move.l	MyWindow,a0
	move.l	wd_RPort(a0),.Rp
	
	move.l	.Rp,a1				;*** Draw 4 concentric rectangles ***
	moveq.l	#0,d0
	CALLGRAF SetAPen
	
	move.l	.Rp,a1
	move.w	#0,d0
	move.w	#0,d1
	move.w	#319,d2
	move.w	#199,d3
	CALLGRAF RectFill
	
	move.l	.Rp,a1
	moveq.l	#1,d0
	CALLGRAF SetAPen
	
	move.l	.Rp,a1
	move.w	#50,d0
	move.w	#35,d1
	move.w	#319-50,d2
	move.w	#199-35,d3
	CALLGRAF RectFill

	move.l	.Rp,a1
	moveq.l	#2,d0
	CALLGRAF SetAPen
	
	move.l	.Rp,a1
	move.w	#100,d0
	move.w	#70,d1
	move.w	#319-100,d2
	move.w	#199-70,d3
	CALLGRAF RectFill

	move.l	.Rp,a1
	moveq.l	#3,d0
	CALLGRAF SetAPen
	
	move.l	.Rp,a1
	move.w	#158,d0
	move.w	#98,d1
	move.w	#161,d2
	move.w	#101,d3
	CALLGRAF RectFill			;Done rectangles

.lp	lea	8(a5),a5
	tst.w	(a5)
	bmi.b	.done
	move.l	MyScreen,a0			;Set Colors... (IF NEEDED!)
	lea	sc_ViewPort(a0),a0		;a0 -> Screen ViewPort
	move.w	#4,d0				;d0 = # of colors
	move.l	a5,a1				;a1 -> ColorList
	CALLGRAF LoadRGB4			;(takes effect immed, but doesn't ReThink display)


	CALLGRAF WaitTOF
	CALLGRAF WaitTOF
	CALLGRAF WaitTOF
	bra.b	.lp

.done	rts


.OffClr	dc.w	$0eee,$0eee,$0eee,$0eee		;color list for "turn off monitor" effect
	dc.w	$0eee,$0eee,$0eee,$0eee
	dc.w	$0eee,$0eee,$0eee,$0eee
	dc.w	$0eee,$0eee,$0eee,$0eee
	dc.w	$0eee,$0eee,$0eee,$0eee
	dc.w	$0000,$0eee,$0eee,$0eee
	dc.w	$0000,$0000,$0eee,$0eee
	dc.w	$0000,$0000,$0000,$0eee
	dc.w	$0000,$0000,$0000,$0eee
	dc.w	$0000,$0000,$0000,$0fff
	dc.w	$0000,$0000,$0000,$0fff
	dc.w	$0000,$0000,$0000,$0ddd
	dc.w	$0000,$0000,$0000,$0eee
	dc.w	$0000,$0000,$0000,$0ede
	dc.w	$0000,$0000,$0000,$0ede
	dc.w	$0000,$0000,$0000,$0ece
	dc.w	$0000,$0000,$0000,$0ece
	dc.w	$0000,$0000,$0000,$0dce
	dc.w	$0000,$0000,$0000,$0cce
	dc.w	$0000,$0000,$0000,$0cde
	dc.w	$0000,$0000,$0000,$0dde
	dc.w	$0000,$0000,$0000,$0eee
	dc.w	$0000,$0000,$0000,$0ddd
	dc.w	$0000,$0000,$0000,$0ccc
	dc.w	$0000,$0000,$0000,$0ddd
	dc.w	$0000,$0000,$0000,$0ccc
	dc.w	$0000,$0000,$0000,$0bbb
	dc.w	$0000,$0000,$0000,$0aaa
	dc.w	$0000,$0000,$0000,$0999
	dc.w	$0000,$0000,$0000,$0888
	dc.w	$0000,$0000,$0000,$0777
	dc.w	$0000,$0000,$0000,$0666
	dc.w	$0000,$0000,$0000,$0555
	dc.w	$0000,$0000,$0000,$0444
	dc.w	$0000,$0000,$0000,$0333
	dc.w	$0000,$0000,$0000,$0222
	dc.w	$0000,$0000,$0000,$0222
	dc.w	$0000,$0000,$0000,$0111
	dc.w	$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000
	dc.w	$0000,$0000,$0000,$0000
	dc.w	-1

.Rp	dc.l	0				;windows RastPort
	even
	
***************** NOW CLOSE EVERYTHING *****************

failLoad move.l	MyChannel,d1
	move.l	#NoFileMsg,d2
	move.l	#EndNoFileMsg-NoFileMsg,d3
	CALLDOS	Write
	bra	exit

Fail020	move.l	MyChannel,d1
	move.l	#Fail020Msg,d2
	move.l	#EndFail020Msg-Fail020Msg,d3
	CALLDOS	Write
	bra	exit

failrom	move.l	MyChannel,d1
	move.l	#NoRomMsg,d2
	move.l	#EndNoRomMsg-NoRomMsg,d3
	CALLDOS	Write
;	bra	fail
		
fail	move.l	MyChannel,d1			;Error exit!
	move.l	#FailMsg,d2
	move.l	#EndFailMsg-FailMsg,d3
	CALLDOS	Write

						;Clean exit...

exit:

.VBInt	tst.w	VBlankIntActive
	beq.b	.sub
	move.l	#INTB_VERTB,d0			;remove VBlank int server...
	lea	VBlankIntServer,a1
	CALLEXEC RemIntServer
	move.w	#0,VBlankIntActive

.sub	tst.l	MySubTask			;free subtask mem...
	beq.b	.mss
	move.l	MySubTask,a1
	move.l	#TC_SIZE,d0
	CALLEXEC FreeMem

.mss	tst.l	MySubStack			;free SubTask's Stack mem...
	beq.b	.EReq
	move.l	MySubStack,a1
	move.l	#8192,d0
	CALLEXEC FreeMem

.EReq	tst.l	EasyReq
	beq.b	.FReq
	move.l	EasyReq,a1
	CALLREQ	rtFreeRequest			;okay to pass NULL...

.FReq	tst.l	FileReq
	beq.b	.Tmr
	move.l	FileReq,a1
	CALLREQ rtFreeRequest

.Tmr

.IORq	tst.l	ConReq				;close device...
	beq.b	.Amem
	move.l	ConReq,a1
	CALLEXEC CloseDevice

.IORq2	move.l	ConReq,a1
	move.l	#IOSTD_SIZE,d0			;& free mem for IOStdReq.
	CALLEXEC FreeMem

.Amem	tst.l	Mem_PtrVar			;free Apple mem image
	beq.b	.BnkMem
	move.l	Mem_PtrVar,a1
	move.l	#65545,d0
	CALLEXEC FreeMem

.BnkMem	tst.l	Bank_PtrVar			;Free Rom & Ram banks
	beq.b	.timer
	move.l	Bank_PtrVar,a1
	move.l	#$7000,d0
	CALLEXEC FreeMem

.timer	move.l	CIAAllocBit,d0			;timer 'x'
	bmi.b	.hires
	move.l	#VBlankIntServer,a1		;Deallocate CIA intrpt
	move.l	_CIABase,a6
	jsr	_LVORemICRVector(a6)
	clr.l	CIAAllocBit


.hires
;	tst.l	HiResDrawTbl			;free 64k hires draw lookup tbl...
;	beq	.disk
;	move.l	HiResDrawTbl,a1
;	move.l	#65536,d0
;	CALLEXEC FreeMem

.disk	tst.l	disk_Buffer2			;free disk buffer
	beq.b	.InsTbl
	move.l	disk_Buffer2,a1
	move.l	#disk_TrackLen*35,d0
	CALLEXEC FreeMem

.InsTbl	;its a static buffer now



.win	tst.l	MyWindow			;close window if open
	beq.b	.scrn1
	move.l	MyWindow,a0
	CALLINT	CloseWindow

.scrn1	tst.l	MyScreen			;close screen if open
	beq.b	.BkDp
	move.l	MyScreen,a0
	CALLINT	CloseScreen

.BkDp	tst.l	BackDrop_Plane			;Close Bkdrop Bitplanes if exists
	beq.b	.txt2pl
	move.l	BackDrop_Plane,a0
	move.w	#320,d0
	move.w	#LINES,d1
	CALLGRAF FreeRaster


.txt2pl

	;removed text2 planes...

.gr1pl	tst.l	Gr1_Planes			;Close Gr1 Bitplanes if exists
	beq.b	.gr2pl
	move.l	Gr1_Planes,a0
	move.w	#320,d0
	move.w	#LINES*4,d1
	CALLGRAF FreeRaster

.gr2pl	tst.l	Gr2_Planes			;Close Gr2 Bitplanes if exists
	beq.b	.hgr1pl
	move.l	Gr2_Planes,a0
	move.w	#320,d0
	move.w	#LINES*4,d1
	CALLGRAF FreeRaster

.hgr1pl	tst.l	Hgr1_Planes			;Close Hgr1 Bitplanes if exists
	beq.b	.hgr2pl
	move.l	Hgr1_Planes,a0
	move.w	#320,d0
	move.w	#LINES*3,d1
	CALLGRAF FreeRaster

.hgr2pl	tst.l	Hgr2_Planes			;Close Hgr2 Bitplanes if exists
	beq.b	.hgrDpl
	move.l	Hgr2_Planes,a0
	move.w	#320,d0
	move.w	#LINES*3,d1
	CALLGRAF FreeRaster

.hgrDpl
.temp

.FreBts	tst.l	PotBits				;if we have any still allocated...
	beq.b	.lib	
	move.l	PotBits,d0			;free potgo bits #8 & bit #9... (DATLX & OUTLX)
	CALLPOTGO FreePotBits
	clr.l	PotBits

.lib	tst.l	_REQBase			;Close ReqTools.lib if open
	beq.b	3$
	move.l	_REQBase,a1
	CALLEXEC CloseLibrary

3$	tst.l	_IntuitionBase			;Close IntuitionLib if open
	beq.b	4$
	move.l	_IntuitionBase,a1
	CALLEXEC CloseLibrary

4$	tst.l	_GfxBase			;Close GfxLib if open
	beq.b	6$
	move.l	_GfxBase,a1
	CALLEXEC CloseLibrary

6$	tst.l	_DOSBase			;Close DosLib if open
	beq.b	7$
	move.l	_DOSBase,a1
	CALLEXEC CloseLibrary

7$	rts

	CNOP	0,4

*************************** System Pointers  **********************
VarList:
_DOSBase	dc.l	0			;space for lib ptr
_GfxBase	dc.l	0
_IntuitionBase	dc.l	0
_REQBase	dc.l	0			;ptr for ReqTools lib...
_ConBase	dc.l	0			;from IOStdReq.io_Device after opening device...

_PotgoBase	dc.l	0			;resource, don't close it!
_CIABase	dc.l	0			;resource for CIA timers

PotBits		dc.l	0			;bits we allocated to read joystick btn #2

MyScreen	dc.l	0
MyWindow	dc.l	0

MyChannel	dc.l	0
Mem_PtrVar	dc.l	0			;ptr to start of Apple Memory Block
Bank_PtrVar	dc.l	0			;ptr to block of Rom & Ram Banks
WinRastPort	dc.l	0
InstTbl_Var	dc.l	0			;ptr to 256K JmpTbl of 6502 instructions
ConReq		dc.l	0			;ptr to IOStdReq for CON access
HiResDrawTbl	dc.l	0			;ptr to 64k lookup tbl for byte -> Plane data

***************************** Input stuff ***************************
		EVEN
KeyBuffer	ds.b	80
		EVEN
InputStrct	ds.b	ie_SIZEOF		;actual structure!
		CNOP	0,4

*************************** ReqTools variables *************************

EasyReq		dc.l	0			;ptr to my EasyRequester struct...
FileReq		dc.l	0			;ptr to my File Requester struct...

*************************** BitPlane Raster Pointers *************************
Gr1_Planes	dc.l	0
Gr2_Planes	dc.l	0
Hgr1_Planes	dc.l	0
Hgr2_Planes	dc.l	0
BackDrop_Plane	dc.l	0

*************************** task & signal vars ***********************
MySubTask	dc.l	0
MySubStack	dc.l	0
ParentSigBit	dc.l	0
ParentSigMask	dc.l	0
ChildSigBit	dc.l	0
ChildSigMask	dc.l	0

ParentTaskPtr	dc.l	0

********************** Disk drive interface variables ***********************
disk_Buffer	dc.l	0	;Ptr to 1st disk buffer ** 2 fields must be together **
disk_Buffer2	dc.l	0	;Ptr to 2nd disk buffer, or NULL if not present

disk_Track	dc.b	00	;Track currently resting on. 0 -> 34
disk_Track2	dc.b	00	;** These 2 byte fields MUST follow each other **

disk_Phase	dc.b	0	;a 0,1,2 or 3 based on last hi-set phase...
disk_Phase2	dc.b	0	;** These 2 byte fields MUST follow each other **

disk_Changed	dc.b	0	;boolean of whether data has been changed
disk_Changed2	dc.b	0	;(used for "Save changes to disk xxx first?")

disk_TkStrt	dc.l	0	;Ptr to memory where track info starts
disk_TkPos	dc.l	0	;Offset (0 -> 4096) into track

disk_Q6		dc.b	0	;
disk_Q7		dc.b	0	;r/w mode boolean... 0 = read, !0 = Write
disk_DriveW	dc.w	0	;drive selected (0=drive 1 or 1=drive 2)
disk_Motor	dc.b	0	;drive on? Boolean  0=off, 1=on

disk_Latch	dc.b	0	;latch where data goes to be written...

Dest_Disk_Buffer dc.l	0	;ptr to disk buffer used for disk image "loading"
Src_Disk_Buffer	dc.l	0	;ptr to disk buffer used for disk image "saving"

disk_TrackLen	EQU	$18a0	;CONST length of track...

;Filler		dcb.l	1024

EndVarList	dc.l	0	;---- end of variables to be cleared at start time! ----



StartA0		dc.l	0		;(Outside VarList on purpose)
StartD0		dc.l	0		;save CLI info when start...

	EVEN

	SECTION	SpriteData,DATA_C
SpriteImage	dc.w	$0000,$0000				;system position stuff
		dc.w	%0000000000000000,%0000000000000000	;data...
		dc.w	%0000000000000000,%0000000000000000
		dc.w	%0000000000000000,%0000000000000000
		dc.w	%0000000000000000,%0000000000000000
		dc.w	$0000,$0000				;reserved by system

	SECTION	MemoryHunk1,BSS
InstLookUpTableMem	ds.b	262148

	SECTION	MemoryHunk2,BSS
HiResDrawTableMem	ds.b	65536

	SECTION	MemoryHunk3,BSS
DiskBufferMem		ds.b	disk_TrackLen*35	;alloc 215K ram for disk buffer


***************************************************************************************
	SECTION	InitializedStructs,DATA

;------------------------ Intuition Structures --------------------------
	
MyNewScreen:
	dc.w	0,0,320,LINES,2			;x/y/width/height/depth
	dc.b	0,1				;detailpen/blockpen
	dc.w	0				;viewmodes
	dc.w	CUSTOMSCREEN!SCREENQUIET	;type
	dc.l	.TxtAtr,.Ttl			;Font,Title
	dc.l	0,0				;Gadgets,BitMap
.Ttl	dc.b	"Happy Bear?",0		
	EVEN
	
.TxtAtr	dc.l	.Font				;Text attribute for NewScreen
	dc.w	8
	dc.b	FS_NORMAL
	dc.b	FPF_ROMFONT!FPF_DESIGNED
.Font	dc.b	"topaz.font",0
	EVEN

MyNewWindow:
	dc.w	0,0,320,LINES			;x/y/w/h
	dc.b	0,1				;detail/block pens
	dc.l	RAWKEY!MOUSEBUTTONS!INTUITICKS				;IDCMP codes
	dc.l	SMART_REFRESH!NOCAREREFRESH!BACKDROP!RMBTRAP!BORDERLESS ;flags  
	dc.l	0,0				;gadget,checkmark
	dc.l	0				;WndwTtl
	dc.l	0				;Screen (FILL LATER)
	dc.l	0				;super bitmap
	dc.w	40,40,-1,-1			;min w/h, max w/h
	dc.w	CUSTOMSCREEN			;type of scrn to open on
.Ttl	dc.b	0,"APPLE 2000 Window",0		
	EVEN


*********************** Speed Regulation/Timer stuff ***************************

CIATimerLo	dc.l	0			;ptrs for alloc'd CIA timer regs
CIATimerHi	dc.l	0
CIAControlReg	dc.l	0
CIAAllocBit	dc.l	-1			;Allocated Int bit (used for de-alloc)


LastStopCycle	dc.l	0			;Cycle # we started regulation period on

CPF		equ	150			;assuming 1.024 Mhz clock???
RealCycPerFrame	dc.l	CPF
WaitCycPerFrame	dc.l	CPF



;--------------------- Names & Messages --------------------------------

SubTaskName	dc.b	"Apple 6502 CPU Emulation",0

StartMsg	dc.b	"Libraries, AppleRom, Screen, Window Opened Ok...",10,13,0

FailMsg		dc.b	"Whoops, failed somewhere!",10,13
EndFailMsg

NoRomMsg	dc.b	"Startup Error: _APPLE.ROM file not found! ",10,13
EndNoRomMsg

FailCIAMsg	dc.b	"Startup Error: Couldn't obtain either CIAB timer!",10,13
EndFailCIAMsg

Fail020Msg	dc.b	"Sorry, Apple 2000 requires at least a 68020 CPU.",10,13
EndFail020Msg

RomName		dc.b	"_Apple.Rom",0
DiskRomName	dc.b	"_Disk.Rom",0

MMUMsg		dc.b	"Sorry, this version requires an MMU.",10,13

NoFileMsg	dc.b	"File specified not found! ",10,13
EndNoFileMsg

BSMsg1		dc.b	"Experimental Code Caching ",0
BSMsg2		dc.b	"On",10,13,0
BSMsg3		dc.b	"Off",10,13,0
BSMsg4		dc.b	"-Shadowed- video refreshing",0
BSMsg5		dc.b	"-Periodic Refresh- video refreshing",0

dosname		DOSNAME		;from the dos_lib.i include... dcb ...
execname	EXECNAME
grafname	GRAFNAME
intname		INTNAME
reqname		REQTOOLSNAME
ciaBname	CIABNAME
potgoname	dc.b	"potgo.resource",0
conname		dc.b	"console.device",0

BSMsg6		dc.b	"Sorry, AAA video drivers not complete.",10,13

	* ReqTools Messages/TagLists *

LoadMsg		dc.b	"Select File to Load:",0
SaveMsg		dc.b	"Select File to Save:",0

;QuitMsg		dc.b	"Really Quit?"
;QuitAnsMsg	dc.b	"Yes|No"

	CNOP	0,4
;------------------------ MyColor Tables ---------------------------
; Each color Table is the # of colors followed by the xRGB definition for each (WORDS)

MainColorTable	dc.w	$0004
		dc.w	$0000,$089a,$0fff,$0888
		dc.w	$0000,$0eee,$000a,$0888		;old colors

TxtColorTable	dc.w	$0004
		dc.w	$0000,$0eee,$0000,$0eee
HiResColorTable	dc.w	$0010
		dc.w	$0000,$0eee,$0000,$0eee,$0666,$0888,$0aaa,$0ccc
		dc.w	$0000,$00e0,$0a0e,$0eee,$0000,$0e60,$000e,$0eee
GrColorTable	dc.w	$0020
		dc.w	$0000,$0eee,$0000,$0eee,$0666,$0888,$0aaa,$0ccc
		dc.w	$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
		dc.w	$0000,$0e00,$0008,$0a0e,$00a0,$0a9a,$000a,$000e
		dc.w	$0830,$0e60,$0ba9,$0f3d,$00e0,$0ed0,$00ae,$0eee
Flash1ColorTbl	dc.w	$0004
		dc.w	$0000,$0eee,$0000,$0eee
Flash2ColorTbl	dc.w	$0004
		dc.w	$0000,$0eee,$0eee,$0000

	EVEN
;----------------------- My DiskRom image -----------------------------
MyDiskRom:
	dc.b	$A2,$20,$A0,$00,$A2,$03,$86,$3C,$A2,$09,$BD,$40,$C6,$9D,$0E,$04
	dc.b	$CA,$10,$F7,$A9,$60,$85,$2B,$AD,$EE,$C0,$AD,$EC,$C0,$AD,$EA,$C0
	dc.b	$AD,$E9,$C0,$A9,$00,$8D,$28,$C0,$A9,$90,$8D,$28,$C0,$D0,$21,$60
	dc.b	$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA
	dc.b	$C1,$D0,$D0,$CC,$C5,$A0,$B2,$B0,$B0,$B0,$EA,$EA,$EA,$EA,$EA,$EA
	dc.b	$A9,$00,$85,$26,$85,$3D,$85,$41,$A9,$08,$85,$27,$48,$EA,$AD,$EC
	dc.b	$C0,$EA,$EA,$C9,$D5,$D0,$F7,$AD,$EC,$C0,$C9,$AA,$D0,$F5,$AD,$EC
	dc.b	$C0,$C9,$96,$D0,$E9,$AD,$29,$C0,$D0,$09,$F0,$07,$60,$EA,$EA,$EA
	dc.b	$EA,$EA,$EA,$AD,$29,$C0,$85,$40,$EA,$C5,$41,$D0,$D1,$AD,$29,$C0
	dc.b	$C5,$3D,$D0,$CA,$AD,$EC,$C0,$C9,$D5,$D0,$F9,$F0,$09,$EA,$EA,$EA
	dc.b	$EA,$EA,$EA,$EA,$EA,$EA,$AD,$EC,$C0,$C9,$AA,$D0,$EA,$AD,$EC,$C0
	dc.b	$C9,$AD,$D0,$AA,$A9,$00,$A0,$56,$4D,$28,$C0,$88,$99,$00,$03,$D0
	dc.b	$F7,$4D,$28,$C0,$91,$26,$C8,$D0,$F8,$4D,$28,$C0,$D0,$90,$F0,$06
	dc.b	$EA,$EA,$EA,$90,$87,$EA,$68,$A2,$56,$CA,$30,$FB,$B1,$26,$5E,$00
	dc.b	$03,$2A,$5E,$00,$03,$2A,$91,$26,$C8,$D0,$EE,$E6,$27,$E6,$3D,$A5
	dc.b	$3D,$CD,$00,$08,$A6,$2B,$90,$DB,$4C,$01,$08,$00,$00,$00,$00,$00

;--------------------------- BackUp Bitmap data ---------------------
OrigScrnRasInfo	dc.l	0		;backup stuff to restore to close screen...
OrigPlane1	dc.l	0
OrigPlane2	dc.l	0
OrigPlane3	dc.l	0
OrigPlane4	dc.l	0
OrigPlane5	dc.l	0

OrigRows	dc.w	0
OrigDepth	dc.b	0
		EVEN
;
;	SECTION	MemoryHunk4,BSS
;		ds.b	1
***************************************************************************************

