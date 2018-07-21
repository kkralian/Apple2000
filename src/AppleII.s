	SECTION	APPLEII,CODE

* Changes: PCount is a full 32 bit pointer, so memory model is now flat. ABSOLUTE!!!
*	   Full "GetByte" IS USED in insts that will ultimately PutByte! (due to hardware rds)
*	   Fixed a couple instructions to work with more flexible hardware emulation subs
*	   fixed intertask communication...

* new changes:
*	Optimized Video-mode rtns for consecutive mode-changing insts (do nothing until
*								       last instruction)
*	Broke program into different SECTIONS (APPLEII,OPENCLOSE, and TABLES)
*	A couple HW locations have data ($c020, $c060) to make a couple progs work
*	Fixed Drive emulation to disable physical drive activity unless drive #1 is enabled
*		(but ignoring motor status for now due to speed)
*	Added 16K ram card emulation! (Currently ignores 2-access rule for write enabling)
*	RESET will set system to ROM read and Write enable bank 2 (like //e, not //+)
* 	NumPad keys used to trim joystick center... (#2,4,5,6, & 8)
*	Changing PCount during "Stop Inst" (eg: LOADING an executable) sets Video to Text1
*	Loading an executable file also sets ram card to "Rom Read"
*	Added PCount History diagnostic for debugging...
*	Added "FlushMsgs" so all inputs immediately after a requester are ignored
*	Removed 1/2 second delay after requesters (due to fixed intertask comm & flush msgs)
*	Added automatic DISK IMAGE COMPRESSION during save, with pseudo-prodos header!
*	Expanded Joystick cycle counts to work with LodeRunner...
*	Added toggleable MOUSE/JOYSTICK control of pdls... (f9 key) w/ word in title bar
*	Reworked Title Bar/Window display so can read mouse buttons ANYWHERE in screen...
*	Reworked Diagnostic message in title bar (each has a FORBID due to shared lib)
*	Added a custom (blank) pointer so not to confuse it with apple progs...
*	I think the DECOMPRESS function automatically  handles both ProDos & Dos headers...
*	Clear Slot Rom memory ($c100 - $c7ff) to FF's so ProDos thinks they're empty!
*	Minor opt in PUSHWORD to write to memory once...
*	Reworked all STAT macros!
*	--Did I fix the Copperlists by adding a CBUMP at the end of each one???
*	DOCYCLE reads word with PostInc'ing of PCount (so PCount needs less adding)
*	** Reg d0 is kept with top word clean at ALL TIMES in 6502 emulation. **
*	   Lots of MEM_addressing & cpu optimizations because of it...
*	Opt'ed BIT__ functions for more straightforward Status update...
*	Hi-Res draw routines first check if memory has been changed! (big opt!)
*	Text/Lo-Res draw routines first check if memory has been changed!
*	Sound is sent to both channel 0 & 1 (stereo'ish)
*	Fixed mischevious bug (undocumented) in PUSHSTAT subroutine... (see it)
*	Optimized: JSR, RTS, ADC, SBC, ROL, ROR...
*	Upon loading executable Apple prog, low memory is reset to fresh "power-up" state


* Major opts to:Vector-Jump table now 256K (16 bit word referenced * 4)
*		DOCYCLE doesn't have to clear hi 8 bits of word anymore
*		All MEM_IMMED routines opt'd to not reference memory, but use d1 from DOCYCLE
*		All Bcc's dont reference mem, but use d1 from DOCYCLE (& other opts) 

* PUBLIC RELEASE v1.0 around in April, 94

* 04/21/94 Fixed PBHardware so "unhandled" writes DO NOT change memory 
*          (fixed probs w/ button read on "Red Alert", which tried to ROL $c061)
* 04/26/94 Included Custom Disk Rom that is used if _DISK.ROM file not present
* 04/27/94 Included ability to load 143,360 byte generic "disk images"
* 04/29/94 Upon any ResumeCPU call, re-activate our window...
* 04/29/94 Check for 68020+ CPU (instead of crashing!)
* 04/29/94 Reading from $c050 - $c057 returns progressive data (Tetris II now boots)
* 05/01/94 Now saves disk images in 143,360 byte "generic disk image" instead of DDD format
* 05/01/94 Opt'd STAT routines a little (ie: use BCHG instead of EOR for inverse carry)
* 05/01/94 Opt'd TX_ Insts to use STAT_SZfast instead of STAT_SZ
* 05/01/94 Opt'd all CLC+ADC and SEC+SBC combinations...
* 05/07/94 Proper Dos/ProDos executable/disk image/Rom file recognition
* 05/08/94 Opt'd all DEX/INX/DEY/INY + CPX/CPY Insts...
* 05/11/94 Put STACK in HIGH WORD of register to make lower word available for use!!!!!!
* 05/13/94 Opt'd STAT_ stuff...
* 05/15/94 Opt'd all DEX/INX/DEY/INY/PHA/TAX/TAY + LDA instruction pairs
* 05/18/94 Opt'd bunch more xxx + STA instruction pairs...
* PUBLIC RELEASE v1.1 on 05/18/94

* 05/28/94 Added support for Analog Joystick w/ 2 buttons!
* 05/29/94 Fixed Stack (now 100% accurate) so words wrap on same page
*          (Sargon, MoonPatrol, Lode Runner, Pinball Co Set now work)
* 06/01/94 Added "Page-flipping loop" recognition & skipping... 
*          (ShortCircuit,BileStoad,OutPost)
* 06/04/94 Overhaul entire video subsystem! No more copper lists, hardware hitting, etc...
* 06/10/94 Continuing to optimize video switches... Useless changes ignored
* 06/17/24 Added StatusBar messaging system
* 06/20/94 Added Lower Case characters from //e charset
* 06/20/94 Flashing/Color-changing only occurs when flashing is visible.
* 06/24/94 Added Dynamic Analog Joystick ranging (for different joysticks)
* 06/24/94 Added Atari Paddle support
* 06/24/94 "Requestor Screen" now uses Workbench colors
* 06/30/94 Fixed AGA/Mode promo bug... Can't set more colors than screen allows!
* 07/01/94 Added 2 drive support (via -2 command) to hardware ($C0xx) handlers...
* 07/02/94 Added 2 drive req support to load/save commands
* 07/03/94 All video-HW reads return values from list (Canyon Climber, Tetris II ttl, Drol)
* 07/03/94 Made keyboard read vals for $c000 - $c007 (Frogger into screen works)
* 07/10/94 Optional DDD compression by ending filename in '>' during save
* 07/12/94 16K card banking routines optimized to prevent needless memcpy...
* 07/15/94 Working Speed Regulation & interface (60 hz screen only)
* 07/17/94 Added 6502 bug- Jump ($xxff) wrap-around getting indirection... (Randamn works)
* 07/18/94 Optimized DDD decompress & denibbleize functions...
* 07/19/94 FileReqs retain independant filenames for each drive for loading/saving
* 07/22/94 Fixed "Rewrite file xxx" requester...
* 07/22/94 Opt'd DDD compress & some saving routines...
* 07/23/94 Screen now 200 lines (standard size)
* 07/24/94 Re-did Joystick & Mouse pdl emu routines (simple, full-range of motion)
* 07/28/94 Added "Warning- Disk image Changed" warnings during disk loads & quit
* 07/28/94 Opt'd Memory Address modes even more...
* 07/28/94 Opt'd BIT, ROL, ROR, ASL insts
* 07/29/94 Free'd up a register ("Temp") by using stack instead (seldom)
* 07/29/94 Opt'd CLC/SEC + ROL pairs...
* 08/02/94 Added cheesy "Monitor off" effect
* 08/18/94 Used old default "requester" colors again... (not WB)
* 08/20/94 Default speed regulation to 100%

* To Do:   
* check ADC/SBC bcd status settings, optimize ROL,ROR,ASL, etc...
* Handle lores/hires page swapping (gorgon, zenith, space voyage, etc...)
* Add Icon...
* Check keymap thing (# on swedish keyboard)
* check if potbits & intrpts are turned off at exit properly...

* Comments:   Map is clean and flat from $0000->$FFFF
* IMPORTANT!  Reg d0 MUST be kept with top word clear at ALL TIMES in 6502 context.

AReg	EQUR	d7	;AReg,XReg,&YReg are clean 8 bit nums. KEEP CLEAN!!!
XReg	EQUR	d6
YReg	EQUR	d5
StatusSZ EQUR	d4
Cycles	EQUR	d3	;Progressive counter of effective apple cycles...
Stack	EQUR	d2	;(Stack) -> Current byte of stack ($100 - $1ff)(HIGH WORD ONLY!!!!!)
StatusCV EQUR	d2	;bottom word only (shared w/ Stack)

;Temp	EQUR	a5	;for routines (CPU inst's) to backup data...
Mem_Ptr	EQUR	a4	;reference start of 64k apple memory block...
InstTbl	EQUR	a3
PCount	EQUR	a2	;ABSOLUTE 32 bit ptr, not index!
	rts


****************************************
* General Note: All "F" routines are FastGetByte or FastPutByte functions,
* which read/write from apple memory in a QUICK fashion, with no checks
* or support for i/o accesses. 6502 Instructions, Operands, and addressing
* from memory use these routines, and therefore will execute differently
* from an actual apple in these conditions. However, executing from i/o
* would be a rare circumstance (NO reason for it), and in all probability
* would only happen during an apple-crash. 
*
* Reading from ROM or RAM Banks (for speed) is not checked for but
* is accounted for. When apple i/o to change "bank/Rom to read from" is
* set, memory is actually copied to the top ($d000-$ffff, whatever) area.
****************************************
CLEAR	MACRO.w
	moveq.l	#0,\1
	ENDM

*******************************
* FPUTBYTE does a quick PutByte with no i/o,hardware,graphic checks.
* Enter:  a0 = LONG 16 bit address (clean in top 16 bits!!!) to write
*         d0 = Byte to write
*******************************
FPUTBYTE MACRO.w
	move.b	d0,(Mem_Ptr,a0.l)
	ENDM

*****************************************
* FGETBYTE does a quick GetByte with no i/o checks. Preserves high bits.
* Enter:  a0 = LONG 16 bit address (clean in top 16 bits) to read
* Return: d0 = Byte read, clean!
*****************************************

FGETBYTE MACRO.w
	move.b  (Mem_Ptr,a0.l),d0					[14]= 18 total!
	ENDM


*********************************
* FGETWORD does a fast read of a 'normalized' word from apple memory. No i/o.
* Enter:  A0 = Long 16 bit addr to read.
* Return: D0 = Word in form 'xxxxHILO', preserving top 16 bits.
********************************** (used in Brk, ResetCPU, and MEM_routines)
FGETWORD MACRO.w
	move.w	(Mem_Ptr,a0.l),d0
	ror.w	#8,d0
	ENDM

***** FGETWORDd0 is same as FGETWORD, but refs "D0" instead of "A0" *****
FGETWORDd0 MACRO.w
	move.w	(Mem_Ptr,d0.l),d0
	ror.w	#8,d0
	ENDM

**************************************************
* GETBYTE - General Routine to Read a Byte of Memory from apple memory.
* Checks for hardware/io access, and handle if rqrd.
* Enter:  D0 = LONG 16 bit addr to read (CLEAN!)    <------ d0!!!!
* Return: D0 = Byte of Data in D0:0-7. (FGetByte is fast-no check)
******************************************	
GETBYTE	MACRO.w
	cmp.w	#$c000,d0	[6]	;in $c001 - $c0ff range???
	bls.b	.InBnd		[9]
	cmp.w	#$c0ff,d0
	bhi.b	.InBnd

	bsr	GBHardWare		;HardWare! Go Handle!
	and.l	#$ff,d0		;remove once all GBHardware routines cleaned...
	bra.b	.EndGB
	CNOP	0,4
.InBnd	move.b	(Mem_Ptr,d0.l),d0	;	[14]
.EndGB	
	ENDM

*************************************************
* PUTBYTE - General Routine to Write a Byte of Memory to apple memory.
* Checks for ALL EXCEPTIONS (hardware/io/Video pages), and handle if rqrd.
* ENTER:  D0 = LONG 16 bit addr to write,
* 	  D1 = Byte to write.
*************************************************

PUTBYTE MACRO.w		
	move.l	d0,a0						[03]
	lsr.w	#8,d0		;/64  Memory page # * 4...	[04]

	move.l	#PBPageList,a1					[5]
	move.l	(a1,d0.w*4),d0					[11] = [29] !
	
	beq.b	.safe\@
	move.l	d0,a1
	jsr	(a1)			;A0 = address, d1 = Data !!!!
	CLEAR	d0		;remove when all routines clean...
	bra.b	.cont\@
.safe\@	move.b	d1,(Mem_Ptr,a0.l)	;modified FPUTBYTE
.cont\@	
	ENDM

PUTBYTE_DOCYCLE MACRO.w		
	move.l	d0,a0						[03]
	lsr.w	#8,d0		;/64  Memory page # * 4...	[04]

	move.l	#PBPageList,a1					[5]
	move.l	(a1,d0.w*4),d0					[11] = [29] !
	
	beq.b	.safe\@
	move.l	d0,a1
	jsr	(a1)			;A0 = address, d1 = Data !!!!
	CLEAR	d0		;remove when all routines clean...
	bra.b	.cont\@
;	DOCYCLE
.safe\@	move.b	d1,(Mem_Ptr,a0.l)	;modified FPUTBYTE
.cont\@	DOCYCLE
	ENDM

********************************
* PUSH/PULL will push or pull selected byte to/from Apple Stack ($100-$1ff).
* eg... "PUSH d0", "PULL AReg" etc.... DO NOT push/pull Status this way!!!!
*
* NOTE: Due to Opts, Pre/Post +- opposite of 6502,
* and "Stack" is +1 actual location. TSX & TXS fixed.  (INVALID- Works identically)
********************************
PUSH	MACRO.w				;eg   "PUSH d0"
	swap	Stack
	move.b	\1,(Mem_Ptr,Stack.w)		; [08]
	subq.b	#1,Stack
	swap	Stack
	ENDM

PULL	MACRO.w				;eg   "PULL AReg"
	swap	Stack
	addq.b	#1,Stack
	move.b	(Mem_Ptr,Stack.w),\1		; [08]
	swap	Stack
	ENDM

*************************************
* PUSH16 / PULL16 will push or pull a word to/from apple stack & update.
*		  Push - Hi 1st, LO 2nd.  Pull - LO first, HI 2nd.
* Enter: (PUSH16) = D0 = Word to push
* Return (PULL16) = D0 = Word from stack
**************************************
PUSH16	MACRO.w
	ror.w	#8,d0
	PUSH	d0
	lsr.w	#8,d0
	PUSH	d0
	ENDM

		;Pull 16 bit number from Apple Stack... LO byte, Then Hi
	        ;Update Stack ptr and return # in D0
PULL16	MACRO.w
	PULL	d1
	PULL	d0
	lsl.w	#8,d0
	move.b	d1,d0
	ENDM


***** All the grind work due to using different bits in the emulated StatusReg
***** are taken care of when pushing/pulling status here...
* Strangely, it appears that the Break flag is always set upon pushing the Status.
* Perhaps its only clear during an interrupt? Saving it as clear allowed a nasty
* bug in Dos 3.3 filtered text, accidentally making a match to the "INIT" command.
* (See dos 3.3 $9fc0 - $9a10). Experiments on the //gs show even if B is forced clear,
* any command that changes the Status reg will automatically set B as well...

*	Apple Status=	;	Bit   7   6   5   4   3   2   1   0
			;           _________________________________
			;           | S | V |   | B | D | I | Z | C |
			;	    +---+---+---+---+---+---+---+---+
			;             |   |       |   |   |   |   |__Carry
			; 	      |   |_Over- |   |   |   |_Zero
			;             |     Flow  |   |   |_Intrpt Disable
			;	      |_Sign      |   |_Decimal
			;                         |_Break


PUSHSTAT MACRO.w				;pushes status in proper apple way
						;build d0 in proper 6502 Status form (above)
.s7	btst.l	#S_BIT,StatusSZ		[05]
	sne.b	d0			[04]
	lsl.w	d0			[04]

.v6	btst.l	#V_BIT,StatusCV
	sne.b	d0
	lsl.w	d0

.b54	move.b	#$ff,d0				;B Always set (& unused bit 5 set too!)
	lsl.w	#2,d0

.d3	btst.l	#D_BIT,StatusSZ
	sne.b	d0
	lsl.w	d0

.i2	btst.l	#I_BIT,StatusSZ
	sne.b	d0
	lsl.w	d0

.z1	btst.l	#Z_BIT,StatusSZ
	sne.b	d0
	lsl.w	d0

.c0	btst.l	#C_BIT,StatusCV
	sne.b	d0
	lsl.w	d0

	lsr.w	#8,d0	
	PUSH	d0
	ENDM	

*	Apple Status=	;	Bit   7   6   5   4   3   2   1   0
			;           _________________________________
			;           | S | V |   | B | D | I | Z | C |
			;	    +---+---+---+---+---+---+---+---+
			;             |   |       |   |   |   |   |__Carry
			; 	      |   |_Over- |   |   |   |_Zero
			;             |     Flow  |   |   |_Intrpt Disable
			;	      |_Sign      |   |_Decimal
			;                         |_Break

PULLSTAT MACRO.w
	PULL	d0
;	moveq.l	#0,StatusSZ	;build internal Status from 6502 form (above)
;	move.w	#0,StatusCV	;low word only...

.bdi	move.b	d0,StatusSZ		[03]	;Got B, D, and I
	swap	StatusSZ		[04]

.sBit	lsl.b	d0			[04]	;get S (temporarily in bit 8)
	scs.b	StatusSZ		[04]
	lsl.w	StatusSZ		[04]

.vBit	lsl.b	d0				;get V
	scs.b	StatusCV
	lsl.w	StatusCV

.cBit	lsr.b	#3,d0				;get C
	scs.b	StatusCV

.zBit	lsr.b	d0				;Get Z
	scs.b	StatusSZ
	lsr.w	#5,StatusSZ			;and slide S & Z to proper bits...

	ENDM

*************************************
* All MEM_ routines are to handle different 6502 address modes.
* Note: Due to 16 bit read of PCount mem, optimizations are made by
* expecting low byte in D0 to have next byte of data!
*
* Enter:  "PCount" = pointing at opcode
*	  D0 = word of data from "PCount"
* Return: D0 = operand's 16 bit address in A0 (CLEAN!)   -----D0-----
*	  PCount = PCount + Full Inst Size
*
*************************************
MEM_Abs	MACRO.w 				;***  LDA $xxxx  ***
	move.b	(PCount)+,d0	[07]
	lsl.w	#8,d0		[04]
	move.b	d1,d0		[03] = 14
	ENDM

MEM_ZP	MACRO.w					;***  LDA $xx  ***
	moveq.l	#0,d0			[03]
	move.b	d1,d0			[03]
	ENDM

MEM_PreIndx	MACRO.w				;***  LDA ($xx,X)  ***
	move.w	XReg,d0				;(clear high byte)
	add.b	d1,d0				;FGETBYTE1PC
			;FGETWORDd0	;get 16 bit # being pointed at
	move.w	(Mem_Ptr,d0.l),d0
	ror.w	#8,d0
	ENDM


MEM_PostIndx	MACRO.w				;***  LDA ($xx),Y  ***
	moveq.l	#0,d0
	move.b	d1,d0
	FGETWORDd0
	add.w	YReg,d0				;16 bit add...(YReg & d0 must be clean!)
	ENDM

MEM_ZPIndxX	MACRO.w				;***  LDA $xx,X  ***
	move.w	XReg,d0
	add.b	d1,d0				;8 bit add
	ENDM

MEM_ZPIndxY	MACRO.w				;***  LDA $xx,Y  ***
	move.w	YReg,d0
	add.b	d1,d0				;8 bit add
	ENDM

MEM_AbsIndxX	MACRO.w				;***  LDA $xxxx,X  ***
	move.b	(PCount)+,d0	[07]
	lsl.w	#8,d0		[04]
	move.b	d1,d0		[03] = 14
	add.w	XReg,d0				;16 bit add
	ENDM

MEM_AbsIndxY	MACRO.w				;***  LDA $xxxx,Y  ***
	move.b	(PCount)+,d0	[07]
	lsl.w	#8,d0		[04]
	move.b	d1,d0		[03] = 14
	add.w	YReg,d0				;16 bit add
	ENDM


*** OLDMEM routines and are for 2'nd INST of CPU inst pairs that don't have operands in D1.
** They read from and Inc PCOUNT to get operands.

OLDMEM_Abs MACRO.w 				;***  LDA $xxxx  ***
	move.w	(PCount)+,d0	[07]
	ror.w	#8,d0		[08] = [15]
	ENDM

OLDMEM_ZP MACRO.w				;***  LDA $xx  ***
	moveq.l	#0,d0
	move.b	(PCount)+,d0	[07]
	ENDM

OLDMEM_PreIndx MACRO.w				;***  LDA ($xx,X)  *** (optd)
	move.w	XReg,d0
	add.b	(PCount)+,d0
	FGETWORDd0				;get 16 bit # being pointed at
	ENDM

OLDMEM_PostIndx	MACRO.w				;***  LDA ($xx),Y  ***
	moveq.l	#0,d0
	move.b	(PCount)+,d0
	FGETWORDd0
	add.w	YReg,d0				;16 bit add...(YReg & d0 must be clean!)
	ENDM

OLDMEM_ZPIndxX MACRO.w				;***  LDA $xx,X  *** (optd)
	move.w	XReg,d0
	add.b	(PCount)+,d0			;8 bit add
	ENDM
	
OLDMEM_ZPIndxY MACRO.w				;***  LDA $xx,Y  *** (optd)
	move.w	YReg,d0
	add.b	(PCount)+,d0			;8 bit add
	ENDM

OLDMEM_AbsIndxX	MACRO.w				;***  LDA $xxxx,X  ***
	move.w	(PCount)+,d0
	ror.w	#8,d0
	add.w	XReg,d0				;16 bit add
	ENDM

OLDMEM_AbsIndxY	MACRO.w				;***  LDA $xxxx,Y  ***
	move.w	(PCount)+,d0
	ror.w	#8,d0
	add.w	YReg,d0				;16 bit add
	ENDM
	
OLDMEM_ImmedD0 MACRO.w				;***  LDA #$08  *** 
	move.b	(PCount)+,d0			;(returns immediate val in d0)
	ENDM


*************************************************8
* STAT_ routines check actual CCR reg after an instruction, and will
* copy pertinent bits to Status. 
*
* Enter:  amiga CCR = conditions set (via operations)
* Return: Status = new status bits properly set
*
* NOTE: due to opts, the BIT / HEX settings are DIFFERENT than the 6502 cpu
* status register. PUSH/PULL STATUS routines accomidate this difference,
* so the apple will never know.

B_BIT equ 20	;(7,6,5 not in Amiga ccr, but we'll let 'em be in Status high word anyways)
D_BIT equ 19
I_BIT equ 18

S_BIT equ 3	;within StatusSZ word (just like amiga CCR)
Z_BIT equ 2

V_BIT equ 8	;within StatusCV word
C_BIT equ 7

S_HEX	equ	$1<<S_BIT	;x_HEX used to build masks in STAT rtns
Z_HEX	equ	$1<<Z_BIT
V_HEX	equ	$1<<V_BIT
C_HEX	equ	$1<<C_BIT

;Scc SpecAddr			[11]
;AND.w	#Imm, Dn		[06]
;AND.l	#Imm, Dn		[08]
;Bcc.b				[05/09]
;BTST/BSET/BCLR #imm,Dx 	[05]
;eor.l	#Imm, Dn		[08]
;BCHG.l	#Imm, Dn		[05]

* Status bits emulated in two separate registers (low word of each).
* StatusSZ has S & Z bits, and StatusCV holds C & V bits. Note that
* the other bits in these two words can't be used for anything!

STAT_SZ	MACRO
	move.w	ccr,StatusSZ				[06]
	ENDM

STAT_C MACRO								(ASL,ROL,ROR,ADCbcd)
	scs.b	StatusCV	;set bit 7 (& more)	[04]   = 10
	ENDM

STAT_SZC MACRO								(ASL,ROL,ROR,ADCbcd)
	move.w	ccr,StatusSZ				[06]
	scs.b	StatusCV	;set bit 7 (& more)	[04]   = 10
	ENDM

STAT_SZiC MACRO					;do C bit reversed  	(CMP, CPX, CPY, SBCbcd)
	move.w	ccr,StatusSZ				[06]
	scc.b	StatusCV				[04]   = 10
	ENDM

STAT_SVZC MACRO						  		(ADC)
	move.w	ccr,StatusSZ				[06]
	move.b	StatusSZ,StatusCV			[03]
	lsl.w	#7,StatusCV	;shift C ->bit 7, V ->8	[04]   = 13
	ENDM


STAT_SVZiC MACRO		;for SBC & CMP due to inverse C...	(SBC)
	move.w	ccr,StatusSZ				[06]
	move.b	StatusSZ,StatusCV			[03]
	lsl.w	#7,StatusCV				[04]
	bchg.l	#C_BIT,StatusCV				[05] = 18
	ENDM

**************************************
* "Regulate" is a macro that checks if speed regulation is necessary, and if so,
* it properly waits the rqrd amount of time.
**************************************
REGULATE MACRO.w

	move.l	WaitCycPerFrame,d0		;d0 = WaitCyc, if -1 then no regulation!
	bmi.b	.done

	move.l	Cycles,d1			;CurrentCycle - LastStopCycle = ElapsedCycles
	sub.l	LastStopCycle,d1		;   (okay if Cycles has wrapped around)
	sub.l	d0,d1
	bmi.b	.done

	move.l	CIAControlReg,a0
.wait	btst.b	#0,(a0)				;wait for timer to finish
	bne.b	.wait	
	bset.b	#0,(a0)				;and restart it

	add.l	d0,LastStopCycle
	sub.l	d0,d1				;Reset LastCycleCount
	bpl.b	.wait

.done
	moveq.l	#0,d0
	ENDM

RegulateSize	dc.l	.end-.strt
.strt	REGULATE
.end


**************************************
* "DoCycle" is the main routine that reads instruction & jumps via
* table to code to process it. Call with all CPU vars set!
**************************************
DOCYCLE MACRO.w
;	jsr	KeepHistory		;FOR DEBUGGING!!!! ----------------
	move.w	(PCount)+,d1		;read byte, inc PCount by 2...	[07]
	move.l	(InstTbl,d1.w*4),a0
	jmp	(a0)
	CNOP	0,8
	ENDM


		CNOP	0,4
PCountIndex	dc.l	0

PCHistory:
	ds.l	256		;for PC history for debugging!!!

	EVEN
KeepHistory:
	move.l	PCountIndex,d0
	move.l	PCount,(PCHistory.l,d0.l*4)
	addq.b	#1,d0
	move.l	d0,PCountIndex
	rts

ReportHistory:
	move.l	d2,-(sp)

	lea	.msg,a0
	jsr	DB_String		;"PC History:"
	move.l	PCountIndex,d1
	sub.b	#40,d1			;d1 = memIndx
	move.l	#39,d2			;d2 = count
.lp	move.l	(PCHistory.l,d1.l*4),d0
	sub.l	Mem_Ptr,d0
	sub.l	#2,d0		;adjust for PostInc of PCount
	jsr	DB_HexW			;Print HexAddr...
	addq.b	#1,d1
	dbf	d2,.lp	
	move.l	(sp)+,d2
	rts


.msg	dc.b	"6502 execution history:",10,13,0

	CNOP	0,4
Start6502:					;*** ONLY CALL to BEGIN 6502 emu task!!!! ***
	move.l	#-1,d0
	CALLEXEC AllocSignal			;Get signal for Stop/Resume control.
	move.l	d0,ChildSigBit			;(fail now and we're screwed!!!)
	moveq.l	#0,d1
	bset	d0,d1
	move.l	d1,ChildSigMask

	jsr	RestoreTable

	move.l	ParentTaskPtr,a1
	move.l	ParentSigMask,d0
	CALLEXEC Signal					;critical init'ing done... Tell parent..
	
	move.w	#60,Hardware+aud0+ac_vol.l		;Set Channel 0 Volume to Max
	move.w	#1,Hardware+aud0+ac_per.l		;Channel 0 Period
	move.w	#60,Hardware+aud1+ac_vol.l		;Set Channel 1 Volume to Max
	move.w	#1,Hardware+aud1+ac_per.l		;Channel 1 Period

	move.l	Mem_PtrVar,Mem_Ptr
	move.l	InstTbl_Var,InstTbl	<----
	add.l	#$20000,InstTbl		;due to (-) word offset reference...

	moveq.l	#0,Cycles		;start it with 0 elapsed cycles...
	move.l	Cycles,LastStopCycle

	move.l	#0,PCount
	clr.l	XReg
	clr.l	YReg
	clr.l	AReg		; _____    (set rom/ram if rqrd)
	move.l	#$fffc,a0	; RESET vector!
	moveq.l	#0,d0
	FGETWORD		;D0=apple PC (clean)
	add.l	Mem_Ptr,d0
	move.l	d0,PCount
	move.l	#$01ff0000,Stack	;reset it to top of frame! (high word is used)
	clr.l	StatusSZ
	bset.l	#I_BIT,StatusSZ
	bset.l	#B_BIT,StatusSZ

	move.b	#%00000001,VidMode	;text page 1
	jsr	RefreshVideo


	CLEAR	d0

	DOCYCLE

		;All CPU instructions are labeled in 3 letter CAPS,
		;followed by the Hex Opcode for it... eg: BRK00 .
		;Internal Names are 3 letter CAPS, followed by
		;a _MEM if gets/puts addr, _ACC if gets addr, puts ACC.
		;Enter with: PCount pointing to OpCode + 2 !!!! D1 = word of mem...
		;Internally ( _Acc, _Mem) enters with A0=<EA>
		;All are called directly from DOCYCLE

		;NOTE: Due to a global optimization in 6502 emulation code,
		;      register d0 must always maintain the high word as 0.
		;      Any instruction code/hardware handler MUST obey!
*------------------------- BRK / UNDefined --------------------------*
UND00:
;	jsr	ReportHistory	;<------- DEBUGGING DIAGNOSTIC!
	lea	.UNDMsg,a0
	lea	.UNDMsg2,a1
	bra	BrkPrnt

.UNDMsg	dc.b	"!!! UNDEFINED INST AT $"
.UNDMsg2 dc.b	"0000",0,50

	EVEN

BRK00:
;	jsr	ReportHistory	;<------- DEBUGGING DIAGNOSTIC!
	lea	.BRKMsg,a0
	lea	.BRKMsg2,a1
	bra	BrkPrnt

.BRKMsg	dc.b	"!!! BREAK INST AT $"
.BRKMsg2 dc.b	"0000",0,50,0

	EVEN

BrkPrnt	move.l	PCount,d0		;<--- for our information
	sub.l	Mem_Ptr,d0
	sub.l	#2,d0		;adjust for PostInc'ing of PCount

	move.w	d0,d1		;parse hex digits...
	lsr.w	#8,d0		;1st digit...
	lsr.w	#4,d0
	add.b	#'0',d0
	cmp.b	#'9',d0
	bls	1$
	add.b	#'A'-'9'-1,d0
1$	move.b	d0,(a1)+

	move.w	d1,d0
	lsr.w	#8,d0		;2st digit...
	and.b	#$0f,d0
	add.b	#'0',d0
	cmp.b	#'9',d0
	bls	2$
	add.b	#'A'-'9'-1,d0
2$	move.b	d0,(a1)+

	move.w	d1,d0
	lsr.b	#4,d0		;3rd digit...
	add.b	#'0',d0
	cmp.b	#'9',d0
	bls	3$
	add.b	#'A'-'9'-1,d0
3$	move.b	d0,(a1)+

	move.w	d1,d0		;4th digit...
	and.b	#$0f,d0
	add.b	#'0',d0
	cmp.b	#'9',d0
	bls	4$
	add.b	#'A'-'9'-1,d0
4$	move.b	d0,(a1)+

	move.l	a0,NewStatusMsgPtr	;and set to print!

.DoActualBreak:

	move.l	#$c081,d0			;reset 16k card on reset!
	GETBYTE
;floob	move.l	#$c081,d0
;	GETBYTE

	bset.l	#B_BIT,StatusSZ		;Set Break bit...
	move.l	PCount,d0
	sub.l	Mem_Ptr,d0
	PUSH16
	PUSHSTAT
	bset.l	#I_BIT,StatusSZ		;Set Intrpt Mask
	move.l	#$fffe,a0
	moveq.l	#0,d0
	FGETWORD
	add.l	Mem_Ptr,d0
	move.l	d0,PCount
	addq.l	#7,Cycles
	CLEAR	d0
	DOCYCLE
	

	EVEN
*------------------- ADC ------ Add Mem + Carry -> Acc---------*** STATUS DEPENDANT ***
ADC6D:	MEM_Abs					;ADC $1234
	addq.l	#4,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ	[05]
	bne.b	.bcd		[05]
.dec	lsr.b	#8,StatusCV	[04] Sets/Clrs X flag based on C in Status & Sets Z Flag
	addx.b	d0,AReg		[03] ADDX - Z flag only cleared, not set! = [17]
	STAT_SVZC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV	;Sets/Clrs X flag based on C in Status & Sets Z Flag
	abcd.b	d0,AReg		;ABCD - Z flag only cleared, not set!
	STAT_SZC			;manual says Z not set. Is V???
	DOCYCLE

ADC61:	MEM_PreIndx				;ADC ($06,X)
	addq.l	#6,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	addx.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

ADC71:	MEM_PostIndx				;ADC ($06),Y
	addq.l	#5,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	addx.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

ADC79:	MEM_AbsIndxY				;ADC $1234,Y
	addq.l	#4,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	addx.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

ADC7D:	MEM_AbsIndxX				;ADC $1234,X
	addq.l	#4,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	addx.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

ADC65:	MEM_ZP					;ADC $06
	addq.l	#3,Cycles
	move.b	(Mem_Ptr,d0.l),d0
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	addx.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

ADC75:	MEM_ZPIndxX				;ADC $06,X
	addq.l	#4,Cycles
	move.b	(Mem_Ptr,d0.l),d0
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	addx.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

ADC69:	move.b	d1,d0		;Mem_Immed - ADC #$06
	addq.l	#2,Cycles
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	addx.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

*--------------------------- AND -----------------------------------*
AND2D:	MEM_Abs					;AND $1234
	addq.l	#4,Cycles
	GETBYTE
	and.b	d0,AReg
	STAT_SZ
	DOCYCLE

AND21:	MEM_PreIndx				;AND ($06,X)
        addq.l	#6,Cycles
	GETBYTE
	and.b	d0,AReg
	STAT_SZ
	DOCYCLE

AND31:	MEM_PostIndx				;AND ($06),Y
	addq.l	#5,Cycles
	GETBYTE
	and.b	d0,AReg
	STAT_SZ
	DOCYCLE

AND39:	MEM_AbsIndxY				;AND $1234,Y
	addq.l	#4,Cycles
	GETBYTE
	and.b	d0,AReg
	STAT_SZ
	DOCYCLE

AND3D:	MEM_AbsIndxX				;AND $1234,X
	addq.l	#4,Cycles
	GETBYTE
	and.b	d0,AReg
	STAT_SZ
	DOCYCLE

AND29:						;MEM_Immed - AND #$06
	addq.l	#2,Cycles
	and.b	d1,AReg
	STAT_SZ
	DOCYCLE

AND25:	MEM_ZP					;AND $06
	and.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	addq.l	#3,Cycles
	DOCYCLE

AND35:	MEM_ZPIndxX				;AND $06,X
	and.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	addq.l	#4,Cycles
	DOCYCLE

*--------------------------- ASL -----------------------------------*
ASL0E:	MEM_Abs					;ASL $1234
	addq.l	#6,Cycles
	move.w	d0,-(sp)
	GETBYTE					;must do safe read in case of PDL, DISK, etc...

	move.b	d0,d1
	lsl.b	d1				;lsl leaves V flag clear
	STAT_SZC

	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

ASL1E:	MEM_AbsIndxX				;ASL $1234,X
	addq.l	#7,Cycles
	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1
	lsl.b	d1
	STAT_SZC
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

ASL0A:	lsl.b	AReg				;ASL Acc
	STAT_SZC
	subq.w	#1,PCount
	addq.l	#2,Cycles
	DOCYCLE

ASL06:	MEM_ZP					;ASL $20
	move.b	(Mem_Ptr,d0.l),d1
	lsl.b	d1
	STAT_SZC				;preserve addr in d0
	move.b	d1,(Mem_Ptr,d0.l)
	addq.l	#5,Cycles
	DOCYCLE

ASL16:	MEM_ZPIndxX				;ASL $06,X
	move.b	(Mem_Ptr,d0.l),d1
	lsl.b	d1
	STAT_SZC				;preserve addr in d0
	move.b	d1,(Mem_Ptr,d0.l)
	addq.l	#6,Cycles
	DOCYCLE
*---------------------------- BIT ---------- (Stat Dependant) -------------------*
BIT2C:	MEM_Abs					;BIT $1234
	addq.l	#4,Cycles
	GETBYTE					;(Doesnt Change Acc, just Status)
	move.b	AReg,d1
	and.b	d0,d1

	STAT_SZ					;get Z from result of "and"
	lsr.b	#6,d0			[04]	;copy this bit into V flag
	bfins	d0,StatusCV{31-V_BIT:1}	[10]
	lsr.b	d0			[04]	;and S flag
	bfins	d0,StatusSZ{31-S_BIT:1}	[10]
	DOCYCLE

BIT24:	MEM_ZP					;BIT $06
	addq.l	#3,Cycles
	move.b	(Mem_Ptr,d0.l),d0		;(Doesnt Change Acc, just Status)
	move.b	AReg,d1
	and.b	d0,d1

	STAT_SZ					;get Z from result of "and"
	lsr.b	#6,d0			[04]	;copy this bit into V flag
	bfins	d0,StatusCV{31-V_BIT:1}	[10]
	lsr.b	d0			[04]	;and S flag
	bfins	d0,StatusSZ{31-S_BIT:1}	[10]
	DOCYCLE

*----------------------- BRANCHING! --------------------------------*

BPL10Reg:
	move.l	d1,a6
	REGULATE
	move.l	a6,d1

BPL10:	btst.l	#S_BIT,StatusSZ			;BPL +-disp     branch if plus (s=0)
	bne.b	.NoJmp
	ext.w	d1
	add.w	d1,PCount
	addq.l	#3,Cycles
	DOCYCLE
.NoJmp	addq.l	#2,Cycles
	DOCYCLE

BMI30Reg:
	move.l	d1,a6
	REGULATE
	move.l	a6,d1

BMI30:	btst.l	#S_BIT,StatusSZ			;BMI (+-Disp)   branch if minus (s=1)
	beq.b	.NoJmp
	ext.w	d1
	add.w	d1,PCount
	addq.l	#3,Cycles
	DOCYCLE
.NoJmp	addq.l	#2,Cycles
	DOCYCLE

BCC90:	btst.l	#C_BIT,StatusCV			;BCC +- Disp   Branch if Carry Clear (C=0)
	bne.b	.NoJmp
	ext.w	d1
	add.w	d1,PCount
	addq.l	#3,Cycles
	DOCYCLE
.NoJmp	addq.l	#2,Cycles
	DOCYCLE

BCSB0:	btst.l	#C_BIT,StatusCV			;BCS +-Disp   Branch Carry Set (C=1)
	beq.b	.NoJmp
	ext.w	d1
	add.w	d1,PCount
	addq.l	#3,Cycles
	DOCYCLE
.NoJmp	addq.l	#2,Cycles
	DOCYCLE

BVC50:	btst.l	#V_BIT,StatusCV			;BVC +-Disp  Branch if Overflow clear (V=0)
	bne.b	.NoJmp
	ext.w	d1
	add.w	d1,PCount
	addq.l	#3,Cycles
	DOCYCLE
.NoJmp	addq.l	#2,Cycles
	DOCYCLE

BVS70:	btst.l	#V_BIT,StatusCV			;BVS +-Disp   branch if overflow (V=1) set
	beq.b	.NoJmp
	ext.w	d1
	add.w	d1,PCount
	addq.l	#3,Cycles
	DOCYCLE
.NoJmp	addq.l	#2,Cycles
	DOCYCLE


BNED0Reg:
	move.l	d1,a6
	REGULATE
	move.l	a6,d1

BNED0:	btst.l	#Z_BIT,StatusSZ		[05]	;BNE +-Disp    Branch if not Equal (z=0)
	bne.b	.NoJmp			[05] 	;not taken
	ext.w	d1			[03]
	add.w	d1,PCount		[03]
	addq.l	#3,Cycles		[03]
	DOCYCLE				[25]=44
.NoJmp	addq.l	#2,Cycles
	DOCYCLE

BEQF0Reg:
	move.l	d1,a6
	REGULATE
	move.l	a6,d1

BEQF0:	btst.l	#Z_BIT,StatusSZ			;beq +-Disp    Branch if Equal (z=1)
	beq.b	.NoJmp
	ext.w	d1
	add.w	d1,PCount
	addq.l	#3,Cycles
	DOCYCLE
.NoJmp	addq.l	#2,Cycles
	DOCYCLE


*----------------------- CLEAR STATUS BITS -------------------------*
CLC18:	bclr.l	#C_BIT,StatusCV			;CLC - Clear Carry (C=0)
	subq.w	#1,PCount
	addq.l	#2,Cycles
	DOCYCLE

CLVB8:	bclr.l	#V_BIT,StatusCV			;CLV - Clear Overflow (V=0)
	subq.w	#1,PCount
	addq.l	#2,Cycles
	DOCYCLE

CLDD8:	bclr.l	#D_BIT,StatusSZ			;CLD - Clear Decimal status (D=0)
	subq.w	#1,PCount
	addq.l	#2,Cycles
	DOCYCLE

CLI58:	bclr.l	#I_BIT,StatusSZ			;CLI- Clear Intrpt Mask (enable) (I=0)
	subq.w	#1,PCount
	addq.l	#2,Cycles
	DOCYCLE
*--------------------------- CMP -----------------------------------*
CMPCD:	MEM_Abs					;CMP $1234
	addq.l	#4,Cycles
	GETBYTE					;read mem, subtract from ACC, set SZC
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

CMPC1:	MEM_PreIndx				;CMP ($06,X)
	addq.l	#6,Cycles
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

CMPD1:	MEM_PostIndx				;CMP ($06),Y
	addq.l	#5,Cycles
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

CMPD9:	MEM_AbsIndxY				;CMP $1234,Y
	addq.l	#4,Cycles
   	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

CMPDD:	MEM_AbsIndxX				;CMP $1234,X
	addq.l	#4,Cycles
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

CMPC9:						;MEM_Immed - CMP #$06
	cmp.b	d1,AReg
	STAT_SZiC
	addq.l	#2,Cycles
	DOCYCLE

CMPD5:	MEM_ZPIndxX				;CMP $06,X
	addq.l	#4,Cycles
	cmp.b	(Mem_Ptr,d0.l),AReg
	STAT_SZiC
	DOCYCLE

CMPC5:	MEM_ZP					;CMP $06
	addq.l	#3,Cycles
	cmp.b	(Mem_Ptr,d0.l),AReg
	STAT_SZiC
	DOCYCLE

*-------------------------- CPX ------------------------------------*
CPXEC:	MEM_Abs					;CPX $1234
	addq.l	#4,Cycles
	GETBYTE					;read mem, subtract from XReg, set SZC
	cmp.b	d0,XReg
	STAT_SZiC
	DOCYCLE

CPXE4:	MEM_ZP					;CPX $06
	cmp.b	(Mem_Ptr,d0.l),XReg
	STAT_SZiC
	addq.l	#3,Cycles
	DOCYCLE

CPXE0:	cmp.b	d1,XReg				;MEM_Immed - CPX #$06
	STAT_SZiC
	addq.l	#2,Cycles
	DOCYCLE
*--------------------------- CPY -----------------------------------*
CPYCC:	MEM_Abs					;CPY $1234
	addq.l	#4,Cycles
	GETBYTE					;read mem, subtract from YReg, set SZC
	cmp.b	d0,YReg
	STAT_SZiC
	DOCYCLE

CPYC4:	MEM_ZP					;CPY $06
	cmp.b	(Mem_Ptr,d0.l),YReg
	STAT_SZiC
	addq.l	#3,Cycles
	DOCYCLE

CPYC0:						;MEM_Immed - CPY #$06
	cmp.b	d1,YReg
	STAT_SZiC
	addq.l	#2,Cycles
	DOCYCLE
*--------------------------- DEC -----------------------------------*
DECCE:	MEM_Abs					;DEC $1234
	addq.l	#6,Cycles
	move.b	(Mem_Ptr,d0.l),d1		;fast read now, i/o check at write! (IMPerfect!)
	subq.b	#1,d1				;DEC memory by 1, set SZ
	STAT_SZ
	PUTBYTE					;d0 still there?
	DOCYCLE

DECDE:	MEM_AbsIndxX				;DEX $1234,X
	addq.l	#7,Cycles
	move.b	(Mem_Ptr,d0.l),d1
	subq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

DECD6:	MEM_ZPIndxX				;DEC $06,X
	addq.l	#6,Cycles
	subq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

DECC6:	MEM_ZP					;DEC $06
	addq.l	#5,Cycles
	subq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

*------------------------ DEX/DEY ----------------------------------*
	CNOP	0,4
DEXCA:	subq.b	#1,XReg			[03]	;DEX - Decrement XReg by 1
	STAT_SZ				[04] ?
	subq.w	#1,PCount		[03]
	addq.l	#2,Cycles		[03]
	DOCYCLE				[25]=38

	CNOP	0,4
DEY88:	subq.b	#1,YReg				;DEY - Decrement YReg by 1
	STAT_SZ
	subq.w	#1,PCount
	addq.l	#2,Cycles
	DOCYCLE
*-------------------------- EOR ------------------------------------*
EOR4D:	MEM_Abs					;EOR $1234
	addq.l	#4,Cycles
	GETBYTE
	eor.b	d0,AReg
	STAT_SZ
	DOCYCLE

EOR51:	MEM_PostIndx				;EOR ($06),Y
	addq.l	#5,Cycles
	GETBYTE
	eor.b	d0,AReg
	STAT_SZ
	DOCYCLE

EOR59:	MEM_AbsIndxY				;EOR $1234,Y
	addq.l	#4,Cycles
	GETBYTE
	eor.b	d0,AReg
	STAT_SZ
	DOCYCLE

EOR5D:	MEM_AbsIndxX				;EOR $1234,X
	addq.l	#4,Cycles
	GETBYTE
	eor.b	d0,AReg
	STAT_SZ
	DOCYCLE

EOR41:	MEM_PreIndx				;EOR ($06,X)
	addq.l	#6,Cycles
	GETBYTE
	eor.b	d0,AReg
	STAT_SZ
	DOCYCLE

EOR49:						;MEM_Immed - EOR #$06
	addq.l	#2,Cycles
	eor.b	d1,AReg
	STAT_SZ
	DOCYCLE

EOR45:	MEM_ZP					;EOR $06
	addq.l	#3,Cycles
	move.b	(Mem_Ptr,d0.l),d0
	eor.b	d0,AReg
	STAT_SZ
	DOCYCLE

EOR55:	MEM_ZPIndxX				;EOR $06,X
	addq.l	#4,Cycles
	move.b	(Mem_Ptr,d0.l),d0
	eor.b	d0,AReg
	STAT_SZ
	DOCYCLE
*-------------------------- INC ------------------------------------*
INCEE:	MEM_Abs					;INC $1234
	addq.l	#6,Cycles
	move.b	(Mem_Ptr,d0.l),d1		;INC memory by 1, set SZ
	addq.b	#1,d1
	STAT_SZ
	PUTBYTE					;d0 still there?
	DOCYCLE

INCFE:	MEM_AbsIndxX				;INC $1234,X
	addq.l	#7,Cycles
	move.b	(Mem_Ptr,d0.l),d1
	addq.b	#1,d1
	STAT_SZ
	PUTBYTE					;d0 still there?
	DOCYCLE

INCE6:	MEM_ZP					;INC $06
	addq.l	#5,Cycles
	addq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

INCF6:	MEM_ZPIndxX				;INC $06,X
	addq.l	#6,Cycles
	addq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE
*----------------------- INX / INY ---------------------------------*
INXE8:	subq.w	#1,PCount
	addq.b	#1,XReg				;INX - Increment XReg by 1
	STAT_SZ
	addq.l	#2,Cycles
	DOCYCLE


INYC8:	subq.w	#1,PCount
	addq.b	#1,YReg				;INY - Increment YReg by 1
	STAT_SZ
	addq.l	#2,Cycles
	DOCYCLE
*------------------------ JSR / JMP --------------------------------*
JMP4CReg:
	move.l	d1,a6
	REGULATE
	move.l	a6,d1
JMP4C:						;JMP $xxxx	;MEM_Abs  -> PCount (clean!)
	MEM_Abs
	lea	(Mem_Ptr,d0.l),PCount
	addq.l	#3,Cycles
	DOCYCLE

JSR20:	move.l	PCount,d0			;JSR $1234 (only type)
	sub.l	Mem_Ptr,d0
	PUSH16

	move.w	-1(PCount),d0			;   FGETWORD1PC -> d0
	ror.w	#8,d0				;	""
	lea	(Mem_Ptr,d0.l),PCount
	addq.l	#6,Cycles
	DOCYCLE

JMP6C:						;JMP($xxxx)
;	FGETWORD1PC				;  Mem_Indirect ($xxxx) for JMP only

	move.w	-1(PCount),d0			;FGETWORD1PC -> D0
	ror.w	#8,d0	

	move.w	(Mem_Ptr,d0.l),d0		;FGETWORDd0 -> PCount
	ror.w	#8,d0	
	lea	(Mem_Ptr,d0.l),PCount
	addq.l	#5,Cycles
	DOCYCLE

*--------------------------- LDA -----------------------------------*
LDAAD:	MEM_Abs					;LDA $1234
	addq.l	#4,Cycles
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

LDAA1:	MEM_PreIndx				;LDA ($06,X)
	addq.l	#6,Cycles
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

LDAB1:	MEM_PostIndx				;LDA ($06),Y
	addq.l	#5,Cycles
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

LDAB9:	MEM_AbsIndxY				;LDA $1234,Y
	addq.l	#4,Cycles
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

LDABD:	MEM_AbsIndxX				;LDA $1234,X
	addq.l	#4,Cycles
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

LDAA9:						;MEM_Immed - LDA #$06
	addq.l	#2,Cycles
	move.b	d1,AReg
	STAT_SZ
	DOCYCLE

LDAB5:	MEM_ZPIndxX				;LDA $06,X
	addq.l	#4,Cycles
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

LDAA5:	MEM_ZP					;LDA $06
	addq.l	#3,Cycles
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE
*-------------------------- LDX ------------------------------------*
LDXAE:	MEM_Abs					;LDX $1234
	addq.l	#4,Cycles
	GETBYTE					;read mem into XReg
	move.b	d0,XReg
	STAT_SZ
	DOCYCLE

LDXBE:	MEM_AbsIndxY				;LDX $1234,Y
	addq.l	#4,Cycles
	GETBYTE
	move.b	d0,XReg
	STAT_SZ
	DOCYCLE

LDXA2:						;MEM_Immed - LDX #$06
	addq.l	#2,Cycles
	move.b	d1,XReg
	STAT_SZ
	DOCYCLE

LDXA6:	MEM_ZP					;LDX $06
	addq.l	#3,Cycles	
	move.b	(Mem_Ptr,d0.l),XReg
	STAT_SZ
	DOCYCLE

LDXB6:	MEM_ZPIndxY				;LDX $06,Y
	addq.l	#4,Cycles
	move.b	(Mem_Ptr,d0.l),XReg
	STAT_SZ
	DOCYCLE
*------------------------- LDY -------------------------------------*
LDYAC:	MEM_Abs					;LDY $1234
	addq.l	#4,Cycles
	GETBYTE					;read mem into YReg..
	move.b	d0,YReg
	STAT_SZ
	DOCYCLE

LDYBC:	MEM_AbsIndxX				;LDY $1234,X
	addq.l	#4,Cycles
	GETBYTE
	move.b	d0,YReg
	STAT_SZ
	DOCYCLE

LDYA0:						;MEM_Immed - LDY #$06
	addq.l	#2,Cycles
	move.b	d1,YReg
	STAT_SZ
	DOCYCLE

LDYA4:	MEM_ZP					;LDY $06
	addq.l	#3,Cycles
	move.b	(Mem_Ptr,d0.l),YReg
	STAT_SZ
	DOCYCLE

LDYB4:	MEM_ZPIndxX				;LDY $06,X
	addq.l	#4,Cycles
	move.b	(Mem_Ptr,d0.l),YReg
	STAT_SZ
	DOCYCLE
*------------------------- LSR -------------------------------------*
LSR4E:	MEM_Abs					;LSR $1234
	addq.l	#6,Cycles
	move.w	d0,-(sp)
	GETBYTE					;must do long read in case of PDL, DISK, etc...
	move.b	d0,d1
	lsr.b	#1,d1
	STAT_SZC
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

LSR5E:	MEM_AbsIndxX				;LSR $1234,X
	addq.l	#7,Cycles
	move.w	d0,-(sp)
	GETBYTE					;must do long read in case of PDL, DISK, etc...
	move.b	d0,d1
	lsr.b	#1,d1
	STAT_SZC
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

LSR4A:	subq.w	#1,PCount
	lsr.b	#1,AReg				;LSR Acc
	STAT_SZC
	addq.l	#2,Cycles
	DOCYCLE

LSR46:	MEM_ZP					;LSR $06
	move.b	(Mem_Ptr,d0.l),d1
	lsr.b	#1,d1
	STAT_SZC
	move.b	d1,(Mem_Ptr,d0.l)
	addq.l	#5,Cycles
	DOCYCLE

LSR56:	MEM_ZPIndxX				;LSR $06,X
	move.b	(Mem_Ptr,d0.l),d1
	lsr.b	#1,d1
	STAT_SZC
	move.b	d1,(Mem_Ptr,d0.l)
	addq.l	#6,Cycles
	DOCYCLE

	dc.b	"This program is Copyright 1994 by Kevin Kralian",0
	EVEN
*--------------------------------------------------------------------*
NOPEA:	subq.w	#1,PCount
	addq.l	#2,Cycles
	DOCYCLE					;NOP - Do nothing! time waster
*-------------------------- ORA -------------------------------------*
ORA0D:	MEM_Abs					;ORA $1234
	addq.l	#4,Cycles
	GETBYTE
	or.b	d0,AReg
	STAT_SZ
	DOCYCLE

ORA01:	MEM_PreIndx				;ORA ($20,X)
	addq.l	#6,Cycles
	GETBYTE
	or.b	d0,AReg
	STAT_SZ
	DOCYCLE

ORA19:	MEM_AbsIndxY				;ORA $1234,Y
	addq.l	#4,Cycles
	GETBYTE
	or.b	d0,AReg
	STAT_SZ
	DOCYCLE

ORA1D:	MEM_AbsIndxX				;ORA $1234,X
	addq.l	#4,Cycles
	GETBYTE
	or.b	d0,AReg
	STAT_SZ
	DOCYCLE

ORA11:	MEM_PostIndx				;ORA ($06),Y
	addq.l	#5,Cycles
	GETBYTE
	or.b	d0,AReg
	STAT_SZ
	DOCYCLE

ORA09:						;MEM_Immed - ORA #$20
	addq.l	#2,Cycles
	or.b	d1,AReg
	STAT_SZ
	DOCYCLE

ORA05:	MEM_ZP					;ORA $20
	addq.l	#3,Cycles
	or.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

ORA15:	MEM_ZPIndxX				;ORA $06,X
	addq.l	#4,Cycles
	or.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE
*-------------------------- PHP ------------------------------------*
PHA48:	PUSH	AReg				;PHA - Push AReg
	subq.w	#1,PCount
	addq.l	#3,Cycles
	DOCYCLE

PHP08:	PUSHSTAT				;PHP
	subq.w	#1,PCount
	addq.l	#3,Cycles
	DOCYCLE
*---------------------- PLA / PLP ----------------------------------*
PLA68:	addq.l	#4,Cycles
	PULL	AReg				;PLA - Pull Accum from stack
	tst.b	AReg
	STAT_SZ
	subq.w	#1,PCount
	DOCYCLE

PLP28:	addq.l	#4,Cycles
	PULLSTAT				;PLP
	subq.w	#1,PCount
	DOCYCLE
*-------------------------- ROL (through Carry)----(Status Dependant)---------------*

ROL2E:	MEM_Abs					;ROL $1234
	addq.l	#6,Cycles
.rol	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1				;needs to be there for PUTBYTE anyways...

	lsl.b	d1			[04]	;shift it
	lsr.b	#C_BIT,StatusCV		[04]	;and add in carry bit...
	add.b	StatusCV,d1		[03]
	STAT_SZ				[06]	;and get SZ flags... (faster than ROXL)
	move.b	d0,StatusCV		[03]	;get Carry Flag

	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

ROL3E:	MEM_AbsIndxX				;ROL $1234,X
	addq.l	#7,Cycles
.rol	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1

	lsl.b	d1
	lsr.b	#C_BIT,StatusCV
	add.b	StatusCV,d1
	STAT_SZ
	move.b	d0,StatusCV

	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE



ROL26:	MEM_ZP					;ROL $06
	addq.l	#5,Cycles
.rol	move.b	(Mem_Ptr,d0.l),d1		;FGETBYTE

	lsr.b	#C_BIT+1,StatusCV
	roxl.b	#1,d1
	STAT_SZC

	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE

ROL36:	MEM_ZPIndxX				;ROL $06,X
	addq.l	#6,Cycles
.rol	move.b	(Mem_Ptr,d0.l),d1		;FGETBYTE
	lsr.b	#C_BIT+1,StatusCV
	roxl.b	#1,d1
	STAT_SZC
	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE


	CNOP	0,4
ROL2A:	subq.w	#1,PCount			;ROL Acc
	addq.l	#2,Cycles
	lsr.b	#C_BIT+1,StatusCV	[04]
	roxl.b	#1,AReg			[12]
	STAT_SZC			[10]  26
	DOCYCLE


*-------------------------- ROR (through Carry)------------------------*
ROR6E:	MEM_Abs					;ROR $2134
	addq.l	#6,Cycles
.ror	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1				;needs to be there for PUTBYTE anyways...
	lsr.b	#C_BIT+1,StatusCV		;Sets/Clrs Extend (X) flag based on C
	roxr.b	#1,d1				;(this always clears V in CCR)
	STAT_SZC
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

ROR66:	MEM_ZP					;ROR $06
	addq.l	#5,Cycles	
.ror	move.b	(Mem_Ptr,d0.l),d1
	lsr.b	#C_BIT+1,StatusCV
	roxr.b	#1,d1
	STAT_SZC
	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE


ROR76:	MEM_ZPIndxX				;ROR $06,X
	addq.l	#6,Cycles
.ror	move.b	(Mem_Ptr,d0.l),d1
	lsr.b	#C_BIT+1,StatusCV
	roxr.b	#1,d1
	STAT_SZC
	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE

ROR7E:	MEM_AbsIndxX				;ROR $1234,X
	addq.l	#7,Cycles
.ror	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1	
	lsr.b	#C_BIT+1,StatusCV
	roxr.b	#1,d1
	STAT_SZC
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

	CNOP	0,4
ROR6A:	subq.w	#1,PCount			;ROR ACC
	lsr.b	#C_BIT+1,StatusCV
	roxr.b	#1,AReg
	STAT_SZC
	addq.l	#2,Cycles
	DOCYCLE
*---------------------- RTI / RTS  -----------------------------------*
RTI40:	addq.l	#7,Cycles
	PULLSTAT				;RTI - Return From Intrpt
	PULL16
	lea	(Mem_Ptr,d0.l),PCount
	DOCYCLE

RTS60:	PULL16					;RTS - Return from Subroutine
	lea	1(Mem_Ptr,d0.l),PCount
	addq.l	#6,Cycles
	DOCYCLE
*------------------------- SBC -------------------------------------* STATUS DEPENDANT!!!
SBCE5:	MEM_ZP					;SBC $06
	addq.l	#3,Cycles
	GETBYTE
	eor.b	#C_HEX,StatusCV		[06] 	;for subtraction, C flag works backwards
	btst.l	#D_BIT,StatusSZ		[05]
	bne.b	.bcd			[05]
.dec	lsr.b	#8,StatusCV		[04]	;Sets/Clrs X flag based on C & Sets Z Flag
	subx.b	d0,AReg			[03]	; SUBX - Z flag only cleared, not set!
	STAT_SVZiC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV			;Sets/Clrs X flag based on C & Sets Z Flag
	sbcd.b	d0,AReg				;SBCD - Z flag only cleared, not set!
	STAT_SZiC				;manual says Z not set. Is V???
	DOCYCLE


SBCE1:	MEM_PreIndx				;SBC ($06,X)
	addq.l	#6,Cycles
	GETBYTE
	eor.b	#C_HEX,StatusCV
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	subx.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

SBCED:	MEM_Abs					;SBC $1234
	addq.l	#4,Cycles
	GETBYTE
	eor.b	#C_HEX,StatusCV
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	subx.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

SBCF1:	MEM_PostIndx				;SBC ($06),Y
	addq.l	#5,Cycles
	GETBYTE
	eor.b	#C_HEX,StatusCV
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	subx.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

SBCF9:	MEM_AbsIndxY				;SBC $1234,Y
	addq.l	#4,Cycles
	GETBYTE
	eor.b	#C_HEX,StatusCV
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	subx.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

SBCFD:	MEM_AbsIndxX				;SBC $1234,X
	addq.l	#4,Cycles
	GETBYTE
	eor.b	#C_HEX,StatusCV
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	subx.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

SBCE9:						;MEM_Immed - SBC #$06
	addq.l	#2,Cycles
	move.b	d1,d0
	eor.b	#C_HEX,StatusCV
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	subx.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE


SBCF5:	MEM_ZPIndxX				;SBC $06,X
	addq.l	#4,Cycles
	move.b	(Mem_Ptr,d0.l),d0
	eor.b	#C_HEX,StatusCV
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	subx.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

*-------------------- SET STATUS BITS ------------------------------*
SEI78:	subq.w	#1,PCount
	bset.l	#I_BIT,StatusSZ			;SEI - Set Interrupt Mask (disable intrps)
	addq.l	#2,Cycles
	DOCYCLE

SEDF8:	subq.w	#1,PCount
	bset.l	#D_BIT,StatusSZ			;SED - Set Decimal mode (D=1)
	addq.l	#2,Cycles
	DOCYCLE

SEC38:	subq.w	#1,PCount
	bset.l	#C_BIT,StatusCV			;SEC - Set Carry (C=1)
	addq.l	#2,Cycles
	DOCYCLE
*-------------------------- STA ------------------------------------*
STA81:	MEM_PreIndx				;STA ($06,X)
	addq.l	#6,Cycles
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

STA8D:	MEM_Abs					;STA $1234
	addq.l	#4,Cycles
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

STA91:	MEM_PostIndx				;STA ($06),Y
	addq.l	#6,Cycles
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

STA99:	MEM_AbsIndxY				;STA $1234,Y
	addq.l	#5,Cycles
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

STA9D:	MEM_AbsIndxX				;STA $1234,X
	addq.l	#5,Cycles
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

STA85:	MEM_ZP					;STA $06
	move.b	AReg,(Mem_Ptr,d0.l)
	addq.l	#3,Cycles
	DOCYCLE

STA95:	MEM_ZPIndxX				;STA $06,X
	move.b	AReg,(Mem_Ptr,d0.l)
	addq.l	#4,Cycles
	DOCYCLE
*-------------------------- STX ------------------------------------*
STX8E:	MEM_Abs					;STX $1234
	addq.l	#4,Cycles
	move.b	XReg,d1
	PUTBYTE_DOCYCLE

STX96:	MEM_ZPIndxY				;STX $06,Y
	move.b	XReg,(Mem_Ptr,d0.l)
	addq.l	#4,Cycles
	DOCYCLE

STX86:	MEM_ZP					;STX $06
	move.b	XReg,(Mem_Ptr,d0.l)
	addq.l	#3,Cycles
	DOCYCLE
*-------------------------- STY ------------------------------------*
STY8C:	MEM_Abs					;STY $1234
	addq.l	#4,Cycles
	move.b	YReg,d1
	PUTBYTE_DOCYCLE

STY94:	MEM_ZPIndxX				;STY $06,X
	move.b	YReg,(Mem_Ptr,d0.l)
	addq.l	#4,Cycles
	DOCYCLE

STY84:	MEM_ZP					;STY $06
	move.b	YReg,(Mem_Ptr,d0.l)
	addq.l	#3,Cycles
	DOCYCLE
*------------------------ TRANSFER REGS ----------------------------*
TXA8A:	subq.w	#1,PCount
	move.b	XReg,AReg			;TXA - Transfer XReg to AReg
	STAT_SZ
	addq.l	#2,Cycles
	DOCYCLE

TYA98:	subq.w	#1,PCount
	move.b	YReg,AReg			;TYA - Transfer Yreg to AReg
	STAT_SZ
	addq.l	#2,Cycles
	DOCYCLE

TXS9A:	subq.w	#1,PCount
	swap	Stack
	move.b	XReg,Stack			;TXS - Transfer XREG to StackPtr
	swap	Stack
	addq.l	#2,Cycles
	DOCYCLE

TAYA8:	subq.w	#1,PCount
	move.b	AReg,YReg			;TAY - Transfer Acc to YReg
	STAT_SZ
	addq.l	#2,Cycles
	DOCYCLE

TAXAA:	subq.w	#1,PCount
	move.b	AReg,XReg			;TAX - Transfer Acc to XReg
	STAT_SZ
	addq.l	#2,Cycles
	DOCYCLE

TSXBA:	subq.w	#1,PCount
	swap	Stack
	move.b	Stack,XReg			;TSX - move StackPtr to XReg
	STAT_SZ
	swap	Stack
	addq.l	#2,Cycles
	DOCYCLE


;**************************
	CNOP	0,4

*----------------------------- Inst Pairs !!!------------------------------------*
*********************************************************************
* DEX/INX/DEY/INY + BPL/BMI/BNE/BEQ Pairs...
*********************************************************************

***************** DEX + Bxx ***************************

DEXCA_BNED0:
	cmp.b	#$fd,(PCount)			;is the BNE at {-3} ?
	beq.b	.GotBNE
.dex	subq.b	#1,XReg		[03]
	beq.b	.NoJmp		[05]
.bne	STAT_SZ		[12] ?
	addq.l	#5,Cycles	[03]
	move.b	(PCount)+,d1	[07]
	ext.w	d1		[03]
	add.w	d1,PCount	[03]
	DOCYCLE			[25] = 64 cycles (branch taken) (vs 93 for seperate insts)
.NoJmp	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

.GotBNE						; INY + BNE {-3}  (delay Lp!)
						; [02] +  [03]  = 5 each
	addq.l	#1,PCount
	addq.l	#4,Cycles

	subq.b	#1,XReg				;add AReg*5 to cycles
	add.l	XReg,Cycles
	lsl.l	#2,XReg
	add.l	XReg,Cycles

	REGULATE

	moveq.l	#0,XReg
	STAT_SZ
	DOCYCLE

DEXCA_BEQF0:
.dex	subq.b	#1,XReg
	bne.b	.NoJmp		;decision...
.beq
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

DEXCA_BPL10:
.dex	subq.b	#1,XReg
	bmi.b	.NoJmp		;decision...
.bpl
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

DEXCA_BMI30:
.dex	subq.b	#1,XReg
	bpl.b	.NoJmp		;decision...
.bmi
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

		************ DEY + Bcc ***********
DEY88_BNED0:
	cmp.b	#$fd,(PCount)			;is the BNE at {-3} ?
	beq.b	.GotBNE

.dey	subq.b	#1,YReg		[03]
	beq.b	.NoJmp		[05]
.bne
	STAT_SZ		[12] ?
	addq.l	#5,Cycles	[03]
	move.b	(PCount)+,d1	[07]
	ext.w	d1		[03]
	add.w	d1,PCount	[03]
	DOCYCLE			[25] = 64 cycles (branch taken) (vs 93 for seperate insts)
.NoJmp
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

.GotBNE						; INY + BNE {-3}  (delay Lp!)
						; [02] +  [03]  = 5 each

	addq.l	#1,PCount
	addq.l	#4,Cycles

	subq.b	#1,YReg				;add AReg*5 to cycles
	add.l	YReg,Cycles
	lsl.l	#2,YReg
	add.l	YReg,Cycles

	REGULATE
	
	moveq.l	#0,YReg
	STAT_SZ
	DOCYCLE

DEY88_BEQF0:
.dey	subq.b	#1,YReg
	bne.b	.NoJmp		;decision...
.beq
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

DEY88_BPL10:
.dey	subq.b	#1,YReg
	bmi.b	.NoJmp		;decision...
.bpl
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

DEY88_BMI30:
.dey	subq.b	#1,YReg
	bpl.b	.NoJmp		;decision...
.bmi
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

		******** INX + Bcc ***********
INXE8_BNED0:
	cmp.b	#$fd,(PCount)			;is the BNE at {-3} ?
	beq.b	.GotBNE

.inx	addq.b	#1,XReg		[03]
	beq.b	.NoJmp		[05]
.bne	
	STAT_SZ		[12] ?
	addq.l	#5,Cycles	[03]
	move.b	(PCount)+,d1	[07]
	ext.w	d1		[03]
	add.w	d1,PCount	[03]
	DOCYCLE			[25] = 64 cycles (branch taken) (vs 93 for seperate insts)
.NoJmp
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

.GotBNE						; INY + BNE {-3}  (delay Lp!)
						; [02] +  [03]  = 5 each

	addq.l	#1,PCount
	addq.l	#4,Cycles

	addq.b	#1,XReg				;add AReg*5 to cycles
	neg.b	XReg
	add.l	XReg,Cycles
	lsl.l	#2,XReg
	add.l	XReg,Cycles

	REGULATE

	moveq.l	#0,XReg
	STAT_SZ
	DOCYCLE

INXE8_BEQF0:
.inx	addq.b	#1,XReg
	bne.b	.NoJmp		;decision...
.beq	
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

INXE8_BPL10:
.inx	addq.b	#1,XReg
	bmi.b	.NoJmp		;decision...
.bpl	
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp	
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

INXE8_BMI30:
.inx	addq.b	#1,XReg
	bpl.b	.NoJmp		;decision...
.bmi	
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp	
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

		******** INY + Bcc ***********

INYC8_BNED0:
	cmp.b	#$fd,(PCount)			;is the BNE at {-3} ?
	beq.b	.GotBNE
.iny	addq.b	#1,YReg		[03]
	beq.b	.NoJmp		[05]
.bne	
	STAT_SZ		[12] ?
	addq.l	#5,Cycles	[03]
	move.b	(PCount)+,d1	[07]
	ext.w	d1		[03]
	add.w	d1,PCount	[03]
	DOCYCLE			[25] = 64 cycles (branch taken) (vs 93 for seperate insts)
.NoJmp	
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

.GotBNE						; INY + BNE {-3}  (delay Lp!)
						; [02] +  [03]  = 5 each
	addq.l	#1,PCount
	addq.l	#4,Cycles

	addq.b	#1,YReg				;add AReg*5 to cycles
	neg.b	YReg
	add.l	YReg,Cycles
	lsl.l	#2,YReg
	add.l	YReg,Cycles

	REGULATE
	
	moveq.l	#0,YReg
	STAT_SZ
	DOCYCLE





INYC8_BEQF0:
.iny	addq.b	#1,YReg
	bne.b	.NoJmp		;decision...
.beq	
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp	
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

INYC8_BPL10:
.iny	addq.b	#1,YReg
	bmi.b	.NoJmp		;decision...
.bpl	
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp	
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

INYC8_BMI30:
.iny	addq.b	#1,YReg
	bpl.b	.NoJmp		;decision...
.bmi	
	STAT_SZ
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp	
	STAT_SZ
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

**************************************************************************


SBCE9_01:					;MEM_Immed - SBC #$01
	cmp.w	#$d0fc,(PCount)			;is next inst BNE {-4} ?
	beq.b	.GotBNE
	addq.l	#2,Cycles
	move.b	d1,d0
	eor.b	#C_HEX,StatusCV
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	lsr.b	#8,StatusCV
	subx.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	lsr.b	#8,StatusCV
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

.GotBNE						;SBC #$01 + BNE {-4}  (delay Lp!)
						;  [02]   +  [03]  = 5 each
	addq.l	#2,PCount
	addq.l	#4,Cycles

	subq.b	#1,AReg				;add AReg*5 to cycles
	add.l	AReg,Cycles
	lsl.l	#2,AReg
	add.l	AReg,Cycles

	REGULATE

	moveq.l	#0,AReg
	STAT_SVZiC
	DOCYCLE


**********************************************************
**** Common Simple Pairs.....
**********************************************************

INXE8_INXE8:
.inx	addq.b	#2,XReg				;INX twice!
	STAT_SZ
	addq.l	#4,Cycles
	DOCYCLE


INYC8_INYC8:
.iny	addq.b	#2,YReg
	STAT_SZ
	addq.l	#4,Cycles
	DOCYCLE

DEXCA_DEXCA:
.dex	subq.b	#2,XReg
	STAT_SZ
	addq.l	#4,Cycles
	DOCYCLE

DEY88_DEY88:
.dey	subq.b	#2,YReg
	STAT_SZ
	addq.l	#4,Cycles
	DOCYCLE


ASL0A_ASL0A:
.asl	lsl.b	#2,AReg
	STAT_SZC
	addq.l	#4,Cycles
	DOCYCLE

LSR4A_LSR4A:
.lsr_x2	lsr.b	#2,AReg				;LSR Acc
	STAT_SZC
	addq.l	#4,Cycles
	DOCYCLE

PLA68_PLA68:
.pla	swap	Stack
	addq.b	#2,Stack
	move.b	(Mem_Ptr,Stack.w),AReg
	STAT_SZ
	swap	Stack
	addq.l	#8,Cycles
	DOCYCLE

JMP6C_BUGFF:					;JMP($xxxx) w/ end-of page wrap-around bug!
;	FGETWORD1PC				;  Mem_Indirect ($xxxx) for JMP only

	move.w	-1(PCount),d0
	ror.w	#8,d0				;D0 = Pre-indirect address

	moveq.l	#0,d1
	move.b	(Mem_Ptr,d0.l),d1		;get low byte of final addr...
	move.b	#0,d0
	lsl.w	#8,d1
	move.b	(Mem_Ptr,d0.l),d1		;and get hi byte of dest in lo byte!
	ror.w	#8,d1				;and rotate into position...
	
	lea	(Mem_Ptr,d1.l),PCount
	addq.l	#5,Cycles
	DOCYCLE



;****************************************************
; New opts as of 05/01/94 follow...

TXA8A_PHA48:
.txa	move.b	XReg,AReg			;TXA - Transfer XReg to AReg
	STAT_SZ
.pha	PUSH	AReg				;PHA - Push AReg
	addq.l	#5,Cycles
	DOCYCLE

TYA98_PHA48:
.tya	move.b	YReg,AReg
	STAT_SZ
.pha	PUSH	AReg
	addq.l	#5,Cycles
	DOCYCLE

PLA68_PHA48:
.PlaPha	swap	Stack
	move.b	1(Mem_Ptr,Stack.w),AReg		; [08]
	STAT_SZ
	swap	Stack
	addq.l	#7,Cycles
	DOCYCLE

PLA68_TAYA8:
.pla	PULL	AReg
.tay	move.b	AReg,YReg
	STAT_SZ
	addq.l	#6,Cycles
	DOCYCLE	

PLA68_TAXAA:
.pla	PULL	AReg
.tax	move.b	AReg,XReg
	STAT_SZ
	addq.l	#6,Cycles
	DOCYCLE

*************************************************************
** CLC + ADC pairs - No need to do BCLR #C_BIT since is reset by Stat after ADD
** No need to check C, always clear for Add! No need for ADDX! C Always clear!
*************************************************************
CLC18_ADC6D:		;(no need to CLC, gets reset by ADC)
	OLDMEM_Abs				;ADC $1234
	addq.l	#6,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ		[05]
	bne.b	.bcd			[05]
.dec	add.b	d0,AReg			[03]	;ADDX - Z flag only cleared, not set! = [17]
	STAT_SVZC
	DOCYCLE
.bcd	moveq.b	#0,d1				;Clrs X flag & Sets Z Flag
	abcd.b	d0,AReg				;ABCD - Z flag only cleared, not set!
	STAT_SZC				;manual says Z not set. Is V???
	DOCYCLE

CLC18_ADC61:
	OLDMEM_PreIndx				;ADC ($06,X)
	addq.l	#8,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	add.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	moveq.b	#0,d1
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

CLC18_ADC71:
	OLDMEM_PostIndx				;ADC ($06),Y
	addq.l	#7,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	add.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	moveq.b	#0,d1
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

CLC18_ADC79:
	OLDMEM_AbsIndxY				;ADC $1234,Y
	addq.l	#6,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	add.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	moveq.b	#0,d1
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

CLC18_ADC7D:
	OLDMEM_AbsIndxX				;ADC $1234,X
	addq.l	#6,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	add.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	moveq.b	#0,d1
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

CLC18_ADC65:
	OLDMEM_ZP				;ADC $06
	addq.l	#5,Cycles
	move.b	(Mem_Ptr,d0.l),d0
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	add.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	moveq.b	#0,d1
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

CLC18_ADC75:
	OLDMEM_ZPIndxX				;ADC $06,X
	addq.l	#6,Cycles
	move.b	(Mem_Ptr,d0.l),d0
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	add.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	moveq.b	#0,d1
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

CLC18_ADC69:
	OLDMEM_ImmedD0				;Mem_Immed - ADC #$06
	addq.l	#4,Cycles
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	add.b	d0,AReg
	STAT_SVZC
	DOCYCLE
.bcd	moveq.b	#0,d1
	abcd.b	d0,AReg
	STAT_SZC
	DOCYCLE

*************************************************************
** SEC + SBC pairs - No need to do BSET #C_BIT since is reset by Stat after SUB
** No need to check C, always set for this sub! No need for SUBX! C Always clear!
*************************************************************

SEC38_SBCED:					;(no need to SEC, gets reset by SBC)
	OLDMEM_Abs				;SBC $1234
	addq.l	#6,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ		[05]
	bne.b	.bcd			[05]
.dec	sub.b	d0,AReg			[03] 	;ADDX - Z flag only cleared, not set! = [17]
	STAT_SVZiC
	DOCYCLE
.bcd	moveq.b	#0,d1				;Clrs X flag & Sets Z Flag
	sbcd.b	d0,AReg				;ABCD - Z flag only cleared, not set!
	STAT_SZiC				;manual says Z not set. Is V???
	DOCYCLE

SEC38_SBCE1:
	OLDMEM_PreIndx				;SBC ($06,X)
	addq.l	#8,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	sub.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	moveq.b	#0,d1
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

SEC38_SBCF1:
	OLDMEM_PostIndx				;SBC ($06),Y
	addq.l	#7,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	sub.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	moveq.b	#0,d1
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

SEC38_SBCF9:
	OLDMEM_AbsIndxY				;SBC $1234,Y
	addq.l	#6,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	sub.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	moveq.b	#0,d1
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

SEC38_SBCFD:
	OLDMEM_AbsIndxX				;SBC $1234,X
	addq.l	#6,Cycles
	GETBYTE
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	sub.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	moveq.b	#0,d1
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

SEC38_SBCE5:
	OLDMEM_ZP				;SBC $06
	addq.l	#5,Cycles
	move.b	(Mem_Ptr,d0.l),d0
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	sub.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	moveq.b	#0,d1
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

SEC38_SBCF5:
	OLDMEM_ZPIndxX				;SBC $06,X
	addq.l	#6,Cycles
	move.b	(Mem_Ptr,d0.l),d0
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	sub.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	moveq.b	#0,d1
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

SEC38_SBCE9:
	OLDMEM_ImmedD0				;Mem_Immed - SBC #$06
	addq.l	#4,Cycles
	btst.l	#D_BIT,StatusSZ
	bne.b	.bcd
.dec	sub.b	d0,AReg
	STAT_SVZiC
	DOCYCLE
.bcd	moveq.b	#0,d1
	sbcd.b	d0,AReg
	STAT_SZiC
	DOCYCLE

; New opts as of 05/08/94 follow...
*************************************************************
** AnythingX + CPX...   Anything that affects X reg then Compares it...
*************************************************************

DEXCA_CPXEC:
	subq.b	#1,XReg				;INX - Increment XReg by 1
	addq.l	#6,Cycles
.cpx	OLDMEM_Abs				;CPX $1234
	GETBYTE
	cmp.b	d0,XReg
	STAT_SZiC
	DOCYCLE

DEXCA_CPXE4:
	subq.b	#1,XReg				;INX - Increment XReg by 1
	addq.l	#5,Cycles
.cpx	OLDMEM_ZP				;CPX $06
	cmp.b	(Mem_Ptr,d0.l),XReg
	STAT_SZiC
	DOCYCLE

DEXCA_CPXE0:
	subq.b	#1,XReg				;INX - Increment XReg by 1
	addq.l	#4,Cycles
.cpx	OLDMEM_ImmedD0
	cmp.b	d0,XReg				;MEM_Immed - CPX #$06
	STAT_SZiC
	DOCYCLE

*----------*

INXE8_CPXEC:
	addq.b	#1,XReg				;INX - Increment XReg by 1
	addq.l	#6,Cycles
.cpx	OLDMEM_Abs				;CPX $1234
	GETBYTE
	cmp.b	d0,XReg
	STAT_SZiC
	DOCYCLE

INXE8_CPXE4:
	addq.b	#1,XReg				;INX - Increment XReg by 1
	addq.l	#5,Cycles
.cpx	OLDMEM_ZP				;CPX $06
	cmp.b	(Mem_Ptr,d0.l),XReg
	STAT_SZiC
	DOCYCLE

INXE8_CPXE0:
	addq.b	#1,XReg				;INX - Increment XReg by 1
	addq.l	#4,Cycles
.cpx	OLDMEM_ImmedD0
	cmp.b	d0,XReg				;MEM_Immed - CPX #$06
	STAT_SZiC
	DOCYCLE

*************************************************************
** AnythingX + CPX...   Anything that affects X reg then Compares it...
*************************************************************
DEY88_CPYCC:
	subq.b	#1,YReg				;INY - Increment YReg by 1
	addq.l	#6,Cycles
.cpy	OLDMEM_Abs				;CPY $1234
	GETBYTE					;read mem, subtract from YReg, set SZC
	cmp.b	d0,YReg
	STAT_SZiC
	DOCYCLE

DEY88_CPYC4:
	subq.b	#1,YReg				;INY - Increment YReg by 1
	addq.l	#5,Cycles
.cpy	OLDMEM_ZP				;CPY $06
	cmp.b	(Mem_Ptr,d0.l),YReg
	STAT_SZiC
	DOCYCLE

DEY88_CPYC0:
	subq.b	#1,YReg				;INY - Increment YReg by 1
	addq.l	#4,Cycles
.cpy	OLDMEM_ImmedD0				;MEM_Immed - CPY #$06
	cmp.b	d0,YReg
	STAT_SZiC
	DOCYCLE

*----------*

INYC8_CPYCC:
	addq.b	#1,YReg				;INY - Increment YReg by 1
	addq.l	#6,Cycles
.cpy	OLDMEM_Abs				;CPY $1234
	GETBYTE					;read mem, subtract from YReg, set SZC
	cmp.b	d0,YReg
	STAT_SZiC
	DOCYCLE

INYC8_CPYC4:
	addq.b	#1,YReg				;INY - Increment YReg by 1
	addq.l	#5,Cycles
.cpy	OLDMEM_ZP				;CPY $06
	cmp.b	(Mem_Ptr,d0.l),YReg
	STAT_SZiC
	DOCYCLE

INYC8_CPYC0:
	addq.b	#1,YReg				;INY - Increment YReg by 1
	addq.l	#4,Cycles
.cpy	OLDMEM_ImmedD0				;MEM_Immed - CPY #$06
	cmp.b	d0,YReg
	STAT_SZiC
	DOCYCLE

; New opts as of 05/08/94 follow...
*************************************************************
** ANYInst + LDA pairs... No need to do STAT_SZ, reset by LDA!
*************************************************************

INXE8_LDAA1:
.inx	addq.b	#1,XReg				;INX
	addq.l	#8,Cycles
.lda	OLDMEM_PreIndx				;LDA ($06,X)
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

INXE8_LDAA5:
.inx	addq.b	#1,XReg				;INX
	addq.l	#5,Cycles
.lda	OLDMEM_ZP				;LDA $06
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

INXE8_LDAA9:
.inx	addq.b	#1,XReg				;INX
	addq.l	#2,Cycles
.lda	OLDMEM_ImmedD0				;LDA #$06
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

INXE8_LDAAD:
.inx	addq.b	#1,XReg				;INX
	addq.l	#6,Cycles
.lda	OLDMEM_Abs				;LDA $1234
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

INXE8_LDAB1:
.inx	addq.b	#1,XReg				;INX
	addq.l	#7,Cycles
.lda	OLDMEM_PostIndx				;LDA ($06),Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

INXE8_LDAB5:
.inx	addq.b	#1,XReg				;INX
	addq.l	#6,Cycles
.lda	OLDMEM_ZPIndxX				;LDA $06,X
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

INXE8_LDAB9:
.inx	addq.b	#1,XReg				;INX
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxY				;LDA $1234,Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

INXE8_LDABD:
.inx	addq.b	#1,XReg				;INX
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxX				;LDA $1234,X
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

*---------*

DEXCA_LDAA1:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#8,Cycles
.lda	OLDMEM_PreIndx				;LDA ($06,X)
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

DEXCA_LDAA5:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#5,Cycles
.lda	OLDMEM_ZP				;LDA $06
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

DEXCA_LDAA9:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#4,Cycles
.lda	OLDMEM_ImmedD0				;LDA #$06
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

DEXCA_LDAAD:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#6,Cycles
.lda	OLDMEM_Abs				;LDA $1234
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

DEXCA_LDAB1:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#7,Cycles
.lda	OLDMEM_PostIndx				;LDA ($06),Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

DEXCA_LDAB5:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#6,Cycles
.lda	OLDMEM_ZPIndxX				;LDA $06,X
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

DEXCA_LDAB9:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxY				;LDA $1234,Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

DEXCA_LDABD:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxX				;LDA $1234,X
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

*---------*
** INY + LDA... No need to do STAT_SZ after INY, reset by LDA!

INYC8_LDAA1:
.iny	addq.b	#1,YReg				;INY
	addq.l	#8,Cycles
.lda	OLDMEM_PreIndx				;LDA ($06,X)
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

INYC8_LDAA5:
.iny	addq.b	#1,YReg				;INY
	addq.l	#5,Cycles
.lda	OLDMEM_ZP				;LDA $06
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

INYC8_LDAA9:
.iny	addq.b	#1,YReg				;INY
	addq.l	#4,Cycles
.lda	OLDMEM_ImmedD0				;LDA #$06
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

INYC8_LDAAD:
.iny	addq.b	#1,YReg				;INY
	addq.l	#6,Cycles
.lda	OLDMEM_Abs				;LDA $1234
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

INYC8_LDAB1:
.iny	addq.b	#1,YReg				;INY
	addq.l	#7,Cycles
.lda	OLDMEM_PostIndx				;LDA ($06),Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

INYC8_LDAB5:
.iny	addq.b	#1,YReg				;INY
	addq.l	#6,Cycles
.lda	OLDMEM_ZPIndxX				;LDA $06,X
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

INYC8_LDAB9:
.iny	addq.b	#1,YReg				;INY
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxY				;LDA $1234,Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

INYC8_LDABD:
.iny	addq.b	#1,YReg				;INY
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxX				;LDA $1234,X
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

*---------*

DEY88_LDAA1:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#8,Cycles
.lda	OLDMEM_PreIndx				;LDA ($06,X)
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

DEY88_LDAA5:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#5,Cycles
.lda	OLDMEM_ZP				;LDA $06
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

DEY88_LDAA9:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#4,Cycles
.lda	OLDMEM_ImmedD0				;LDA #$06
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

DEY88_LDAAD:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#6,Cycles
.lda	OLDMEM_Abs				;LDA $1234
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

DEY88_LDAB1:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#7,Cycles
.lda	OLDMEM_PostIndx				;LDA ($06),Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

DEY88_LDAB5:	
.dey	subq.b	#1,YReg				;DEY
	addq.l	#6,Cycles
.lda	OLDMEM_ZPIndxX				;LDA $06,X
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

DEY88_LDAB9:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxY				;LDA $1234,Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

DEY88_LDABD:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxX				;LDA $1234,X
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

*---------*
** TAY + LDA... No need to do STAT_SZ after INY, reset by LDA!

TAYA8_LDAA1:
.tay	move.b	AReg,YReg			;TAY
	addq.l	#8,Cycles
.lda	OLDMEM_PreIndx				;LDA ($06,X)
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

TAYA8_LDAA5:
.tay	move.b	AReg,YReg			;TAY
	addq.l	#5,Cycles
.lda	OLDMEM_ZP				;LDA $06
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

TAYA8_LDAA9:
.tay	move.b	AReg,YReg			;TAY
	addq.l	#4,Cycles
.lda	OLDMEM_ImmedD0				;LDA #$06
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

TAYA8_LDAAD:
.tay	move.b	AReg,YReg			;TAY
	addq.l	#6,Cycles
.lda	OLDMEM_Abs				;LDA $1234
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

TAYA8_LDAB1:
.tay	move.b	AReg,YReg			;TAY
	addq.l	#7,Cycles
.lda	OLDMEM_PostIndx				;LDA ($06),Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

TAYA8_LDAB5:
.tay	move.b	AReg,YReg			;TAY
	addq.l	#6,Cycles
.lda	OLDMEM_ZPIndxX				;LDA $06,X
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

TAYA8_LDAB9:
.tay	move.b	AReg,YReg			;TAY
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxY				;LDA $1234,Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

TAYA8_LDABD:
.tay	move.b	AReg,YReg			;TAY
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxX				;LDA $1234,X
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

*---- TAX + LDA -----*

TAXAA_LDAA1:
.tax	move.b	AReg,XReg			;TAX
	addq.l	#8,Cycles
.lda	OLDMEM_PreIndx				;LDA ($06,X)
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

TAXAA_LDAA5:
.tax	move.b	AReg,XReg			;TAX
	addq.l	#5,Cycles
.lda	OLDMEM_ZP				;LDA $06
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

TAXAA_LDAA9:
.tax	move.b	AReg,XReg			;TAX
	addq.l	#4,Cycles
.lda	OLDMEM_ImmedD0				;LDA #$06
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

TAXAA_LDAAD:
.tax	move.b	AReg,XReg			;TAX
	addq.l	#6,Cycles
.lda	OLDMEM_Abs				;LDA $1234
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

TAXAA_LDAB1:
.tax	move.b	AReg,XReg			;TAX
	addq.l	#7,Cycles
.lda	OLDMEM_PostIndx				;LDA ($06),Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

TAXAA_LDAB5:
.tax	move.b	AReg,XReg			;TAX
	addq.l	#6,Cycles
.lda	OLDMEM_ZPIndxX				;LDA $06,X
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

TAXAA_LDAB9:	
.tax	move.b	AReg,XReg			;TAX
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxY				;LDA $1234,Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

TAXAA_LDABD:
.tax	move.b	AReg,XReg			;TAX
	addq.l	#6,Cycles
.lda	OLDMEM_AbsIndxX				;LDA $1234,X
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

*---------*

PHA48_LDAA1:
.pha	PUSH	AReg				;PHA
	addq.l	#4,Cycles			;2 addq's faster than 1 addi.l
	addq.l	#5,Cycles
.lda	OLDMEM_PreIndx				;LDA ($06,X)
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

PHA48_LDAA5:
.pha	PUSH	AReg				;PHA
	addq.l	#6,Cycles
.lda	OLDMEM_ZP				;LDA $06
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

PHA48_LDAA9:
.pha	PUSH	AReg				;PHA
	addq.l	#5,Cycles
.lda	OLDMEM_ImmedD0				;LDA #$06
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

PHA48_LDAAD:
.pha	PUSH	AReg				;PHA
	addq.l	#7,Cycles
.lda	OLDMEM_Abs				;LDA $1234
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

PHA48_LDAB1:
.pha	PUSH	AReg				;PHA
	addq.l	#8,Cycles
.lda	OLDMEM_PostIndx				;LDA ($06),Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

PHA48_LDAB5:
.pha	PUSH	AReg				;PHA
	addq.l	#7,Cycles
.lda	OLDMEM_ZPIndxX				;LDA $06,X
	move.b	(Mem_Ptr,d0.l),AReg
	STAT_SZ
	DOCYCLE

PHA48_LDAB9:
.pha	PUSH	AReg				;PHA
	addq.l	#7,Cycles
.lda	OLDMEM_AbsIndxY				;LDA $1234,Y
	GETBYTE					;read mem into Acc...
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

PHA48_LDABD:
.pha	PUSH	AReg				;PHA
	addq.l	#7,Cycles
.lda	OLDMEM_AbsIndxX				;LDA $1234,X
	GETBYTE
	move.b	d0,AReg
	STAT_SZ
	DOCYCLE

*************************************************************
** ANY Inst + STA pairs...
*************************************************************

INXE8_STA81:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PreIndx				;STA ($06,X)
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

INXE8_STA8D:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_Abs				;STA $1234   8D
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

INXE8_STA91:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PostIndx				;STA ($06),Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

INXE8_STA99:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxY				;STA $1234,Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

INXE8_STA9D:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxX				;STA $1234,X
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

INXE8_STA85:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#5,Cycles
.sta	OLDMEM_ZP				;STA $06
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

INXE8_STA95:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_ZPIndxX				;STX $06,X
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

INYC8_STA81:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PreIndx				;STA ($06,X)
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

INYC8_STA8D:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_Abs				;STA $1234
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

INYC8_STA91:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PostIndx				;STA ($06),Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

INYC8_STA99:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxY				;STA $1234,Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

INYC8_STA9D:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxX				;STA $1234,X
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

INYC8_STA85:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#5,Cycles
.sta	OLDMEM_ZP				;STA $06
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

INYC8_STA95:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_ZPIndxX				;STX $06,X
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

DEXCA_STA81:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PreIndx				;STA ($06,X)
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

DEXCA_STA8D:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_Abs				;STA $1234
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

DEXCA_STA91:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PostIndx				;STA ($06),Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

DEXCA_STA99:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxY				;STA $1234,Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

DEXCA_STA9D:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxX				;STA $1234,X
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

DEXCA_STA85:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#5,Cycles
.sta	OLDMEM_ZP				;STA $06
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

DEXCA_STA95:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_ZPIndxX				;STX $06,X
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

DEY88_STA81:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PreIndx				;STA ($06,X)
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

DEY88_STA8D:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_Abs				;STA $1234
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

DEY88_STA91:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PostIndx				;STA ($06),Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

DEY88_STA99:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxY				;STA $1234,Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

DEY88_STA9D:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxX				;STA $1234,X
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

DEY88_STA85:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#5,Cycles
.sta	OLDMEM_ZP				;STA $06
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

DEY88_STA95:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_ZPIndxX				;STX $06,X
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

PLA68_STA81:
.pla	PULL	AReg				;PLA
	tst.b	AReg
	STAT_SZ
	addq.l	#5,Cycles			;faster than addi.l #10
	addq.l	#5,Cycles
.sta	OLDMEM_PreIndx				;STA ($06,X)
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

PLA68_STA8D:
.pla	PULL	AReg				;PLA
	tst.b	AReg
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_Abs				;STA $1234
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

PLA68_STA91:
.pla	PULL	AReg				;PLA
	tst.b	AReg
	STAT_SZ
	addq.l	#5,Cycles
	addq.l	#5,Cycles
.sta	OLDMEM_PostIndx				;STA ($06),Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

PLA68_STA99:
.pla	PULL	AReg				;PLA
	tst.b	AReg
	STAT_SZ
	addq.l	#4,Cycles
	addq.l	#5,Cycles
.sta	OLDMEM_AbsIndxY				;STA $1234,Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

PLA68_STA9D:
.pla	PULL	AReg				;PLA
	tst.b	AReg
	STAT_SZ
	addq.l	#4,Cycles
	addq.l	#5,Cycles
.sta	OLDMEM_AbsIndxX				;STA $1234,X
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

PLA68_STA85:
.pla	PULL	AReg				;PLA
	tst.b	AReg
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_ZP				;STA $06
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

PLA68_STA95:
.pla	PULL	AReg				;PLA
	tst.b	AReg
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_ZPIndxX				;STX $06,X
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

TXA8A_STA81:
.txa	move.b	XReg,AReg			;TXA
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PreIndx				;STA ($06,X)
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

TXA8A_STA8D:
.txa	move.b	XReg,AReg			;TXA
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_Abs				;STA $1234
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

TXA8A_STA91:
.txa	move.b	XReg,AReg			;TXA
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PostIndx				;STA ($06),Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

TXA8A_STA99:
.txa	move.b	XReg,AReg			;TXA
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxY				;STA $1234,Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

TXA8A_STA9D:
.txa	move.b	XReg,AReg			;TXA
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxX				;STA $1234,X
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

TXA8A_STA85:
.txa	move.b	XReg,AReg			;TXA
	STAT_SZ
	addq.l	#5,Cycles
.sta	OLDMEM_ZP				;STA $06
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

TXA8A_STA95:
.txa	move.b	XReg,AReg			;TXA
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_ZPIndxX				;STX $06,X
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

TYA98_STA81:
.tya	move.b	YReg,AReg			;TYA
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PreIndx				;STA ($06,X)
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

TYA98_STA8D:
.tya	move.b	YReg,AReg			;TYA
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_Abs				;STA $1234
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

TYA98_STA91:
.tya	move.b	YReg,AReg			;TYA
	STAT_SZ
	addq.l	#8,Cycles
.sta	OLDMEM_PostIndx				;STA ($06),Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

TYA98_STA99:
.tya	move.b	YReg,AReg			;TYA
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxY				;STA $1234,Y
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

TYA98_STA9D:
.tya	move.b	YReg,AReg			;TYA
	STAT_SZ
	addq.l	#7,Cycles
.sta	OLDMEM_AbsIndxX				;STA $1234,X
	move.b	AReg,d1
	PUTBYTE_DOCYCLE

TYA98_STA85:
.tya	move.b	YReg,AReg			;TYA
	STAT_SZ
	addq.l	#5,Cycles
.sta	OLDMEM_ZP				;STA $06
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

TYA98_STA95:
.tya	move.b	YReg,AReg			;TYA
	STAT_SZ
	addq.l	#6,Cycles
.sta	OLDMEM_ZPIndxX				;STX $06,X
	move.b	AReg,(Mem_Ptr,d0.l)
	DOCYCLE

** New opts as of 07/29/94
*************************************************************
** ANY Inst + STX pairs...
*************************************************************

DEXCA_STX8E:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#6,Cycles
.stx	OLDMEM_Abs				;STX $1234   (4 cyc)
	move.b	XReg,d1
	PUTBYTE_DOCYCLE

DEXCA_STX96:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#6,Cycles
.stx	OLDMEM_ZPIndxY				;STX $06,Y   (4 cyc)
	move.b	XReg,(Mem_Ptr,d0.l)
	DOCYCLE

DEXCA_STX86:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#5,Cycles
.stx	OLDMEM_ZP				;STX $06   (3 cyc)
	move.b	XReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

INXE8_STX8E:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#6,Cycles
.stx	OLDMEM_Abs				;STX $1234   (4 cyc)
	move.b	XReg,d1
	PUTBYTE_DOCYCLE

INXE8_STX96:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#6,Cycles
.stx	OLDMEM_ZPIndxY				;STX $06,Y   (4 cyc)
	move.b	XReg,(Mem_Ptr,d0.l)
	DOCYCLE

INXE8_STX86:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#5,Cycles
.stx	OLDMEM_ZP				;STX $06   (3 cyc)
	move.b	XReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

DEY88_STX8E:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#6,Cycles
.stx	OLDMEM_Abs				;STX $1234   (4 cyc)
	move.b	XReg,d1
	PUTBYTE_DOCYCLE

DEY88_STX96:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#6,Cycles
.stx	OLDMEM_ZPIndxY				;STX $06,Y   (4 cyc)
	move.b	XReg,(Mem_Ptr,d0.l)
	DOCYCLE

DEY88_STX86:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#5,Cycles
.stx	OLDMEM_ZP				;STX $06   (3 cyc)
	move.b	XReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

INYC8_STX8E:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#6,Cycles
.stx	OLDMEM_Abs				;STX $1234   (4 cyc)
	move.b	XReg,d1
	PUTBYTE_DOCYCLE

INYC8_STX96:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#6,Cycles
.stx	OLDMEM_ZPIndxY				;STX $06,Y   (4 cyc)
	move.b	XReg,(Mem_Ptr,d0.l)
	DOCYCLE

INYC8_STX86:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#5,Cycles
.stx	OLDMEM_ZP				;STX $06   (3 cyc)
	move.b	XReg,(Mem_Ptr,d0.l)
	DOCYCLE

*************************************************************
** ANY Inst + STY pairs...
*************************************************************

DEXCA_STY8C:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#6,Cycles
.sty	OLDMEM_Abs				;STY $1234   (4 cyc)
	move.b	YReg,d1
	PUTBYTE_DOCYCLE

DEXCA_STY94:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#6,Cycles
.sty	OLDMEM_ZPIndxX				;STY $06,X   (4 cyc)
	move.b	YReg,(Mem_Ptr,d0.l)
	DOCYCLE

DEXCA_STY84:
.dex	subq.b	#1,XReg				;DEX
	STAT_SZ
	addq.l	#5,Cycles
.sty	OLDMEM_ZP				;STY $06     (3 cyc)
	move.b	YReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

INXE8_STY8C:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#6,Cycles
.sty	OLDMEM_Abs				;STY $1234   (4 cyc)
	move.b	YReg,d1
	PUTBYTE_DOCYCLE

INXE8_STY94:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#6,Cycles
.sty	OLDMEM_ZPIndxX				;STY $06,X   (4 cyc)
	move.b	YReg,(Mem_Ptr,d0.l)
	DOCYCLE

INXE8_STY84:
.inx	addq.b	#1,XReg				;INX
	STAT_SZ
	addq.l	#5,Cycles
.sty	OLDMEM_ZP				;STY $06     (3 cyc)
	move.b	YReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

DEY88_STY8C:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#6,Cycles
.sty	OLDMEM_Abs				;STY $1234   (4 cyc)
	move.b	YReg,d1
	PUTBYTE_DOCYCLE

DEY88_STY94:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#6,Cycles
.sty	OLDMEM_ZPIndxX				;STY $06,X   (4 cyc)
	move.b	YReg,(Mem_Ptr,d0.l)
	DOCYCLE

DEY88_STY84:
.dey	subq.b	#1,YReg				;DEY
	STAT_SZ
	addq.l	#5,Cycles
.sty	OLDMEM_ZP				;STY $06     (3 cyc)
	move.b	YReg,(Mem_Ptr,d0.l)
	DOCYCLE

**************************************

INYC8_STY8C:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#6,Cycles
.sty	OLDMEM_Abs				;STY $1234   (4 cyc)
	move.b	YReg,d1
	PUTBYTE_DOCYCLE

INYC8_STY94:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#6,Cycles
.sty	OLDMEM_ZPIndxX				;STY $06,X   (4 cyc)
	move.b	YReg,(Mem_Ptr,d0.l)
	DOCYCLE

INYC8_STY84:
.iny	addq.b	#1,YReg				;INY
	STAT_SZ
	addq.l	#5,Cycles
.sty	OLDMEM_ZP				;STY $06     (3 cyc)
	move.b	YReg,(Mem_Ptr,d0.l)
	DOCYCLE

*************************************************************
** All CLC/SEC + ROL/ROR pairs...
*************************************************************
						;*** Stat dependant! ***
CLC18_ROL2E:
.clc	addq.l	#8,Cycles
.rol	OLDMEM_Abs				;ROL $1234   (6 cyc)
	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1				;needs to be there for PUTBYTE anyways...
	move.b	d0,StatusCV			;get Carry from pre-shifted hi-bit
	lsl.b	d1				;just do LSL (shift 0 in since carry clear)
	STAT_SZ					;get SZ flags
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

CLC18_ROL26:
.clc	addq.l	#7,Cycles
.rol	OLDMEM_ZP				;ROL $06   (5 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	move.b	d1,StatusCV
	lsl.b	d1
	STAT_SZ
	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE

CLC18_ROL36:
.clc	addq.l	#8,Cycles
.rol	OLDMEM_ZPIndxX				;ROL $06,X   (6 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	move.b	d1,StatusCV
	lsl.b	d1
	STAT_SZ
	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE

CLC18_ROL3E:	
.clc	addq.l	#8,Cycles
	addq.l	#1,Cycles
.rol	OLDMEM_AbsIndxX				;ROL $1234,X   (7 cyc)
	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1
	move.b	d0,StatusCV
	lsl.b	d1
	STAT_SZ
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

	CNOP	0,4
CLC18_ROL2A:	
.clc	addq.l	#4,Cycles
.rol	move.b	AReg,StatusCV				;ROL Acc   (2 cyc)
	lsl.b	AReg
	STAT_SZ
	DOCYCLE

**************************************
						;*** Stat dependant! ***
SEC38_ROL2E:
.sec	addq.l	#8,Cycles
.rol	OLDMEM_Abs				;ROL $1234   (6 cyc)
	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1
	lsl.b	d1				;do LSL and force in a 1 (due to SEC)
	move.b	d0,StatusCV			;get carry bit (pre-shifted hi bit)
	addq.b	#1,d1				;and force in that 1
	STAT_SZ
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

SEC38_ROL26:
.sec	addq.l	#7,Cycles
.rol	OLDMEM_ZP				;ROL $06   (5 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	move.b	d1,StatusCV
	lsl.b	d1
	addq.b	#1,d1
	STAT_SZ
	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE

SEC38_ROL36:
.sec	addq.l	#8,Cycles
.rol	OLDMEM_ZPIndxX				;ROL $06,X   (6 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	move.b	d1,StatusCV
	lsl.b	d1
	addq.b	#1,d1
	STAT_SZ
	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE

SEC38_ROL3E:	
.sec	addq.l	#8,Cycles
	addq.l	#1,Cycles
.rol	OLDMEM_AbsIndxX				;ROL $1234,X   (7 cyc)
	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1
	lsl.b	d1
	move.b	d0,StatusCV
	addq.b	#1,d1
	STAT_SZ
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

	CNOP	0,4
SEC38_ROL2A:	
.sec	addq.l	#8,Cycles
.rol	move.b	AReg,StatusCV			;ROL Acc   (2 cyc)
	lsl.b	AReg
	addq.b	#1,AReg
	STAT_SZ
	DOCYCLE

************************************** (ROR)

CLC18_ROR6E:
.clc	addq.l	#8,Cycles			;CLC
.ror	OLDMEM_Abs				;ROR $2134   (6 cyc)
	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1				;needs to be there for PUTBYTE anyways...
	lsr	d1				;shift in a 0 (due to CLC)
	STAT_SZC
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

CLC18_ROR66:
.clc	addq.l	#7,Cycles			;CLC
.ror	OLDMEM_ZP				;ROR $06   (5 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	lsr	d1
	STAT_SZC
	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE


CLC18_ROR76:
.clc	addq.l	#8,Cycles			;CLC
.ror	OLDMEM_ZPIndxX				;ROR $06,X   (6 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	lsr	d1
	STAT_SZC
	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE

CLC18_ROR7E:
.clc	addq.l	#8,Cycles			;CLC
	addq.l	#1,Cycles
.ror	OLDMEM_AbsIndxX				;ROR $1234,X   (7 cyc)
	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1	
	lsr	d1
	STAT_SZC
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

	CNOP	0,4
CLC18_ROR6A:
.clc	addq.l	#4,Cycles			;CLC
.ror	lsr	AReg				;ROR ACC   (2 cyc)
	STAT_SZC
	DOCYCLE

**************************************

SEC38_ROR6E:
.sec	addq.l	#8,Cycles			;SEC
.ror	OLDMEM_Abs				;ROR $2134   (6 cyc)
	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1				;needs to be there for PUTBYTE anyways...

	lsr	d1			[04]	;shift it
	STAT_C				[04]	;get carry..
	or.b	#%10000000,d1		[06]	;and insert a 1 (due to SEC)			
	STAT_SZ				[06]=20	;and get remaining flags...

	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

SEC38_ROR66:
.sec	addq.l	#7,Cycles			;SEC
.ror	OLDMEM_ZP				;ROR $06   (5 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	lsr	d1
	STAT_C
	or.b	#%10000000,d1
	STAT_SZ
	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE


SEC38_ROR76:
.sec	addq.l	#8,Cycles			;SEC
.ror	OLDMEM_ZPIndxX				;ROR $06,X   (6 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	lsr	d1
	STAT_C
	or.b	#%10000000,d1
	STAT_SZ
	move.b	d1,(Mem_Ptr,d0.l)
	DOCYCLE

SEC38_ROR7E:
.sec	addq.l	#8,Cycles			;SEC
	addq.l	#1,Cycles
.ror	OLDMEM_AbsIndxX				;ROR $1234,X   (7 cyc)
	move.w	d0,-(sp)
	GETBYTE
	move.b	d0,d1	
	lsr	d1
	STAT_C
	or.b	#%10000000,d1
	STAT_SZ
	move.w	(sp)+,d0
	PUTBYTE_DOCYCLE

	CNOP	0,4
SEC38_ROR6A:
.sec	addq.l	#4,Cycles			;SEC
.ror	lsr	AReg				;ROR ACC   (2 cyc)
	STAT_C
	or.b	#%10000000,AReg
	STAT_SZ
	DOCYCLE

*************************************************************
** ANY INST + CMP pairs...
*************************************************************

INXE8_CMPCD:
.inx	addq.b	#1,XReg				;INX
	addq.l	#6,Cycles
.cmp	OLDMEM_Abs				;CMP $1234   (4 cyc)
	GETBYTE					;read mem, subtract from ACC, set SZC
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

INXE8_CMPC1:
.inx	addq.b	#1,XReg				;INX
	addq.l	#8,Cycles
.cmp	OLDMEM_PreIndx				;CMP ($06,X)   (6 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

INXE8_CMPD1:
.inx	addq.b	#1,XReg				;INX
	addq.l	#7,Cycles
.cmp	OLDMEM_PostIndx				;CMP ($06),Y   (5 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

INXE8_CMPD9:
.inx	addq.b	#1,XReg				;INX
	addq.l	#6,Cycles
.cmp	OLDMEM_AbsIndxY				;CMP $1234,Y   (4 cyc)
   	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

INXE8_CMPD5:
.inx	addq.b	#1,XReg				;INX
	addq.l	#6,Cycles
.cmp	OLDMEM_ZPIndxX				;CMP $06,X   (4 cyc)
	cmp.b	(Mem_Ptr,d0.l),AReg
	STAT_SZiC
	DOCYCLE

INXE8_CMPC5:
.inx	addq.b	#1,XReg				;INX
	addq.l	#5,Cycles
.cmp	OLDMEM_ZP				;CMP $06   (3 cyc)
	cmp.b	(Mem_Ptr,d0.l),AReg
	STAT_SZiC
	DOCYCLE

INXE8_CMPDD:
.inx	addq.b	#1,XReg				;INX
	addq.l	#6,Cycles
.cmp	OLDMEM_AbsIndxX				;CMP $1234,X   (4 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

INXE8_CMPC9:	
.inx	addq.b	#1,XReg				;INX
	addq.l	#4,Cycles
.cmp	OLDMEM_ImmedD0				;CMP #$06   (2 cyc)
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

**************************************

DEXCA_CMPCD:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#6,Cycles
.cmp	OLDMEM_Abs				;CMP $1234   (4 cyc)
	GETBYTE					;read mem, subtract from ACC, set SZC
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

DEXCA_CMPC1:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#8,Cycles
.cmp	OLDMEM_PreIndx				;CMP ($06,X)   (6 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

DEXCA_CMPD1:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#7,Cycles
.cmp	OLDMEM_PostIndx				;CMP ($06),Y   (5 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

DEXCA_CMPD9:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#6,Cycles
.cmp	OLDMEM_AbsIndxY				;CMP $1234,Y   (4 cyc)
   	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

DEXCA_CMPD5:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#6,Cycles
.cmp	OLDMEM_ZPIndxX				;CMP $06,X   (4 cyc)
	cmp.b	(Mem_Ptr,d0.l),AReg
	STAT_SZiC
	DOCYCLE

DEXCA_CMPC5:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#5,Cycles
.cmp	OLDMEM_ZP				;CMP $06   (3 cyc)
	cmp.b	(Mem_Ptr,d0.l),AReg
	STAT_SZiC
	DOCYCLE

DEXCA_CMPDD:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#6,Cycles
.cmp	OLDMEM_AbsIndxX				;CMP $1234,X   (4 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

DEXCA_CMPC9:	
.dex	subq.b	#1,XReg				;DEX
	addq.l	#4,Cycles
.cmp	OLDMEM_ImmedD0				;CMP #$06   (2 cyc)
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

**************************************

INYC8_CMPCD:
.iny	addq.b	#1,YReg				;INY
	addq.l	#6,Cycles
.cmp	OLDMEM_Abs				;CMP $1234   (4 cyc)
	GETBYTE					;read mem, subtract from ACC, set SZC
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

INYC8_CMPC1:
.iny	addq.b	#1,YReg				;INY
	addq.l	#8,Cycles
.cmp	OLDMEM_PreIndx				;CMP ($06,X)   (6 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

INYC8_CMPD1:
.iny	addq.b	#1,YReg				;INY
	addq.l	#7,Cycles
.cmp	OLDMEM_PostIndx				;CMP ($06),Y   (5 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

INYC8_CMPD9:
.iny	addq.b	#1,YReg				;INY
	addq.l	#6,Cycles
.cmp	OLDMEM_AbsIndxY				;CMP $1234,Y   (4 cyc)
   	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

INYC8_CMPD5:
.iny	addq.b	#1,YReg				;INY
	addq.l	#6,Cycles
.cmp	OLDMEM_ZPIndxX				;CMP $06,X   (4 cyc)
	cmp.b	(Mem_Ptr,d0.l),AReg
	STAT_SZiC
	DOCYCLE

INYC8_CMPC5:
.iny	addq.b	#1,YReg				;INY
	addq.l	#5,Cycles
.cmp	OLDMEM_ZP				;CMP $06   (3 cyc)
	cmp.b	(Mem_Ptr,d0.l),AReg
	STAT_SZiC
	DOCYCLE

INYC8_CMPDD:
.iny	addq.b	#1,YReg				;INY
	addq.l	#6,Cycles
.cmp	OLDMEM_AbsIndxX				;CMP $1234,X   (4 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

INYC8_CMPC9:	
.iny	addq.b	#1,YReg				;INY
	addq.l	#4,Cycles
.cmp	OLDMEM_ImmedD0				;CMP #$06   (2 cyc)
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

**************************************

DEY88_CMPCD:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#6,Cycles
.cmp	OLDMEM_Abs				;CMP $1234   (4 cyc)
	GETBYTE					;read mem, subtract from ACC, set SZC
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

DEY88_CMPC1:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#8,Cycles
.cmp	OLDMEM_PreIndx				;CMP ($06,X)   (6 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

DEY88_CMPD1:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#7,Cycles
.cmp	OLDMEM_PostIndx				;CMP ($06),Y   (5 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

DEY88_CMPD9:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#6,Cycles
.cmp	OLDMEM_AbsIndxY				;CMP $1234,Y   (4 cyc)
   	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

DEY88_CMPD5:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#6,Cycles
.cmp	OLDMEM_ZPIndxX				;CMP $06,X   (4 cyc)
	cmp.b	(Mem_Ptr,d0.l),AReg
	STAT_SZiC
	DOCYCLE

DEY88_CMPC5:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#5,Cycles
.cmp	OLDMEM_ZP				;CMP $06   (3 cyc)
	cmp.b	(Mem_Ptr,d0.l),AReg
	STAT_SZiC
	DOCYCLE

DEY88_CMPDD:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#6,Cycles
.cmp	OLDMEM_AbsIndxX				;CMP $1234,X   (4 cyc)
	GETBYTE
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

DEY88_CMPC9:	
.dey	subq.b	#1,YReg				;DEY
	addq.l	#4,Cycles
.cmp	OLDMEM_ImmedD0				;CMP #$06   (2 cyc)
	cmp.b	d0,AReg
	STAT_SZiC
	DOCYCLE

*************************************************************
** Shifts + Bcc pairs...
*************************************************************

ASL0A_BNED0:
.asl	lsl.b	AReg				;ASL Acc
	beq.b	.NoJmp
.bne						;BNE
	STAT_SZC
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp	
	STAT_SZC
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

ASL0A_BEQF0:
.asl	lsl.b	AReg				;ASL Acc
	bne.b	.NoJmp
.beq						;BEQ
	STAT_SZC
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp	
	STAT_SZC
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

ASL0A_BCC90:
.asl	lsl.b	AReg				;ASL Acc
	STAT_SZC
	bcs.b	.NoJmp
.bcc						;BCC
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE

.NoJmp
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

ASL0A_BCSB0:
.asl	lsl.b	AReg				;ASL Acc
	STAT_SZC
	bcc.b	.NoJmp
.bcs						;BCS
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE

.NoJmp
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

**************************************

LSR4A_BNED0:
.lsr	lsr.b	AReg				;LSR Acc
	beq.b	.NoJmp
.bne						;BNE
	STAT_SZC
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp	
	STAT_SZC
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

LSR4A_BEQF0:
.lsr	lsr.b	AReg				;LSR Acc
	bne.b	.NoJmp
.beq						;BEQ
	STAT_SZC
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp	
	STAT_SZC
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

LSR4A_BCC90:
.lsr	lsr.b	AReg				;LSR Acc
	bcs.b	.NoJmp
.bcc						;BCC
	STAT_SZC
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp	
	STAT_SZC
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

LSR4A_BCSB0:
.lsr	lsr.b	AReg				;LSR Acc
	bcc.b	.NoJmp
.bcs						;BCS
	STAT_SZC
	addq.l	#5,Cycles
	move.b	(PCount)+,d1
	ext.w	d1
	add.w	d1,PCount
	DOCYCLE
.NoJmp	
	STAT_SZC
	addq.w	#1,PCount
	addq.l	#4,Cycles
	DOCYCLE

*************************************************************
** INX/DEX/INY/DEY + DEC/INC pairs...
*************************************************************

INXE8_DECCE:
.inx	addq.b	#1,XReg				;INX
	addq.l	#8,Cycles
.dec	OLDMEM_Abs				;DEC $1234
	move.b	(Mem_Ptr,d0.l),d1	;delay i/o check until write! (not perfect!!!)
	subq.b	#1,d1			;DEC memory by 1, set SZ
	STAT_SZ
	PUTBYTE				;d0 still there?
	DOCYCLE

INXE8_DECDE:
.inx	addq.b	#1,XReg				;INX
	addq.l	#8,Cycles
	addq.l	#1,Cycles
.dec	OLDMEM_AbsIndxX				;DEX $1234,X
	move.b	(Mem_Ptr,d0.l),d1
	subq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

INXE8_DECD6:
.inx	addq.b	#1,XReg				;INX
	addq.l	#8,Cycles
.dec	OLDMEM_ZPIndxX				;DEC $06,X
	subq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

INXE8_DECC6:
.inx	addq.b	#1,XReg				;INX
	addq.l	#7,Cycles
.dec	OLDMEM_ZP				;DEC $06
	subq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

**************************************

DEXCA_DECCE:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#8,Cycles
.dec	OLDMEM_Abs				;DEC $1234
	move.b	(Mem_Ptr,d0.l),d1	;delay i/o check until write! (not perfect!!!)
	subq.b	#1,d1			;DEC memory by 1, set SZ
	STAT_SZ
	PUTBYTE				;d0 still there?
	DOCYCLE

DEXCA_DECDE:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#8,Cycles
	addq.l	#1,Cycles
.dec	OLDMEM_AbsIndxX				;DEX $1234,X
	move.b	(Mem_Ptr,d0.l),d1
	subq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

DEXCA_DECD6:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#8,Cycles
.dec	OLDMEM_ZPIndxX				;DEC $06,X
	subq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

DEXCA_DECC6:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#7,Cycles
.dec	OLDMEM_ZP				;DEC $06
	subq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE



**************************************

INYC8_DECCE:
.iny	addq.b	#1,YReg				;INY
	addq.l	#8,Cycles
.dec	OLDMEM_Abs				;DEC $1234
	move.b	(Mem_Ptr,d0.l),d1	;delay i/o check until write! (not perfect!!!)
	subq.b	#1,d1			;DEC memory by 1, set SZ
	STAT_SZ
	PUTBYTE				;d0 still there?
	DOCYCLE

INYC8_DECDE:
.iny	addq.b	#1,YReg				;INY
	addq.l	#8,Cycles
	addq.l	#1,Cycles
.dec	OLDMEM_AbsIndxX				;DEX $1234,X
	move.b	(Mem_Ptr,d0.l),d1
	subq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

INYC8_DECD6:
.iny	addq.b	#1,YReg				;INY
	addq.l	#8,Cycles
.dec	OLDMEM_ZPIndxX				;DEC $06,X
	subq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

INYC8_DECC6:
.iny	addq.b	#1,YReg				;INY
	addq.l	#7,Cycles
.dec	OLDMEM_ZP				;DEC $06
	subq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE



**************************************

DEY88_DECCE:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#8,Cycles
.dec	OLDMEM_Abs				;DEC $1234
	move.b	(Mem_Ptr,d0.l),d1	;delay i/o check until write! (not perfect!!!)
	subq.b	#1,d1			;DEC memory by 1, set SZ
	STAT_SZ
	PUTBYTE				;d0 still there?
	DOCYCLE

DEY88_DECDE:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#8,Cycles
	addq.l	#1,Cycles
.dec	OLDMEM_AbsIndxX				;DEX $1234,X
	move.b	(Mem_Ptr,d0.l),d1
	subq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

DEY88_DECD6:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#8,Cycles
.dec	OLDMEM_ZPIndxX				;DEC $06,X
	subq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

DEY88_DECC6:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#7,Cycles
.dec	OLDMEM_ZP				;DEC $06
	subq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE



**************************************

INXE8_INCEE:
.inx	addq.b	#1,XReg				;INX
	addq.l	#8,Cycles
.inc	OLDMEM_Abs				;INC $1234   (6 cyc)
	move.b	(Mem_Ptr,d0.l),d1		;(not perfect!)
	addq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

INXE8_INCFE:
.inx	addq.b	#1,XReg				;INX
	addq.l	#8,Cycles
	addq.l	#1,Cycles
.inc	OLDMEM_AbsIndxX				;INC $1234,X   (7 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	addq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

INXE8_INCE6:
.inx	addq.b	#1,XReg				;INX
	addq.l	#7,Cycles
.inc	OLDMEM_ZP				;INC $06   (5 cyc)
	addq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

INXE8_INCF6:
.inx	addq.b	#1,XReg				;INX
	addq.l	#8,Cycles
.inc	OLDMEM_ZPIndxX				;INC $06,X   (6 cyc)
	addq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

**************************************

DEXCA_INCEE:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#8,Cycles
.inc	OLDMEM_Abs				;INC $1234   (6 cyc)
	move.b	(Mem_Ptr,d0.l),d1		;(not perfect!)
	addq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

DEXCA_INCFE:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#8,Cycles
	addq.l	#1,Cycles
.inc	OLDMEM_AbsIndxX				;INC $1234,X   (7 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	addq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

DEXCA_INCE6:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#7,Cycles
.inc	OLDMEM_ZP				;INC $06   (5 cyc)
	addq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

DEXCA_INCF6:
.dex	subq.b	#1,XReg				;DEX
	addq.l	#8,Cycles
.inc	OLDMEM_ZPIndxX				;INC $06,X   (6 cyc)
	addq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

**************************************

INYC8_INCEE:
.iny	addq.b	#1,YReg				;INY
	addq.l	#8,Cycles
.inc	OLDMEM_Abs				;INC $1234   (6 cyc)
	move.b	(Mem_Ptr,d0.l),d1		;(not perfect!)
	addq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

INYC8_INCFE:
.iny	addq.b	#1,YReg				;INY
	addq.l	#8,Cycles
	addq.l	#1,Cycles
.inc	OLDMEM_AbsIndxX				;INC $1234,X   (7 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	addq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

INYC8_INCE6:
.iny	addq.b	#1,YReg				;INY
	addq.l	#7,Cycles
.inc	OLDMEM_ZP				;INC $06   (5 cyc)
	addq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

INYC8_INCF6:
.iny	addq.b	#1,YReg				;INY
	addq.l	#8,Cycles
.inc	OLDMEM_ZPIndxX				;INC $06,X   (6 cyc)
	addq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

**************************************

DEY88_INCEE:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#8,Cycles
.inc	OLDMEM_Abs				;INC $1234   (6 cyc)
	move.b	(Mem_Ptr,d0.l),d1		;(not perfect!)
	addq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

DEY88_INCFE:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#8,Cycles
	addq.l	#1,Cycles
.inc	OLDMEM_AbsIndxX				;INC $1234,X   (7 cyc)
	move.b	(Mem_Ptr,d0.l),d1
	addq.b	#1,d1
	STAT_SZ
	PUTBYTE
	DOCYCLE

DEY88_INCE6:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#7,Cycles
.inc	OLDMEM_ZP				;INC $06   (5 cyc)
	addq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

DEY88_INCF6:
.dey	subq.b	#1,YReg				;DEY
	addq.l	#8,Cycles
.inc	OLDMEM_ZPIndxX				;INC $06,X   (6 cyc)
	addq.b	#1,(Mem_Ptr,d0.l)
	STAT_SZ
	DOCYCLE

**************************************

	SECTION TABLES,DATA
	CNOP	0,4
InstPairList:
			;--- Misc simple inst pairs ---
	dc.l	$e8e8,INXE8_INXE8,$c8c8,INYC8_INYC8,$caca,DEXCA_DEXCA,$8888,DEY88_DEY88 ;OK!
	dc.l	$0a0a,ASL0A_ASL0A,$4a4a,LSR4A_LSR4A,$6868,PLA68_PLA68					;ok

	dc.l	$8a48,TXA8A_PHA48,$9848,TYA98_PHA48					;ok
	dc.l	$6848,PLA68_PHA48,$68a8,PLA68_TAYA8,$68aa,PLA68_TAXAA			;ok

			;--- And special case JMP (xxFF) bug ---
	dc.l	$6cff,JMP6C_BUGFF

			;--- CLC + ADC Pairs ---
	dc.l	$1861,CLC18_ADC61,$186d,CLC18_ADC6D,$1865,CLC18_ADC65,$1869,CLC18_ADC69	;ok
	dc.l	$1871,CLC18_ADC71,$187d,CLC18_ADC7D,$1875,CLC18_ADC75,$1879,CLC18_ADC79	;ok

			;--- SEC + SBC Pairs ---
	dc.l	$38e5,SEC38_SBCE5,$38e1,SEC38_SBCE1,$38ed,SEC38_SBCED,$38f1,SEC38_SBCF1	;ok
	dc.l	$38f9,SEC38_SBCF9,$38fd,SEC38_SBCFD,$38e9,SEC38_SBCE9,$38f5,SEC38_SBCF5

			;--- DEX/INX + CPX Pairs ---
	dc.l	$caec,DEXCA_CPXEC,$cae4,DEXCA_CPXE4,$cae0,DEXCA_CPXE0
	dc.l	$e8ec,INXE8_CPXEC,$e8e4,INXE8_CPXE4,$e8e0,INXE8_CPXE0

			;--- DEY/INY + CPY Pairs ---
	dc.l	$88cc,DEY88_CPYCC,$88c4,DEY88_CPYC4,$88c0,DEY88_CPYC0
	dc.l	$c8cc,INYC8_CPYCC,$c8c4,INYC8_CPYC4,$c8c0,INYC8_CPYC0

			;--- AnyInst + LDA Pairs ---
	dc.l	$e8a1,INXE8_LDAA1,$e8a5,INXE8_LDAA5,$e8a9,INXE8_LDAA9,$e8ad,INXE8_LDAAD
	dc.l	$e8b1,INXE8_LDAB1,$e8b5,INXE8_LDAB5,$e8b9,INXE8_LDAB9,$e8bd,INXE8_LDABD
	dc.l	$caa1,DEXCA_LDAA1,$caa5,DEXCA_LDAA5,$caa9,DEXCA_LDAA9,$caad,DEXCA_LDAAD
	dc.l	$cab1,DEXCA_LDAB1,$cab5,DEXCA_LDAB5,$cab9,DEXCA_LDAB9,$cabd,DEXCA_LDABD

	dc.l	$c8a1,INYC8_LDAA1,$c8a5,INYC8_LDAA5,$c8a9,INYC8_LDAA9,$c8ad,INYC8_LDAAD
	dc.l	$c8b1,INYC8_LDAB1,$c8b5,INYC8_LDAB5,$c8b9,INYC8_LDAB9,$c8bd,INYC8_LDABD
	dc.l	$88a1,DEY88_LDAA1,$88a5,DEY88_LDAA5,$88a9,DEY88_LDAA9,$88ad,DEY88_LDAAD
	dc.l	$88b1,DEY88_LDAB1,$88b5,DEY88_LDAB5,$88b9,DEY88_LDAB9,$88bd,DEY88_LDABD

	dc.l	$a8a1,TAYA8_LDAA1,$a8a5,TAYA8_LDAA5,$a8a9,TAYA8_LDAA9,$a8ad,TAYA8_LDAAD
	dc.l	$a8b1,TAYA8_LDAB1,$a8b5,TAYA8_LDAB5,$a8b9,TAYA8_LDAB9,$a8bd,TAYA8_LDABD
	dc.l	$aaa1,TAXAA_LDAA1,$aaa5,TAXAA_LDAA5,$aaa9,TAXAA_LDAA9,$aaad,TAXAA_LDAAD
	dc.l	$aab1,TAXAA_LDAB1,$aab5,TAXAA_LDAB5,$aab9,TAXAA_LDAB9,$aabd,TAXAA_LDABD

	dc.l	$48a1,PHA48_LDAA1,$48a5,PHA48_LDAA5,$48a9,PHA48_LDAA9,$48ad,PHA48_LDAAD
	dc.l	$48b1,PHA48_LDAB1,$48b5,PHA48_LDAB5,$48b9,PHA48_LDAB9,$48bd,PHA48_LDABD

			;---AnyInst + STA Pairs---
	dc.l	$e881,INXE8_STA81,$e88d,INXE8_STA8D,$e891,INXE8_STA91,$e899,INXE8_STA99
	dc.l	$e89d,INXE8_STA9D,$e885,INXE8_STA85,$e895,INXE8_STA95
	dc.l	$c881,INYC8_STA81,$c88d,INYC8_STA8D,$c891,INYC8_STA91,$c899,INYC8_STA99
	dc.l	$c89d,INYC8_STA9D,$c885,INYC8_STA85,$c895,INYC8_STA95

	dc.l	$ca81,DEXCA_STA81,$ca8d,DEXCA_STA8D,$ca91,DEXCA_STA91,$ca99,DEXCA_STA99
	dc.l	$ca9d,DEXCA_STA9D,$ca85,DEXCA_STA85,$ca95,DEXCA_STA95
	dc.l	$8881,DEY88_STA81,$888d,DEY88_STA8D,$8891,DEY88_STA91,$8899,DEY88_STA99
	dc.l	$889d,DEY88_STA9D,$8885,DEY88_STA85,$8895,DEY88_STA95

	dc.l	$6881,PLA68_STA81,$688d,PLA68_STA8D,$6891,PLA68_STA91,$6899,PLA68_STA99
	dc.l	$689d,PLA68_STA9D,$6885,PLA68_STA85,$6895,PLA68_STA95

	dc.l	$8a81,TXA8A_STA81,$8a8d,TXA8A_STA8D,$8a91,TXA8A_STA91,$8a99,TXA8A_STA99
	dc.l	$8a9d,TXA8A_STA9D,$8a85,TXA8A_STA85,$8a95,TXA8A_STA95

	dc.l	$9881,TYA98_STA81,$988d,TYA98_STA8D,$9891,TYA98_STA91,$9899,TYA98_STA99
	dc.l	$989d,TYA98_STA9D,$9885,TYA98_STA85,$9895,TYA98_STA95

			;---AnyInst + STX Pairs---  (new 7/29/94)
	dc.l	$ca8e,DEXCA_STX8E,$ca96,DEXCA_STX96,$ca86,DEXCA_STX86
	dc.l	$e88e,INXE8_STX8E,$e896,INXE8_STX96,$e886,INXE8_STX86
	dc.l	$888e,DEY88_STX8E,$8896,DEY88_STX96,$8886,DEY88_STX86
	dc.l	$c88e,INYC8_STX8E,$c896,INYC8_STX96,$c886,INYC8_STX86

			;---AnyInst + STY Pairs---  (new 7/29/94)
	dc.l	$ca8c,DEXCA_STY8C,$ca94,DEXCA_STY94,$ca84,DEXCA_STY84
	dc.l	$e88c,INXE8_STY8C,$e894,INXE8_STY94,$e884,INXE8_STY84
	dc.l	$888c,DEY88_STY8C,$8894,DEY88_STY94,$8884,DEY88_STY84
	dc.l	$c88c,INYC8_STY8C,$c894,INYC8_STY94,$c884,INYC8_STY84

			;---CLC/SEC + ROL Pairs---  (new 7/30/94)
	dc.l	$182e,CLC18_ROL2E,$1826,CLC18_ROL26,$1836,CLC18_ROL36
	dc.l	$183e,CLC18_ROL3E,$182a,CLC18_ROL2A	
	dc.l	$382e,SEC38_ROL2E,$3826,SEC38_ROL26,$3836,SEC38_ROL36
	dc.l	$383e,SEC38_ROL3E,$382a,SEC38_ROL2A	

			;---CLC/SEC + ROR Pairs---  (new 7/30/94)
	dc.l	$186e,CLC18_ROR6E,$1866,CLC18_ROR66,$1876,CLC18_ROR76
	dc.l	$187e,CLC18_ROR7E,$186a,CLC18_ROR6A
	dc.l	$386e,SEC38_ROR6E,$3866,SEC38_ROR66,$3876,SEC38_ROR76
	dc.l	$387e,SEC38_ROR7E,$386a,SEC38_ROR6A

			;---AnyInst + CMP Pairs---  (new 7/30/94)
	dc.l	$e8cd,INXE8_CMPCD,$e8c1,INXE8_CMPC1,$e8d1,INXE8_CMPD1,$e8d9,INXE8_CMPD9
	dc.l	$e8d5,INXE8_CMPD5,$e8c5,INXE8_CMPC5,$e8dd,INXE8_CMPDD,$e8c9,INXE8_CMPC9	
	dc.l	$cacd,DEXCA_CMPCD,$cac1,DEXCA_CMPC1,$cad1,DEXCA_CMPD1,$cad9,DEXCA_CMPD9
	dc.l	$cad5,DEXCA_CMPD5,$cac5,DEXCA_CMPC5,$cadd,DEXCA_CMPDD,$cac9,DEXCA_CMPC9	

	dc.l	$c8cd,INYC8_CMPCD,$c8c1,INYC8_CMPC1,$c8d1,INYC8_CMPD1,$c8d9,INYC8_CMPD9
	dc.l	$c8d5,INYC8_CMPD5,$c8c5,INYC8_CMPC5,$c8dd,INYC8_CMPDD,$c8c9,INYC8_CMPC9	
	dc.l	$88cd,DEY88_CMPCD,$88c1,DEY88_CMPC1,$88d1,DEY88_CMPD1,$88d9,DEY88_CMPD9
	dc.l	$88d5,DEY88_CMPD5,$88c5,DEY88_CMPC5,$88dd,DEY88_CMPDD,$88c9,DEY88_CMPC9	


			;--- Shifts + Bcc Pairs ---
	dc.l	$0ad0,ASL0A_BNED0,$0af0,ASL0A_BEQF0	;$0a90,ASL0A_BCC90,$0ab0,ASL0A_BCSB0
	dc.l	$4ad0,LSR4A_BNED0,$4af0,LSR4A_BEQF0	;$4a90,LSR4A_BCC90,$4ab0,LSR4A_BCSB0

			;--- INX/DEX/INY/DEY + DEC/INC Pairs ---
	dc.l	$e8ce,INXE8_DECCE,$e8de,INXE8_DECDE,$e8d6,INXE8_DECD6,$e8c6,INXE8_DECC6
	dc.l	$cace,DEXCA_DECCE,$cade,DEXCA_DECDE,$cad6,DEXCA_DECD6,$cac6,DEXCA_DECC6
	dc.l	$c8ce,INYC8_DECCE,$c8de,INYC8_DECDE,$c8d6,INYC8_DECD6,$c8c6,INYC8_DECC6
	dc.l	$88ce,DEY88_DECCE,$88de,DEY88_DECDE,$88d6,DEY88_DECD6,$88c6,DEY88_DECC6

	dc.l	$e8ee,INXE8_INCEE,$e8fe,INXE8_INCFE,$e8e6,INXE8_INCE6,$e8f6,INXE8_INCF6
	dc.l	$caee,DEXCA_INCEE,$cafe,DEXCA_INCFE,$cae6,DEXCA_INCE6,$caf6,DEXCA_INCF6
	dc.l	$c8ee,INYC8_INCEE,$c8fe,INYC8_INCFE,$c8e6,INYC8_INCE6,$c8f6,INYC8_INCF6
	dc.l	$88ee,DEY88_INCEE,$88fe,DEY88_INCFE,$88e6,DEY88_INCE6,$88f6,DEY88_INCF6


	dc.l	$e901,SBCE9_01
	dc.l	$cad0,DEXCA_BNED0,$88d0,DEY88_BNED0
	dc.l	$e8d0,INXE8_BNED0,$c8d0,INYC8_BNED0

InstPairEarlyEnd:
	dc.l	$0000,$0000				;PUT $0001,BRK00 to go past here...!

		;--- Inc/Dec + Bcc Pairs ---  (** Unused during speed regulation! **)
	dc.l	$caf0,DEXCA_BEQF0,$ca10,DEXCA_BPL10,$ca30,DEXCA_BMI30
	dc.l	$88f0,DEY88_BEQF0,$8810,DEY88_BPL10,$8830,DEY88_BMI30
	dc.l	$e8f0,INXE8_BEQF0,$e810,INXE8_BPL10,$e830,INXE8_BMI30
	dc.l	$c8f0,INYC8_BEQF0,$c810,INYC8_BPL10,$c830,INYC8_BMI30


	dc.l	0,0,0,0
	dc.l	0,0,0,0


*-------------------------------------------------------------------*
	SECTION	APPLEII,CODE
	CNOP	0,4
STOP_INST:				;6502 Inst to stop processing, (Parent WAITING for sig)
	clr.l	ResumePCount		;set to 0 incase I should resume elsewhere...
	clr.l	RebootApple			;""
	clr.l	ResumeWithDiagnostic		;""	
	clr.l	ResumeWithNewSpeed

	subq.w	#2,PCount

	move.l	ParentTaskPtr,a1
	move.l	ParentSigMask,d0
	CALLEXEC Signal				;Tell parent "Okay, I'm Stopped..."

	move.l	ChildSigMask,d0			;"When can I continue???"
	CALLEXEC Wait

	move.l	ResumeWithNewSpeed,d0
	beq	.SameSpeed
	bmi.b	.NoRegulation

.Regulate
	mulu.l	RealCycPerFrame,d0		;WaitCycles= RealCycles*Percentage/100
	divu.l	#100,d0				;32/32->32q
	move.l	d0,WaitCycPerFrame
	move.l	Cycles,LastStopCycle

	clr.l	InstPairEarlyEnd		;patch out opt'd INC/DEC+BNE/BEQ/BPL pairs!
	clr.l	InstPairEarlyEnd+4

	move.l	#BNED0Reg,InstJmpTblBkUp+$50*4	;and use regulated BNE/BEQ/BPL/BMI/JMP
	move.l	#BEQF0Reg,InstJmpTblBkUp+$70*4
	move.l	#BPL10Reg,InstJmpTblBkUp+$90*4
	move.l	#BMI30Reg,InstJmpTblBkUp+$b0*4
	move.l	#JMP4CReg,InstJmpTblBkUp+$cc*4
	bra.b	.DoneFixingSpeed

.NoRegulation
	move.l	d0,WaitCycPerFrame		;FIX THIS LATER!!!
	move.l	#$0001,InstPairEarlyEnd		;patch back in opt'd INC/DEC+Bxx pairs!
	move.l	#BRK00,InstPairEarlyEnd+4

	move.l	#BNED0,InstJmpTblBkUp+$50*4	;and fast BNE/BEQ/BPL/BMI/JMP
	move.l	#BEQF0,InstJmpTblBkUp+$70*4
	move.l	#BPL10,InstJmpTblBkUp+$90*4
	move.l	#BMI30,InstJmpTblBkUp+$b0*4
	move.l	#JMP4C,InstJmpTblBkUp+$cc*4

.DoneFixingSpeed:
.SameSpeed:
	jsr	RestoreTable
	move.l	ParentTaskPtr,a1
	move.l	ParentSigMask,d0
	CALLEXEC Signal				;Signal parent "Okay, Table restored & Im going..."

	tst.l	RebootApple			;reboot flag set?
	bne	.CauseReboot			;yep, go force a reboot

	tst.l	ResumeWithDiagnostic		;Diagnostic Message wanted?
	beq	.noDiag				;no...

.diag	lea	.Diag1,a0			;PCount
	jsr	DB_String	
	move.l	PCount,d0
	sub.l	Mem_Ptr,d0
	jsr	DB_HexL
	lea	.Diag2,a0			;Acc
	jsr	DB_String
	move.w	AReg,d0
	jsr	DB_HexW
	lea	.Diag3,a0			;XReg
	jsr	DB_String
	move.w	XReg,d0
	jsr	DB_HexW
	lea	.Diag4,a0
	jsr	DB_String			;YReg
	move.w	YReg,d0
	jsr	DB_HexW
	lea	.Diag5,a0			;Stack Loc
	jsr	DB_String
	move.l	Stack,d0
	swap	d0
	jsr	DB_HexW
	lea	.Diag6,a0			;Stack Frame
	jsr	DB_String
	move.l	Stack,d1
	swap	d1
	move.l	1(Mem_Ptr,d1.w),d0
	jsr	DB_HexL
	move.l	5(Mem_Ptr,d1.w),d0
	jsr	DB_HexL
	lea	.Diag7,a0	
	jsr	DB_String			;Cycles
	move.l	Cycles,d0
	jsr	DB_HexL
	
.noDiag	tst.l	ResumePCount			;new PCount set? patch it in!
	beq	.samePC
					;Executable Prog! Set Apple to "freshly booted" status
	move.b	#%00000001,VidMode		;text page 1 (reset Video!) 
	jsr	RefreshVideo

	move.l	#$c082,d0			;reset 16k card on reset!
	GETBYTE					;(read rom, write prot ram)
	move.l	ResumePCount,PCount		;set PCount
	add.l	Mem_Ptr,PCount
	move.l	#$01f00000,Stack		;set stack near top of frame... (top word)

.samePC	jsr	RefreshVideo			;get screens all proper! (does ChkVid too)
	CLEAR	d0
	DOCYCLE

.CauseReboot:
	move.w	#00,([Mem_PtrVar.l],$3f2.w)	;blank out soft reset vector...
	move.b	#00,([Mem_PtrVar.l],$3f4.w)	;and checksum byte... WILL REBOOT!
	bsr	ChkVid
	bra	RESET_INSTa			;just go to there myself!

.Diag1	dc.b	10,13,"PC:",0
.Diag2	dc.b	" A:",0
.Diag3	dc.b	" X:",0
.Diag4	dc.b	" Y:",0
.Diag5	dc.b	" StackPtr:",0
.Diag6	dc.b	" Frame:",0
.Diag7	dc.b	" Cycles:",0
	CNOP	0,4

ResumePCount	dc.l	0		;<-- LONG!!! Set 16 bit PC when paused to resume at
RebootApple	dc.l	0		;<-- Boolean! Set TRUE to reboot when continued...
ResumeWithDiagnostic	dc.l	0	;<-- Boolean! Set TRUE to print Diagnostic when continued...
ResumeWithNewSpeed	dc.l	0	;<-- Set to % of Apple speed (or -1 for no regulation!)

RESET_INST:					;6502 Inst to do a hard-reset (No signals)
	jsr	RestoreTable			;restore 256k jump table...
RESET_INSTa:	
	move.w	#64,Hardware+aud0+ac_vol.l	;Set Channel 0 Volume to Max
	move.w	#1,Hardware+aud0+ac_per.l	;Channel 0 Period

	move.w	#64,Hardware+aud1+ac_vol.l	;Set Channel 1 Volume to Max
	move.w	#1,Hardware+aud1+ac_per.l	;Channel 1 Period

	move.l	#$c081,d0			;reset 16k card on reset!
	GETBYTE
;floob	move.l	#$c081,d0
;	GETBYTE

	move.l	#0,d0
	move.l	#$fffc,a0			; RESET vector!
	FGETWORD				;(doest change hi D0)
	add.l	Mem_Ptr,d0
	move.l	d0,PCount
	move.l	#$01ff0000,Stack		;reset it to top of frame! (top word)
	bset.l	#I_BIT,StatusSZ
	CLEAR	d0
	DOCYCLE

RestoreTable:					;*** Restore 256k jump table... ***
	move.l	d2,-(sp)
	CALLEXEC Forbid
	move.l	#InstJmpTblBkUp,a0		;array of 256 longs...
	move.l	InstTbl_Var,a1			;array of 65536 longs...
	move.w	#255,d1
.loop	move.w	#255,d0
	move.l	(a0)+,d2
.lp	move.l	d2,(a1)+
	dbf	d0,.lp
	dbf	d1,.loop
;	bra	.done			;<<<-------- temporary
	
	move.l	#InstPairList,a0
	move.l	InstTbl_Var,a1
	add.l	#$20000,a1
2$	move.l	(a0)+,d0
	beq.b	.done
	move.l	(a0)+,(a1,d0.w*4)
	bra.b	2$
.done	CALLEXEC Permit
	move.l	(sp)+,d2
	rts	

	CNOP	0,4
GBHardWare:		;enter with d0=address to read, clean!
	move.l	#HardwareReadTbl-$c000*4,a0
	move.l	(a0,d0.l*4),d1
	beq	.noJmp
	move.l	d1,a0
	jmp	(a0)
.noJmp	move.b	(Mem_Ptr,d0.l),d0	;read anything else (for $c000!)
	rts

	SECTION	TABLES,DATA
	CNOP	0,4
HardwareReadTbl:	;$c000 - $c0ff range...
	dc.l	$00,HWr_Kybd,HWr_Kybd,HWr_Kybd,HWr_Kybd,HWr_Kybd,HWr_Kybd,HWr_Kybd	;$00-07
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00						;$08-0f
	dc.l	HW_C010,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$10-1f
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,HWr_C028,HWr_C029,$0,$0,$0,$0,$0,$00	;$20-2f
	dc.l	HW_C030,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$30-3f
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;$40-4f
	dc.l	HW_C050,HW_C051,HW_C052,HW_C053,HW_C054,HW_C055,HW_C056,HW_C057		;$50-57
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00						;58-5f
	dc.l	$00,HWr_C061,HWr_C062,$00,HWr_C064,HWr_C065,$00,$00			;60-67
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00						;68-6f
	dc.l	HW_C070_Joystick,HW_C071,$00,$00,$00,$00,$00,$00			;70-77
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00						;78-7f
	dc.l	HW_C080,HW_C081,HW_C082,HW_C083,HW_C080,HW_C081,HW_C082,HW_C083		;80-87
	dc.l	HW_C088,HW_C089,HW_C082,HW_C08B,HW_C088,HW_C089,HW_C082,HW_C08B		;88-8f

	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;a0-af
;	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,HWr_C099,$00,$00,$00,$00,$00,$00		;90-9f
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;a0-af
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;b0-bf
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;c0-cf
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;d0-df
	dc.l	$00,HW_C0E1,$00,HW_C0E3,$00,HW_C0E5,$00,HW_C0E7				;e0-e7
	dc.l	HW_C0E8,HW_C0E9,HW_C0EA,HW_C0EB,HW_C0EC,HWr_C0ED,HW_C0EE,HW_C0EF	;e8-ef
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,HWr_Dgn	;f0-ff
	
	SECTION	APPLEII,CODE

	CNOP	0,4
PBSlot:	rts		;dont write to slot addresses ($c100 - $cfff)

	include	"TextMap.s"	;has PBTxt1/PBTxt2 routine!

	include "HiResMap.s"	;has PBHgr1 routine!

	CNOP	0,4
PBHardWare:		;Called from PutByte. A0=Address, D1=Byte to write.
	move.l	#HardwareWriteTbl-$c000*4,a1
	move.l	(a1,a0.l*4),d0
	beq	.noJmp
	move.l	d0,a1
	jmp	(a1)
.noJmp	;move.b	d1,(Mem_Ptr,a0.l)
	rts

	CNOP	0,4


	SECTION	TABLES,DATA
	CNOP	0,4
HardwareWriteTbl:
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;$00-0f
	dc.l	HW_C010,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$10-1f
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,HWw_C028,$00,$00,$00,$00,$00,$00,$00	;$20-2f
	dc.l	HW_C030,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$30-3f
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;$40-4f
	dc.l	HW_C050,HW_C051,HW_C052,HW_C053,HW_C054,HW_C055,HW_C056,HW_C057		;$50-57
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00						;58-5f
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00		;60-67 (c060=CasseteIn)
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00		;$c068 - $c06f
	dc.l	HW_C070_Joystick,HW_C071,$0,$0,$0,$0,$0,$0				;70-77
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00						;78-7f
	dc.l	HW_C080,HW_C081,HW_C082,HW_C083,HW_C080,HW_C081,HW_C082,HW_C083		;80-87
	dc.l	HW_C088,HW_C089,HW_C082,HW_C08B,HW_C088,HW_C089,HW_C082,HW_C08B		;88-8f
;	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,HWw_C09A,$00,$00,$00,$00,$00	;90-9f
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;90-9f
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;a0-af
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;b0-bf
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;c0-cf
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00		;d0-df
	dc.l	$00,HW_C0E1,$00,HW_C0E3,$00,HW_C0E5,$00,HW_C0E7				;e0-e7
	dc.l	HW_C0E8,HW_C0E9,HW_C0EA,HW_C0EB,HW_C0EC,HWw_C0ED,HW_C0EE,HW_C0EF	;e8-ef
	dc.l	HW_C0F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,HWw_Dgn	;f0-ff
	
	SECTION	APPLEII,CODE
	CNOP	0,4
******** HardWare Accesses *********
	;HW_$$$$ subroutines are intended to handle "touch" access
	;(not READ or WRITE). Input: D1 = Data to write (if any)
	;HWR_$$$$ are for Hardware Reads (d0 = result)

HWr_Dgn:		;diagnostic, print msg & hardware addr attempting to read...
	lea	.msg,a0
	jsr	DB_String
	jsr	DB_HexW
	rts
.msg	dc.b	10,"Hardware read of: ",0

	CNOP	0,4
HWw_Dgn:		;diagnostic, print msg & hardware addr attempting to read...
	move.l	a0,d0
	lea	.msg,a0
	jsr	DB_String
	jsr	DB_HexW
	rts
.msg	dc.b	10,"Hardware write of: ",0

	CNOP	0,4

HWr_C099:		;read from ser port?
	move.l	#$c099,a0
	move.b	(Mem_Ptr,a0.l),d0
;	move.b	#0,(Mem_Ptr,a0.l)
	rts

HWw_C09A:		;write to ser port?
	move.l	#$c09a,a0
	move.b	d1,.msg
	lea	.msg,a0
	jsr	DB_String
	rts
.msg	dc.b	0,0	

	CNOP	0,4
HWr_Kybd:					;for $c001 - $c007 (frogger uses $c006)
	move.l	#$c000,a0
	move.b	(Mem_Ptr,a0.l),d0
	rts

	CNOP	0,4
HW_C010	move.l	#$c000,a0	;Kybrd strobe, Clear keyboard
	and.b	#$7f,(Mem_Ptr,a0.l)
	rts

;
; HWw_C028 --- My custom hardware interface, located in disk subsystem code....

	CNOP	0,4
HW_C030
	move.w	#INTF_AUD0!INTF_AUD1,Hardware+intreq.l	;Chnl 0 int req clr, Audio Machine can take data
	eor.w	#$ffff,.SndDat
	move.w	.SndDat,Hardware+aud0+ac_dat.l	;Channel 0 Data!
	move.w	.SndDat,Hardware+aud1+ac_dat.l	;Channel 1 data!
	rts
.SndDat dc.w	$7f7f



	CNOP	0,4
HWr_C061 move.b	$bfe001.l,d0		;reading Button #0 status... (Fix to CIAAPRA laters!)
	bpl	.press
.noBut	move.l	#$c061,a0		;No joy button, return contents of memory
	move.b	(Mem_Ptr,a0.l),d0	; location (cause ALT keys set it..)
	rts
.press	move.b	#$ff,d0			;Joy Button pressed! Return as such!!
	rts
	CNOP	0,4

HWr_C061_Analog:

	move.w	Hardware+joy1dat.l,d0

	btst.l	#9,d0			;test joystick button...
	sne.b	d0
	move.l	#$c061,a0		;& allow ALT override...
	or.b	(Mem_Ptr,a0.l),d0
	rts

HWr_C062 move.w	$dff016.l,d0		;read button #1 status... (Fix to POTINP laters!)

	btst.l	#14,d0
	beq	.press
.noBut	move.l	#$c062,a0		;no joy, return contents of memory
	move.b	(Mem_Ptr,a0.l),d0	;location (cause ALT keys set it...)
	rts
.press	move.b	#$ff,d0
	rts

HWr_C062_Analog:
	move.w	Hardware+joy1dat.l,d0

	btst.l	#1,d0			;test joystick button...
	sne.b	d0
	move.l	#$c062,a0		;& allow ALT override...
	or.b	(Mem_Ptr,a0.l),d0
	rts

	CNOP	0,4
HWr_C064 move.l	Cycles,d0		;PDL 0 Countdown timer.. (goes + when done)
	sub.l	PdlOrigCyc,d0	;get elapsed cycles...
	cmp.l	Pdl0Wait,d0
	blo.s	.NotYet
.Yes	move.b	#00,d0
	rts
.NotYet	move.b	#$80,d0
	rts
	
	CNOP	0,4
HWr_C065 move.l	Cycles,d0		;PDL 1 Countdown timer.. (goes + when done)
	sub.l	PdlOrigCyc,d0	;get elapsed cycles...
	cmp.l	Pdl1Wait,d0
	blo.s	.NotYet
.Yes	move.b	#00,d0
	rts
.NotYet	move.b	#$80,d0
	rts
	CNOP	0,4

HW_C070_Joystick:				;*** Strobe PDLs, start timers (Joystick) ***
	move.w	Hardware+joy1dat.l,d0

	moveq.l	#8,d1				;If left, use 8 cycles...
	btst.l	#9,d0
	bne.b	.CkUpDn

	move.w	#2940,d1			;If right, use use 2940 cycles...
	btst.l	#1,d0				;(2940 is min for champ Lode Runner)
	bne.b	.CkUpDn

	move.w	Pdl0CenterW,d1			;else centered, use variable center value
	
.CkUpDn	move.l	d1,Pdl0Wait
						;pdl routines have 11 cycles min resolution
	move.w	d0,d1				;(hardware already read above)
	lsl.w	d1
	eor.w	d1,d0				;XOR bit #0 onto bit #1, bit #8 onto bit #9

	moveq.l	#8,d1				;if up, use 8 cycles...
	btst.l	#9,d0
	bne.b	.done

	move.w	#2940,d1			;if down, use 2940 cycles...
	btst.l	#1,d0
	bne.b	.done

	move.w	Pdl1CenterW,d1			;else centered, use variable center value

.done	move.l	d1,Pdl1Wait
	move.l	Cycles,PdlOrigCyc
	rts

	CNOP	0,4

HW_C070_Mouse:					;*** Strobe PDLs, start timers (Mouse) ***
	
.pdl0	move.l	MyWindow,a0
	move.w	wd_MouseX(a0),d1

.pdl0ok	mulu.w	#2360,d1			;d1 = XPos * 2360 / 256  (cycles=0->2940)
	lsr.l	#8,d1
	move.l	d1,Pdl0Wait			;how many cycles timer will need for this...

.pdl1	move.l	MyWindow,a0
	move.w	wd_MouseY(a0),d1
		
	mulu.w	#3783,d1			;d1 = YPos * 3783 / 256 (cycles=0->2940)
	lsr.l	#8,d1			
	move.l	d1,Pdl1Wait

	move.l	Cycles,PdlOrigCyc
	rts


HW_C070_Analog:					;*** Strobe PDLs, start timers (Anlg Jystk) ***
	move.w	PotPosition,a0			;a0 = backup PotPosition value
	move.w	a0,d0
	
.ChkY	cmp.b	PotMinY,d0			;Check min Y value, extend if needed
	bhs.b	.1	
	move.b	d0,PotMinY
	bra.b	.2
.1	cmp.b	PotMaxY,d0			;Check max Y value, extend if needed
	bls.b	.2
	move.b	d0,PotMaxY
.2	moveq.l	#0,d1
	move.b	PotMaxY,d1
	sub.b	PotMinY,d1			;d1.l = Range of values (MaxY-MinY) in clean Long
	
	sub.b	PotMinY,d0			;d0 = Normalized Y value (starting at 0)

	tst.b	PdlMode				;if in Pdl mode, then value = Max-Value
	beq	.3
	neg.b	d0
	add.b	PotMaxY,d0

.3	lsl.w	#8,d0
	and.l	#$ffff,d0
	divu.w	d1,d0				;d0.w = Value * 256 / Range

	mulu.w	#12,d0
	addq.l	#1,d0				;d0.l = how many cycles for pdl strobe to go hi
	move.l	d0,Pdl1Wait


	move.w	a0,d0
	lsr.w	#8,d0
		
.ChkX	cmp.b	PotMinX,d0			;Check min X value, extend if needed
	bhs.b	.5
	move.b	d0,PotMinX
	bra.b	.6
.5	cmp.b	PotMaxX,d0			;Check max X value, extend if needed
	bls.b	.6
	move.b	d0,PotMaxX

.6	moveq.l	#0,d1
	move.b	PotMaxX,d1
	sub.b	PotMinX,d1			;d1.l = Range of values (MaxX-MinX) in clean Long
	
	sub.b	PotMinX,d0			;d0 = Normalized Y value (starting at 0)

	tst.b	PdlMode				;if in Pdl mode, then value = Max-Value
	beq	.7
	neg.b	d0
	add.b	PotMaxX,d0
	
.7	lsl.w	#8,d0
	and.l	#$ffff,d0
	divu.w	d1,d0				;d0.w = Value * 256 / Range

	mulu.w	#12,d0
	addq.l	#1,d0				;d0.l = how many cycles for pdl strobe to go hi
	move.l	d0,Pdl0Wait

	move.l	Cycles,PdlOrigCyc
	rts


HW_C070_AtariPdl:				;Merely jump to AnalogJoystick support call...
	jmp	HW_C070_Analog
		

HW_C071 			;I'm not sure EXACTLY how this works, but it sets PDL 1 timer
	move.l	HardwareWriteTbl+$70*4,a1	;just call $c070 routine for now...
;	move.l	$70*4(a1),a1
	jmp	(a1)

Pdl0Wait	dc.l	0		;# of cycles until paddle timer is done
Pdl1Wait	dc.l	0		;# of cycles  ""
PdlOrigCyc	dc.l	0		;Current cycles at strobe of pdls...
Pdl0CenterW	dc.w	127*11		;adjustable, default to 127
Pdl1CenterW	dc.w	127*11		;adjustable, ""

HW_C0F0				;MY CONTROL of hi-res shadowing on/off
				;write #$00 for both off, $f0 for pg 1 on, $0f for pg2 on
				;$ff for both pages on

	move.b	d0,(Mem_Ptr,a0.l)
	lea	PBPageList,a0
.OffOff	move.w	#$20,d0
.lp1	move.l	#0,(a0,d0.w*4)
	addq.w	#1,d0
	cmp.w	#$60,d0
	blo.b	.lp1

	cmp.b	#$f0,d1
	beq	.On1
	cmp.b	#$ff,d1
	beq	.On1
	beq	.NotOn1

.On1	move.w	#$20,d0
.lp2	move.l	#PBHgr1,(a0,d0.w*4)
	addq.w	#1,d0
	cmp.w	#$40,d0
	blo.b	.lp2

.NotOn1	cmp.b	#$0f,d1
	beq	.On2
	cmp.b	#$ff,d1
	beq	.On2
	rts	

.On2	move.w	#$40,d0
.lp3	move.l	#PBHgr2,(a0,d0.w*4)
	addq.w	#1,d0
	cmp.w	#$60,d0
	blo.b	.lp3
	rts




***************************************************************************************
* 16K card hardware routines - Each of the individual hardware accesses to slot 0
* ram card. Bank memory is handled via screened & re-directed memory writes, and
* reads are quick from direct ram (but swapped beforehand).    enter w/ d1=data, a0=addr
* NOTE: DBL-Access to enable ram writing is not implimented until I understand exact
*       circumstances of STAs, Pre-Enabled writing (another single enable disables?), etc

* The "Bank" memory ptd to by "Bank_PtrVar" is mapped as follows:
* $0000 - $0fff : Bank1 Ram (used in apple $d000 - $dfff)
* $1000 - $1fff : Bank2 Ram (used in apple $d000 - $dfff)
* $2000 - $3fff : 8K block (used in apple $e000 - $ffff)


HW_C080:					;*** Read from Ram Bank 2, write Disabled ***
	cmp.b	#2,CurrentReadBank		;reading from bank 2 already?
	beq.b	.setWrt

;	move.l	Bank_PtrVar,a0			;Put bank 2 in memory to read...
;	add.l	#$1000,a0			;a0 -> Bank 2 & 8K block
;	move.l	Mem_PtrVar,a1
;	add.l	#$d000,a1			;a1 -> Apple $d000
;	move.w	#$3000/4-1,d0
;.MemCpy	move.l	(a0)+,(a1)+
;	dbf	d0,.MemCpy

	move.l	Bank_PtrVar,a0			;Put bank 2 in memory to read...
	add.l	#$1000,a0			;a0 -> Bank 2 backup
	move.l	Mem_PtrVar,a1
	add.l	#$d000,a1			;a1 -> Apple $d000
	move.w	#$1000/4-1,d0
.2	move.l	(a0)+,(a1)+
	dbf	d0,.2

	tst.b	CurrentReadBank			;reading from common 8k block already?
	bne.b	.setWrt

.8k	move.l	Bank_PtrVar,a0			;Put 8k block in memory to read...
	add.l	#$2000,a0			;a0 -> 8k block backup
	move.l	Mem_PtrVar,a1
	add.l	#$e000,a1			;a1 -> Apple $e000
	move.w	#$2000/4-1,d0
.8	move.l	(a0)+,(a1)+
	dbf	d0,.8

.setWrt
	lea	PBPageList,a0			;write protect rom space...
	lea	$d0*4(a0),a0			;a0 -> PBPageList entry for $D0xx
	lea	PBNone,a1			;a1 -> Addr of "PBNone"
	move.w	#$30-1,d0
.WPRam	move.l	a1,(a0)+
	dbf	d0,.WPRam

.done	move.l	#$c080,Last_C08x
	move.b	#2,CurrentReadBank
	rts


	
HW_C081:					;*** Read from ROM, write Ram Bank 2 ***
	cmp.b	#0,CurrentReadBank		;reading from Rom already?
	beq.b	.setWrt

	move.l	Bank_PtrVar,a0			;Put Rom data in memory to read...
	add.l	#$4000,a0			;a0 -> Rom Buffer
	move.l	Mem_PtrVar,a1
	add.l	#$d000,a1			;a1 -> Apple $d000
	move.w	#$3000/4-1,d0
.RomCpy	move.l	(a0)+,(a1)+
	dbf	d0,.RomCpy

.setWrt
	lea	PBPageList,a0
	lea	$d0*4(a0),a0			;a0 -> PBPageList entry for $D0xx
	lea	PBBank2,a1			;a1 -> Addr of "PBBank2"
	move.w	#$10-1,d0
.Bnk2	move.l	a1,(a0)+
	dbf	d0,.Bnk2
	lea	PBBank,a1			;a1 -> Addr of "PBBank" 8k block
	move.w	#$20-1,d0
.Bnk8	move.l	a1,(a0)+
	dbf	d0,.Bnk8

.done	move.l	#$c081,Last_C08x
	move.b	#0,CurrentReadBank
	rts	


	
HW_C082:					;*** Read from ROM, write Disabled ***
	cmp.b	#0,CurrentReadBank		;reading from Rom already?
	beq.b	.setWrt

	move.l	Bank_PtrVar,a0			;Put Rom data in memory to read...
	add.l	#$4000,a0			;a0 -> ROM Buffer...
	move.l	Mem_PtrVar,a1
	add.l	#$d000,a1			;a1 -> Apple $d000
	move.w	#$3000/4-1,d0
.RomCpy	move.l	(a0)+,(a1)+
	dbf	d0,.RomCpy

.setWrt
	lea	PBPageList,a0
	lea	$d0*4(a0),a0			;a0 -> PBPageList entry for $D0xx
	lea	PBNone,a1			;a1 -> Addr of "PBNone"
	move.w	#$30-1,d0
.WPRam	move.l	a1,(a0)+
	dbf	d0,.WPRam

.done	move.l	#$c082,Last_C08x
	move.b	#0,CurrentReadBank
	rts



HW_C083						;*** Read & Write Ram Bank 2 ***
	cmp.b	#2,CurrentReadBank		;reading from bank 2 already?
	beq.b	.setWrt

;	move.l	Bank_PtrVar,a0
;	add.l	#$1000,a0			;a0 -> Bank 2 & 8K Block
;	move.l	Mem_PtrVar,a1
;	add.l	#$d000,a1			;a1 -> Apple $d000
;	move.w	#$3000/4-1,d0
;.MemCpy	move.l	(a0)+,(a1)+
;	dbf	d0,.MemCpy

	move.l	Bank_PtrVar,a0			;Put bank 2 in memory to read...
	add.l	#$1000,a0			;a0 -> Bank 2 backup
	move.l	Mem_PtrVar,a1
	add.l	#$d000,a1			;a1 -> Apple $d000
	move.w	#$1000/4-1,d0
.2	move.l	(a0)+,(a1)+
	dbf	d0,.2

	tst.b	CurrentReadBank			;reading from common 8k block already?
	bne.b	.setWrt

.8k	move.l	Bank_PtrVar,a0			;Put 8k block in memory to read...
	add.l	#$2000,a0			;a0 -> 8k block backup
	move.l	Mem_PtrVar,a1
	add.l	#$e000,a1			;a1 -> Apple $e000
	move.w	#$2000/4-1,d0
.8	move.l	(a0)+,(a1)+
	dbf	d0,.8

.setWrt
	lea	PBPageList,a0
	lea	$d0*4(a0),a0			;a0 -> PBPageList entry for $D0xx
	lea	PBBoth2,a1			;a1 -> Addr of "PBBoth2"
	move.w	#$10-1,d0
.Bnk2	move.l	a1,(a0)+
	dbf	d0,.Bnk2
;	lea	PBBank,a1			;a1 -> """"
	move.w	#$20-1,d0
.Bnk8	move.l	a1,(a0)+
	dbf	d0,.Bnk8

.done	move.l	#$c083,Last_C08x
	move.b	#2,CurrentReadBank
	rts	



HW_C088:					;*** Read from Ram Bank 1, write Disabled ***
	cmp.b	#1,CurrentReadBank		;reading from bank 1 already?
	beq.b	.setWrt

;	move.l	Bank_PtrVar,a0			
;	move.l	Mem_PtrVar,a1			;a0 -> Bank 1
;	add.l	#$d000,a1			;a1 -> Apple $d000
;	move.w	#$1000/4-1,d0
;.MemCpy	move.l	(a0)+,(a1)+
;	dbf	d0,.MemCpy
;	
;	add.l	#$1000,a0			;a0 -> 8K Bank  ( a1 -> Apple $e000)
;	move.w	#$2000/4-1,d0
;.Mem2	move.l	(a0)+,(a1)+
;	dbf	d0,.Mem2

	move.l	Bank_PtrVar,a0			;Put bank 1 in memory to read...
	move.l	Mem_PtrVar,a1			;a0 -> Bank 1 backup
	add.l	#$d000,a1			;a1 -> Apple $d000
	move.w	#$1000/4-1,d0
.1	move.l	(a0)+,(a1)+
	dbf	d0,.1

	tst.b	CurrentReadBank			;reading from common 8k block already?
	bne.b	.setWrt

.8k	move.l	Bank_PtrVar,a0			;Put 8k block in memory to read...
	add.l	#$2000,a0			;a0 -> 8k block backup
	move.l	Mem_PtrVar,a1
	add.l	#$e000,a1			;a1 -> Apple $e000
	move.w	#$2000/4-1,d0
.8	move.l	(a0)+,(a1)+
	dbf	d0,.8

.setWrt
	lea	PBPageList,a0
	lea	$d0*4(a0),a0			;a0 -> PBPageList entry for $D0xx
	lea	PBNone,a1			;a1 -> Addr of "PBNone"
	move.w	#$30-1,d0
.WPRam	move.l	a1,(a0)+
	dbf	d0,.WPRam

.done	move.l	#$c088,Last_C08x
	move.b	#1,CurrentReadBank
	rts



HW_C089:					;**** Read from ROM, write Ram Bank 1 ***
	cmp.b	#0,CurrentReadBank		;reading from Rom already?
	beq.b	.setWrt

	move.l	Bank_PtrVar,a0
	add.l	#$4000,a0			;a0 -> Rom Buffer
	move.l	Mem_PtrVar,a1
	add.l	#$d000,a1			;a1 -> Apple $d000
	move.w	#$3000/4-1,d0
.RomCpy	move.l	(a0)+,(a1)+
	dbf	d0,.RomCpy
	
.setWrt
	lea	PBPageList,a0
	lea	$d0*4(a0),a0			;a0 -> PBPageList entry for $D0xx
	lea	PBBank1,a1			;a1 -> Addr of "PBBank1"
	move.w	#$10-1,d0
.Bnk1	move.l	a1,(a0)+
	dbf	d0,.Bnk1
	lea	PBBank,a1			;a1 -> Addr of "PBBank" 8k block
	move.w	#$20-1,d0
.Bnk8	move.l	a1,(a0)+
	dbf	d0,.Bnk8

.done	move.l	#$c089,Last_C08x
	move.b	#0,CurrentReadBank
	rts	



HW_C08B						;*** Read & write Ram Bank 1 ***
	cmp.b	#1,CurrentReadBank		;reading from bank 1 already?
	beq.b	.setWrt

;	move.l	Bank_PtrVar,a0			
;	move.l	Mem_PtrVar,a1			;a0 -> Bank 1
;	add.l	#$d000,a1			;a1 -> Apple $d000
;	move.w	#$1000/4-1,d0
;.MemCpy	move.l	(a0)+,(a1)+
;	dbf	d0,.MemCpy
;
;	add.l	#$1000,a0			;a0 -> 8K Bank  ( a1 -> Apple $e000)
;	move.w	#$2000/4-1,d0
;.Mem2	move.l	(a0)+,(a1)+
;	dbf	d0,.Mem2

	move.l	Bank_PtrVar,a0			;Put bank 1 in memory to read...
	move.l	Mem_PtrVar,a1			;a0 -> Bank 1 backup
	add.l	#$d000,a1			;a1 -> Apple $d000
	move.w	#$1000/4-1,d0
.1	move.l	(a0)+,(a1)+
	dbf	d0,.1

	tst.b	CurrentReadBank			;reading from common 8k block already?
	bne.b	.setWrt

.8k	move.l	Bank_PtrVar,a0			;Put 8k block in memory to read...
	add.l	#$2000,a0			;a0 -> 8k block backup
	move.l	Mem_PtrVar,a1
	add.l	#$e000,a1			;a1 -> Apple $e000
	move.w	#$2000/4-1,d0
.8	move.l	(a0)+,(a1)+
	dbf	d0,.8
	
.setWrt
	lea	PBPageList,a0
	lea	$d0*4(a0),a0			;a0 -> PBPageList entry for $D0xx
	lea	PBBank1RW,a1			;a1 -> Addr of "PBBank1RW" (read & write)
	move.w	#$10-1,d0
.Bnk2	move.l	a1,(a0)+
	dbf	d0,.Bnk2
	lea	PBBoth2,a1			;a1 -> addr of "PBBoth2"
	move.w	#$20-1,d0
.Bnk8	move.l	a1,(a0)+
	dbf	d0,.Bnk8

.done	move.l	#$c08b,Last_C08x
	move.b	#1,CurrentReadBank
	rts	
	
	CNOP	0,4
CurrentReadBank	 dc.b	0			;= 0 (rom), 1 (bank 1), 2 (bank 2)
;CurrentWriteBank dc.b	0			;= 0 (rom), 1 (bank 1), 2 (bank 2)

	CNOP	0,4
PBBank1	move.l	Bank_PtrVar,a1		;Write a byte to Bank 1
	sub.l	#$d000,a1		;offset
	move.b	d1,(a1,a0.l)
	rts
	
PBBank2	move.l	Bank_PtrVar,a1		;Write a byte to Bank 2
	sub.l	#$c000,a1		;offset...  - $d000 + $1000
	move.b	d1,(a1,a0.l)
	rts

PBBoth2	move.l	Bank_PtrVar,a1	   ;Write to Bank 2/8k bank & apple $d000 area (rd/wrt)
	sub.l	#$c000,a1
	move.b	d1,(a1,a0.l)
	move.b	d1,(Mem_Ptr,a0.l)
	rts

PBBank1RW
	move.l	Bank_PtrVar,a1		;Write a byte to Bank 1 AND Apple $dxxx
	sub.l	#$d000,a1
	move.b	d1,(a1,a0.l)
	move.b	d1,(Mem_Ptr,a0.l)
	rts

PBBank	move.l	Bank_PtrVar,a1		;Write a byte to 8K bank ($e000-$ffff)
	sub.l	#$c000,a1		; - $e000 + $2000
	move.b	d1,(a1,a0.l)
PBNone	rts

Last_C08x	dc.l	0		;holds last address touched
;BankRamStat	dc.l	0		;

***************************************************************************************
* Disk hardware routines - Each of the individual hardware accesses to the slot 6
* drive controller. For optimization, when the drive 1 is turned off, most entries
* for slot 6 h/w jump tables are turned off, and turned back on when used.
* This prevents contant checks for each byte read/written... 
* ---(currently, only checks Drive #, not motor status for speed)

	CNOP	0,4

HWw_C028:			;MY Custom Hardware location for Drive i/o
	cmp.b	#$22,d1
	bhi.b	.NotTrk
.Trk	move.b	d1,disk_Track
	bra	NewTk

.NotTrk	cmp.b	#$90,d1
	beq.b	.MkTbl
	rts
.MkTbl	lea	DiskDataToValTbl,a0
	move.l	#$36c,a1		;Put table in $36c -> $3d5 magically!
.lp	move.b	(a0)+,(Mem_Ptr,a1.l)
	addq.l	#1,a1
	cmp.w	#$3d5,a1
	bls.b	.lp
	rts

HWr_C028:		;MY Custom Hardware location for Drive i/o
			;Upon reading this location, the next byte will be read from the disk,
			;and will be returned as NORMALIZED 6 bit number (via table)...

	jsr	HW_C0EC		;go read next byte of data...
	and.w	#$ff,d0		;mask out top...
	cmp.b	#$96,d0		;ensure in range (in case no disk image present)
	bhs.b	.InRng
	move.b	#$96,d0
.InRng	move.l	#DiskDataToValTbl-$96,a0	;look up normal val in table...
	move.b	(a0,d0.w),d0
	rts

HWr_C029:		;MY Custom Hardware location for Drive i/o
			;Reading this location will read next 2 bytes from disk,
			;and return a byte in 4X4 decoded format...
	jsr	HW_C0EC
	lsl.b	d0
	or.b	#$01,d0
	move.w	d0,-(sp)
	jsr	HW_C0EC
	and.w	(sp)+,d0
	rts
	
			;The lookup table used as $ba00,y ALWAYS had Y >= $96,
			;so here is the pertinant portion of the table ($ba96-$baff)
			;Make sure to subtract $96 when getting the address of it.
DiskDataToValTbl:
	dc.b	$00,$01,$00,$00,$02,$03,$00,$04,$05,$06,$00,$00,$00,$00,$00
	dc.b	$00,$07,$08,$00,$00,$00,$09,$0A,$0B,$0C,$0D,$00,$00,$0E,$0F
	dc.b	$10,$11,$12,$13,$00,$14,$15,$16,$17,$18,$19,$1A,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$1B,$00,$1C,$1D,$1E,$00,$00
	dc.b	$00,$1F,$00,$00,$20,$21,$00,$22,$23,$24,$25,$26,$27,$28,$00
	dc.b	$00,$00,$00,$00,$29,$2A,$2B,$00,$2C,$2D,$2E,$2F,$30,$31,$32
	dc.b	$00,$00,$33,$34,$35,$36,$37,$38,$00,$39,$3A,$3B,$3C,$3D,$3E,$3F

	CNOP	0,4

HW_C0E1	move.w	disk_DriveW,d1			;index of 0 or 1 into disk drive fields...
	move.b	(disk_Phase.l,d1.w),d0		;access fields (indexed to drive 1 or 2)
	move.b	#0,(disk_Phase.l,d1.w)
	cmp.b	#3,d0				;phase 0 high (track!)
	beq	IncTk
	cmp.b	#1,d0
	beq	DecTk
	rts

HW_C0E3	move.w	disk_DriveW,d1
	move.b	#1,(disk_Phase.l,d1.w)		;phase 1 high (half track)
	rts

HW_C0E5	move.w	disk_DriveW,d1
	move.b	(disk_Phase.l,d1.w),d0
	move.b	#2,(disk_Phase.l,d1.w)
	cmp.b	#1,d0				;phase 2 high (track!)
	beq	IncTk
	cmp.b	#3,d0
	beq	DecTk
	rts

HW_C0E7	move.w	disk_DriveW,d1
	move.b	#3,(disk_Phase.l,d1.w)		;phase 3 high (half track)
	rts

IncTk	move.w	disk_DriveW,d1
	addq.b	#1,(disk_Track.l,d1.w)		;Increment Track!
	cmp.b	#34,(disk_Track.l,d1.w)
	bls	NewTk
	move.b	#34,(disk_Track.l,d1.w)
	bra	NewTk

DecTk	move.w	disk_DriveW,d1
	subq.b	#1,(disk_Track.l,d1.w)			;Decrement Track!
	bpl	NewTk
	move.b	#0,(disk_Track.l,d1.w)

;	VVV -fall through- VVV

NewTk	move.w	disk_DriveW,d1
	moveq.l	#0,d0				;Adjusts ptrs after moving to new track...
	move.b	(disk_Track.l,d1.w),d0
	mulu.w	#disk_TrackLen,d0		;calc ptr to where track starts in memory
	add.l	(disk_Buffer.l,d1.w*4),d0
	move.l	d0,disk_TkStrt
	move.l	#0,disk_TkPos			;reset track position/offset counter...
	
PrintTk						;*** print trk num in title bar ***
	move.w	disk_DriveW,d1
	move.b	(disk_Track.l,d1.w),d0
	
	lsr.b	#4,d0				;convert Track to 2 hex digits...
	add.b	#'0',d0
	move.b	d0,.Disk1Digits
	move.b	d0,.Disk2Digits
	move.b	(disk_Track.l,d1.w),d0
	and.b	#$0f,d0
	add.b	#'0',d0
	cmp.b	#'9',d0
	bls	.ok
	add.b	#'A'-'9'-1,d0
.ok	move.b	d0,.Disk1Digits+1
	move.b	d0,.Disk2Digits+1

	move.l	(.MsgPtr.l,d1.w*4),NewStatusMsgPtr

	rts

.MsgPtr	dc.l	.Disk1Msg,.Disk2Msg

.Disk1Msg    dc.b  "           DRIVE ONE : TK $"
.Disk1Digits dc.b  "00",0,10

.Disk2Msg    dc.b  "           DRIVE TWO : TK $"
.Disk2Digits dc.b  "00",0,10

;	     dc.b  "1234567890123456789012345678901234567890"
	CNOP	0,4
HW_C0E8	move.b	#0,disk_Motor			;turn drive off!
	rts					;<-- no motor check currently for speed...


HW_C0E9	move.b	#1,disk_Motor			;turn drive on!
	tst.w	disk_DriveW
	beq	PrintTk				;reprint StatusBar if drive #1 active...
	tst.l	disk_Buffer2			;or if drive #2 present & active...
	bne	PrintTk				;no motor check currently for speed...
	move.l	#.NothingMsg,NewStatusMsgPtr
	rts
.NothingMsg	dc.b	0,1			;empty message, 1 tick duration (blank)
	
HW_C0EA	move.w	#0,disk_DriveW			;drive 1 selected

	lea	HardwareReadTbl,a0		;enable full drive function by patching
	move.l	#HW_C0E1,$e1*4(a0)		;in routines that might have been disabled
	move.l	#HW_C0E3,$e3*4(a0)
	move.l	#HW_C0E5,$e5*4(a0)
	move.l	#HW_C0E7,$e7*4(a0)
	move.l	#HW_C0EC,$ec*4(a0)

	lea	HardwareWriteTbl,a0
	move.l	#HW_C0E1,$e1*4(a0)
	move.l	#HW_C0E3,$e3*4(a0)
	move.l	#HW_C0E5,$e5*4(a0)
	move.l	#HW_C0E7,$e7*4(a0)
	move.l	#HW_C0EC,$ec*4(a0)
	bra	NewTk				;reset track pointers for this drive...
	rts	

HW_C0EB	tst.l	disk_Buffer2			;is there a 2nd drive buffer enabled?
	beq.b	.NoDrv
	move.w	#1,disk_DriveW			;drive 2 selected
	bra	NewTk				;reset track pointers for this drive...
	rts
		
.NoDrv	lea	HardwareReadTbl,a0		;Only 1 drive system, so when #2 selected,
	moveq.l	#0,d0				;disable mechanical drive functions!
	move.l	d0,$e1*4(a0)
	move.l	d0,$e3*4(a0)
	move.l	d0,$e5*4(a0)
	move.l	d0,$e7*4(a0)
	move.l	#HW_C0EC_DrvOff,$ec*4(a0)

	lea	HardwareWriteTbl,a0
	move.l	d0,$e1*4(a0)
	move.l	d0,$e3*4(a0)
	move.l	d0,$e5*4(a0)
	move.l	d0,$e7*4(a0)
	move.l	#HW_C0EC_DrvOff,$ec*4(a0)
	rts


HW_C0EC	move.b	#0,disk_Q6			;q6 = low;  READ / WRITE Latch!

	tst.b	disk_Q7
	bne.b	.write				;if q7=hi then go write latch to disk
.read	move.l	disk_TkStrt,a0			;else read
	move.l	disk_TkPos,d1	
	move.b	(a0,d1.w),d0			;get data
	bset	#7,d0				;force disk data hi-bit high
	addq.w	#1,d1
	cmp.w	#disk_TrackLen,d1		;within length of track?
	bhs.b	.Rept
	move.l	d1,disk_TkPos
	rts
.Rept	move.l	#0,disk_TkPos
	rts
;.noZero	move.b	#$ff,d0
;	bra.b	.ok

.write	move.l	disk_TkStrt,a0
	move.l	disk_TkPos,d1
	move.b	disk_Latch,(a0,d1.w)		;write data
	addq.w	#1,d1
	cmp.w	#disk_TrackLen,d1		;within length of track?
	blo.b	.NoRep2
	move.w	#0,d1
.NoRep2	move.l	d1,disk_TkPos
	move.w	disk_DriveW,d1			;and mark drive as being changed
	move.b	#1,(disk_Changed.l,d1.w)
	rts


HW_C0EC_DrvOff:
	move.b	#0,disk_Q6		;q6 = low; return $ff while drive is off
	move.b	#$ff,d0
	rts


HWr_C0ED move.b	#1,disk_Q6		;on read, q6 = Hi
	rts


HWw_C0ED move.b	#1,disk_Q6		;on write, q6 = Hi  ;  load latch
	move.b	d1,disk_Latch
	rts


HW_C0EE	move.b	#0,disk_Q7		; q7 = lo
	tst.b	disk_Q6			; if q6=1 then return Write Prot Status
	bne	.WPstat
	rts
.WPstat	move.b	#0,d0			;return -1 for WP'ed disk
	rts


HW_C0EF	move.b	#1,disk_Q7		; q7 = hi
	rts

***************************************************************************************
* Video handling... Hardware switches, mode checking, redundancy elimination, etc...
*
	CNOP	0,4
HW_C050	bclr.b	#0,VidMode	;GRAPHICS! (vs text)
	bne	ChkVid		;if this video flag already set, do nothing...
	bra	ProgressiveRetVal
	
	CNOP	0,4
HW_C051	bset.b	#0,VidMode	;TEXT! (vs graphics)
	beq	ChkVid
	bra	ProgressiveRetVal
	
	CNOP	0,4
HW_C052	bclr.b	#1,VidMode	;FULL SCRN GRFX (vs mixed text)
	bne	ChkVid
	bra	ProgressiveRetVal
	
	CNOP	0,4
HW_C053	bset.b	#1,VidMode	;MIXED GRFX/TXT (vs full scrn)
	beq	ChkVid
	bra	ProgressiveRetVal
	
	CNOP	0,4
HW_C054	bclr.b	#2,VidMode	;PAGE 1 (vs 2)
	bne	ChkVid
	bra	ProgressiveRetVal
	
	CNOP	0,4
HW_C055	bset.b	#2,VidMode	;PAGE 2 (vs 1)
	beq	ChkVid
	bra	ProgressiveRetVal
	
	CNOP	0,4
HW_C056	bclr.b	#3,VidMode	;LORES (vs Hires)
	bne	ChkVid
	bra	ProgressiveRetVal
		
	CNOP	0,4
HW_C057	bset.b	#3,VidMode	;HIRES (vs Lores)
	beq	ChkVid
	bra	ProgressiveRetVal

	CNOP	0,4
*--------------------------------------------------------------------------------*	
* ChkVid - Called after any of the video modes have been changed. This routine
* 	   first checks to see if the next instruction will change another video
* 	   attribute, and if so, does nothing.
*	   Otherwise, it examines "VidMode" and sets pointers to the proper BPlanes,
*	   refreshes if necessary, and remakes the display.
*	   CALL ONLY from 6502 CPU context!

ChkVid	move.l	-1(PCount),d0     ;1st check if next inst changes video also...
	cmp.b	#$c0,d0
	bne.b	.chgVid
	lsr.l	#8,d0
	cmp.b	#$50,d0
	blo.b	.chgVid
	cmp.b	#$57,d0
	bhi.b	.chgVid		;at this point, its $c050 - $c057, now check inst

	lsr.l	#8,d0
	cmp.b	#$2c,d0		;bit $xxxx
;	beq	.done
	cmp.b	#$ad,d0		;lda $xxxx
;	beq	.done		
	cmp.b	#$8d,d0		;sta $xxxx
;	beq	.done		;any .done match means return, & do video on next inst

.chgVid
	clr.w	d0
	move.b	VidMode,d0
	jmp	([VideoDispatchTbl.l,d0.w*4])

.done	rts

VideoDispatchTbl
	dc.l	SetGr1,SetTxt1,SetGrTxt1,SetTxt1
	dc.l	SetGr2,SetTxt2,SetGrTxt2,SetTxt2
	dc.l	SetHgr1,SetTxt1,SetHgrTxt1,SetTxt1
	dc.l	SetHgr2,SetTxt2,SetHgrTxt2,SetTxt2

SetTxt1:
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a1			;SET BITMAP fields...
	lea	sc_BitMap(a1),a1		;a1 -> Screen.BitMap
	move.b	#2,bm_Depth(a1)
	move.l	Gr1_Planes,a0
	move.l	a0,bm_Planes(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+4(a1)

	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	move.l	#PBTxt1,PBPageList+16
	move.l	#PBTxt1,PBPageList+20
	move.l	#PBTxt1,PBPageList+24
	move.l	#PBTxt1,PBPageList+28

	jsr	RefreshTxt1

	move.b	#1,FlashEnableB

	lea	TxtColorTable,a1		;a1 -> MyColorTable for this mode...

	bra	SetDone


SetGr1:
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a1			;SET BITMAP fields...
	lea	sc_BitMap(a1),a1		;a1 -> Screen.BitMap
	move.b	#5,bm_Depth(a1)
	move.l	Gr1_Planes,a0
	move.l	a0,bm_Planes(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+4(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+8(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+12(a1)
	move.l	BackDrop_Plane,a0		;5th bplane! (choose text/gr colors)
	move.l	a0,bm_Planes+16(a1)
	
	jsr	SetBackDropToGrfx

	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	move.l	#PBGr1,PBPageList+16
	move.l	#PBGr1,PBPageList+20
	move.l	#PBGr1,PBPageList+24
	move.l	#PBGr1,PBPageList+28

	jsr	RefreshTxt1

	move.b	#0,FlashEnableB

	lea	GrColorTable,a1		;a1 -> MyColorTable for this mode...

	bra	SetDone

SetGrTxt1:
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a1			;SET BITMAP fields...
	lea	sc_BitMap(a1),a1		;a1 -> Screen.BitMap
	move.b	#5,bm_Depth(a1)
	move.l	Gr1_Planes,a0
	move.l	a0,bm_Planes(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+4(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+8(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+12(a1)
	move.l	BackDrop_Plane,a0		;5th bplane! (choose text/gr colors)
	move.l	a0,bm_Planes+16(a1)

	jsr	SetBackDropToText
	
	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	move.l	#PBGrTxt1,PBPageList+16
	move.l	#PBGrTxt1,PBPageList+20
	move.l	#PBGrTxt1,PBPageList+24
	move.l	#PBGrTxt1,PBPageList+28

	jsr	ClearXtraPlanesTxt1
	jsr	RefreshTxt1

	move.b	#1,FlashEnableB

	lea	GrColorTable,a1

	bra	SetDone

SetTxt2:
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a1			;SET BITMAP fields...
	lea	sc_BitMap(a1),a1		;a1 -> Screen.BitMap
	move.b	#2,bm_Depth(a1)
	move.l	Gr2_Planes,a0
	move.l	a0,bm_Planes(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+4(a1)

	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	move.l	#PBTxt2,PBPageList+32
	move.l	#PBTxt2,PBPageList+36
	move.l	#PBTxt2,PBPageList+40
	move.l	#PBTxt2,PBPageList+44

	jsr	ClearXtraPlanesTxt2
	jsr	RefreshTxt2

	move.b	#1,FlashEnableB

	lea	TxtColorTable,a1

	bra	SetDone


SetGr2:
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a1			;SET BITMAP fields...
	lea	sc_BitMap(a1),a1		;a1 -> Screen.BitMap
	move.b	#5,bm_Depth(a1)
	move.l	Gr2_Planes,a0
	move.l	a0,bm_Planes(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+4(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+8(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+12(a1)
	move.l	BackDrop_Plane,a0		;5th bplane! (choose text/gr colors)
	move.l	a0,bm_Planes+16(a1)
	
	jsr	SetBackDropToGrfx


	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	move.l	#PBGr2,PBPageList+32
	move.l	#PBGr2,PBPageList+36
	move.l	#PBGr2,PBPageList+40
	move.l	#PBGr2,PBPageList+44

	jsr	RefreshTxt2

	move.b	#0,FlashEnableB

	lea	GrColorTable,a1

	bra	SetDone

SetGrTxt2:
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a1			;SET BITMAP fields...
	lea	sc_BitMap(a1),a1		;a1 -> Screen.BitMap
	move.b	#5,bm_Depth(a1)
	move.l	Gr2_Planes,a0
	move.l	a0,bm_Planes(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+4(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+8(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+12(a1)
	move.l	BackDrop_Plane,a0		;5th bplane! (choose text/gr colors)
	move.l	a0,bm_Planes+16(a1)
	
	jsr	SetBackDropToText

	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	move.l	#PBGrTxt2,PBPageList+32
	move.l	#PBGrTxt2,PBPageList+36
	move.l	#PBGrTxt2,PBPageList+40
	move.l	#PBGrTxt2,PBPageList+44

	jsr	ClearXtraPlanesTxt2
	jsr	RefreshTxt2

	move.b	#1,FlashEnableB

	lea	GrColorTable,a1		

	bra	SetDone

SetHgr1:
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a1			;SET BITMAP fields...
	lea	sc_BitMap(a1),a1		;a1 -> Screen.BitMap
	move.b	#4,bm_Depth(a1)
	move.l	Hgr1_Planes,a0
	move.l	a0,bm_Planes(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+4(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+8(a1)
	move.l	BackDrop_Plane,a0		;4th bplane! (choose text/gr colors)
	move.l	a0,bm_Planes+12(a1)

	jsr	SetBackDropToGrfx

	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	moveq.l	#0,d0
	move.l	d0,PBPageList+16		;ignore txt1 page...
	move.l	d0,PBPageList+20
	move.l	d0,PBPageList+24
	move.l	d0,PBPageList+28

	move.l	d0,PBPageList+32		;and ignore txt2...
	move.l	d0,PBPageList+36
	move.l	d0,PBPageList+40
	move.l	d0,PBPageList+44

	cmp.l	#PBHgr1,PBPageList+$20*4	;already doing full-scrn hgr1
	beq	.SkpChg

	move.l	#PBHgr1,d0
	move.l	#PBPageList+$20*4,a0
	jsr	InstallHgrServer

	jsr	RefreshHgr1
	
.SkpChg
	move.b	#0,FlashEnableB

	lea	HiResColorTable,a1

	bra	SetDone

SetHgrTxt1:
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a1			;SET BITMAP fields...
	lea	sc_BitMap(a1),a1		;a1 -> Screen.BitMap
	move.b	#4,bm_Depth(a1)
	move.l	Hgr1_Planes,a0
	move.l	a0,bm_Planes(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+4(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+8(a1)
	move.l	BackDrop_Plane,a0		;4th bplane! (choose text/gr colors)
	move.l	a0,bm_Planes+12(a1)

	jsr	SetBackDropToText

	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	move.l	#PBHgrTxt1,PBPageList+16
	move.l	#PBHgrTxt1,PBPageList+20
	move.l	#PBHgrTxt1,PBPageList+24
	move.l	#PBHgrTxt1,PBPageList+28

	cmp.l	#PBHgr1Top,PBPageList+$20*4
	beq	.SkpChg				;already doing mixed-scrn hgr1

	move.l	#PBHgr1Top,d0
	move.l	#PBPageList+$20*4,a0
	jsr	InstallHgrServer

	jsr	RefreshHgr1

.SkpChg	jsr	RefreshTxt1

	move.b	#1,FlashEnableB

	lea	HiResColorTable,a1		

	bra	SetDone


SetHgr2:
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a1			;SET BITMAP fields...
	lea	sc_BitMap(a1),a1		;a1 -> Screen.BitMap
	move.b	#4,bm_Depth(a1)
	move.l	Hgr2_Planes,a0
	move.l	a0,bm_Planes(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+4(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+8(a1)
	move.l	BackDrop_Plane,a0		;4th bplane! (choose text/gr colors)
	move.l	a0,bm_Planes+12(a1)

	jsr	SetBackDropToGrfx

	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	moveq.l	#0,d0
	move.l	d0,PBPageList+16		;ignore txt1 page...
	move.l	d0,PBPageList+20
	move.l	d0,PBPageList+24
	move.l	d0,PBPageList+28

	move.l	d0,PBPageList+32		;and ignore txt2...
	move.l	d0,PBPageList+36
	move.l	d0,PBPageList+40
	move.l	d0,PBPageList+44

	cmp.l	#PBHgr2,PBPageList+$40*4
	beq	.SkpChg				;already doing full-scrn hgr2

	move.l	#PBHgr2,d0
	move.l	#PBPageList+$40*4,a0
	jsr	InstallHgrServer

	jsr	RefreshHgr2

.SkpChg
	move.b	#0,FlashEnableB

	lea	HiResColorTable,a1

	bra	SetDone



SetHgrTxt2:
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a1			;SET BITMAP fields...
	lea	sc_BitMap(a1),a1		;a1 -> Screen.BitMap
	move.b	#4,bm_Depth(a1)
	move.l	Hgr2_Planes,a0
	move.l	a0,bm_Planes(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+4(a1)
	add.l	#LINES*40,a0
	move.l	a0,bm_Planes+8(a1)
	move.l	BackDrop_Plane,a0		;4th bplane! (choose text/gr colors)
	move.l	a0,bm_Planes+12(a1)

	jsr	SetBackDropToText

	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	move.l	#PBHgrTxt2,PBPageList+32
	move.l	#PBHgrTxt2,PBPageList+36
	move.l	#PBHgrTxt2,PBPageList+40
	move.l	#PBHgrTxt2,PBPageList+44

	cmp.l	#PBHgr2Top,PBPageList+$40*4
	beq	.SkpChg				;already doing mixed-scrn hgr2

	move.l	#PBHgr2Top,d0
	move.l	#PBPageList+$40*4,a0
	jsr	InstallHgrServer

	jsr	RefreshHgr2

.SkpChg	jsr	RefreshTxt2

	move.b	#1,FlashEnableB

	lea	HiResColorTable,a1

	bra	SetDone

;--------------------	

InstallHgrServer:		;Plugs in server for either Hi-res memory dispatchs
				;Enter: d0 = Addr of HiRes server
				;	a0-> Base of server table (PBPageList+$20*4 or +$40*4)
	move.w	#$20-1,d1
.lp	move.l	d0,(a0)+
	dbf	d1,.lp
	rts

;--------------------	

SetDone:					;enter w/ a1 -> MyColorTable
	move.l	Cycles,d1
	sub.l	LastVideoCycle,d1		;skip if we are in a "page flipping" rtn
	move.l	Cycles,LastVideoCycle
	cmp.l	#5680,d1			;1/3 the cycles in a single frame...
	blo	.TooSoon			;(LATER check if more than 3 changes happen 1st)

.DoIt	clr.l	DoVideoFlag			;enter w/ a1 -> MyColorTable
	cmp.l	LastColorTable,a1
	beq.b	.same


.ChgCol	move.l	a1,-(sp)

	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a0
	CALLINT	MakeScreen			;Rebuild local VPort copper list
	CALLINT	RethinkDisplay			;Rebuild Display

	move.l	MyScreen,a0			;Set Colors... (IF NEEDED!)
	move.l	(sp)+,a1
	move.l	a1,LastColorTable
	lea	sc_ViewPort(a0),a0		;a0 -> Screen ViewPort
	move.w	(a1)+,d0			;d0 = # of colors, a1 -> ColorList
	CALLGRAF LoadRGB4			;(takes effect immed, but doesn't ReThink display)
						;(not very fast either)
	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	bra.b	ProgressiveRetVal

						;enter w/ a1 -> MyColorTable
.same
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a0			;Rebuild local VPort copper list
	CALLINT	MakeScreen
	CALLINT	RethinkDisplay			;and Rebuild Display

	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	bra.b	ProgressiveRetVal

.TooSoon					;enter w/ a1 -> MyColorTable
	move.l	a1,DoVideoFlag			;do nothing now, Main routine will change it soon...

ProgressiveRetVal:				;return the next value from a list, because
						;some games expect certain values in HW locations.

	move.l	.ListPtr,a0
	move.b	(a0)+,d0
	bne.b	.ok
	lea	.List,a0
.ok	move.l	a0,.ListPtr
	rts

.ListPtr dc.l	.List				; (Drol Anim wants 4 x $80)
.List	 dc.b	$20,$20,$20,$20,$80,$80,$80,$80,$ff,$ff,$ff,$ff,$81,$81,$00

					

;--------------------	
	CNOP	0,4

DoVideoFlag	dc.l	0
LastVideoCycle	dc.l	0
IBaseLock	dc.l	0
LastColorTable	dc.l	0
	EVEN

;--------------------	

RefreshVideo:					;----- "refreshes" all video related ram -----
						; not performance critical (after using screen)
	move.l	d2,-(sp)
	bsr	ChkVid
	move.l	#$400,d2			;   (only call from 6502 context)
.lp	move.l	d2,d0
	move.b	(Mem_Ptr,d0.l),d1		;FGETBYTE
	addq.b	#$1,(Mem_Ptr,d0.l)		;must change mem so Hi-res routines re-draw...
	PUTBYTE

	addq.l	#1,d2
	cmp.l	#$6000,d2
	blo	.lp
	move.l	(sp)+,d2
	rts

;--------------------	

RefreshTxt1:
.rfrsh	move.l	d2,-(sp)			;Refresh video ram $400 -> $7ff only
	move.l	#$400,d2
.lp	move.l	d2,d0
	move.b	(Mem_Ptr,d0.l),d1
	addq.b	#$1,(Mem_Ptr,d0.l)
	PUTBYTE
	addq.l	#1,d2
	cmp.w	#$800,d2
	blo	.lp
	move.l	(sp)+,d2
	rts


ClearXtraPlanesTxt1:
	move.l	Gr1_Planes,a0	
	add.l	#LINES*40*2+160*40,a0	;a0 ->3 Bplane line #160
	moveq.l	#$0,d0
	move.w	#32*40/4-1,d1		;clear 32 lines in text window...
.fill	move.l	d0,LINES*40(a0)		;bplanes 3 & 4
	move.l	d0,(a0)+
	dbf	d1,.fill
	rts

;--------------------	

RefreshTxt2:
.rfrsh	move.l	d2,-(sp)		;Refresh video ram $800 -> $bff
	move.l	#$800,d2
.lp	move.l	d2,d0
	move.b	(Mem_Ptr,d0.l),d1
	addq.b	#$1,(Mem_Ptr,d0.l)
	PUTBYTE
	addq.l	#1,d2
	cmp.w	#$c00,d2
	blo.b	.lp
	move.l	(sp)+,d2
	rts


ClearXtraPlanesTxt2:
	move.l	Gr2_Planes,a0	
	add.l	#LINES*40*2+160*40,a0	;a0 ->3 Bplane line #160
	moveq.l	#$0,d0
	move.w	#32*40/4-1,d1		;clear 32 lines in text window...
.fill	move.l	d0,LINES*40(a0)		;bplanes 3 & 4
	move.l	d0,(a0)+
	dbf	d1,.fill
	rts

;--------------------	

RefreshHgr1:				;clear txt at edges / hi-res bplns...
.clearXtraPlanes:
	move.l	Hgr1_Planes,a0
	add.l	#160*40,a0	;a0 ->Bplane1 line #160
	moveq.l	#$0,d0
	move.w	#32*40/4-1,d1		;clear 32 lines in text window...
.fill	move.l	d0,LINES*40*2(a0)		;bplanes 3
	move.l	d0,LINES*40(a0)			;	2
	move.l	d0,(a0)+			;	1
	dbf	d1,.fill


.Hgr1RefreshBottom:
	movem.l	a2-a4/d2-d7,-(sp)
	move.l	HiResDrawTbl,a1			;a1 -> HiRes Draw Table (Preserve!)

* Step through all the HGR line addresses...

	move.l	#$2250,a3	;a3 = main base counter...
	move.l	a3,a2		;a2 -> Base of HGR line we are doing (preserve!)
	
.loop	move.l	#40-1,d2
1$	move.l	a2,d0		;address
	add.l	d2,d0
	move.b	(Mem_Ptr,d0.l),d1	;FGETBYTE
	addq.b	#$1,(Mem_Ptr,d0.l)	;must change mem so Hi-res routines re-draw...
	PUTBYTE
	dbf	d2,1$

.done
	add.w	#$400,a2	;$2000 -> $2400 -> ... -> $3800 (each line in grp of 8)
	cmp.w	#$4000,a2
	blo	.loop
	sub.w	#$2000-$80,a2
	cmp.w	#$2400,a2	;$2000 -> $2080 -> $2100 -> $2180 -> ... -> $2380
	blo	.loop

	add.w	#$28,a3		;from $2000 -> $2028 -> $2050
	move.w	a3,a2
	cmp.w	#$2050,a3
	bls	.loop	

	movem.l	(sp)+,a2-a4/d2-d7
	rts

;--------------------	

RefreshHgr2:
.clearXtraplanes:
	move.l	Hgr2_Planes,a0
	add.l	#160*40,a0	;a0 ->Bplane1 line #160
	moveq.l	#$0,d0
	move.w	#32*40/4-1,d1		;clear 32 lines in text window...
.fill	move.l	d0,LINES*40*2(a0)		;bplanes 3
	move.l	d0,LINES*40(a0)			;	2
	move.l	d0,(a0)+			;	1
	dbf	d1,.fill

.Hgr2RefreshBottom:
	movem.l	a2-a4/d2-d7,-(sp)
	move.l	HiResDrawTbl,a1			;a1 -> HiRes Draw Table (Preserve!)

			;Step through all the HGR line addresses...
	move.l	#$4250,a3	;a3 = main base counter...
	move.l	a3,a2		;a2 -> Base of HGR line we are doing (preserve!)
	
.loop	move.l	#40-1,d2
1$	move.l	a2,d0		;address
	add.l	d2,d0
	move.b	(Mem_Ptr,d0.l),d1	;FGETBYTE
	addq.b	#$1,(Mem_Ptr,d0.l)	;must change mem so Hi-res routines re-draw...
	PUTBYTE
	dbf	d2,1$

.done	add.w	#$400,a2	;$4000 -> $4400 -> ... -> $5800 (each line in grp of 8)
	cmp.w	#$6000,a2
	blo	.loop
	sub.w	#$2000-$80,a2
	cmp.w	#$4400,a2	;$4000 -> $4080 -> $4100 -> $4180 -> ... -> $2380
	blo	.loop

	add.w	#$28,a3		;from $4000 -> $4028 -> $4050
	move.w	a3,a2
	cmp.w	#$4050,a3
	bls	.loop	

	movem.l	(sp)+,a2-a4/d2-d7
	rts

;--------------------	

SetBackDropToText:
	move.l	BackDrop_Plane,a0	;and completely fill it!
	add.l	#160*40,a0		;a0 -> Bplane line #160
	tst.b	(a0)
	beq.b	.done			;if already in mode, then return

	moveq.l	#$0,d0
	move.w	#32*40/4-1,d1		;change 32 lines in text window...
.fill	move.l	d0,(a0)+
	dbf	d1,.fill
.done	rts

SetBackDropToGrfx:
	move.l	BackDrop_Plane,a0	;and completely fill it!
	add.l	#160*40,a0		;a0 -> Bplane line #160
	tst.b	(a0)
	bne.b	.done			;if already in mode, then return

	move.l	#$ffffffff,d0
	move.w	#32*40/4-1,d1		;change 32 lines in text window...
.fill	move.l	d0,(a0)+
	dbf	d1,.fill
.done	rts

************************************************************************	

	SECTION	TABLES,DATA	
	CNOP	0,4

;BPlaneTbl1	dc.l	Gr1_Planes,Txt1_Plane1ND,Gr1_Planes,Txt1_Plane1ND
;		dc.l	Gr2_Planes,Txt2_Planes,Gr2_Planes,Txt2_Planes
;		dc.l	Hgr1_Planes,Txt1_Plane1ND,Hgr1_Planes,Txt1_Plane1ND
;		dc.l	Hgr2_Planes,Txt2_Planes,Hgr2_Planes,Txt2_Planes

;BPlaneTbl2	dc.l	0,Txt1_Plane2ND,0,Txt1_Plane2ND
;		dc.l	0,0,0,0
;		dc.l	0,Txt1_Plane2ND,0,Txt1_Plane2ND
;		dc.l	0,0,0,0

ModeColorTbl	dc.l	GrColorTable,TxtColorTable,GrColorTable,TxtColorTable
		dc.l	GrColorTable,TxtColorTable,GrColorTable,TxtColorTable
		dc.l	HiResColorTable,TxtColorTable,HiResColorTable,TxtColorTable
		dc.l	HiResColorTable,TxtColorTable,HiResColorTable,TxtColorTable

************************************************************************	
         
Variables
VidMode	ds.b	1	;bits showing apple video mode!

	CNOP	0,4
InstJmpTblBkUp
	dc.l	UND00,STA81,UND00,UND00,STY84,STA85,STX86,UND00
	dc.l	DEY88,UND00,TXA8A,UND00,STY8C,STA8D,STX8E,UND00
	dc.l	BCC90,STA91,UND00,UND00,STY94,STA95,STX96,UND00
	dc.l	TYA98,STA99,TXS9A,UND00,UND00,STA9D,UND00,UND00
	dc.l	LDYA0,LDAA1,LDXA2,UND00,LDYA4,LDAA5,LDXA6,UND00
	dc.l	TAYA8,LDAA9,TAXAA,UND00,LDYAC,LDAAD,LDXAE,UND00
	dc.l	BCSB0,LDAB1,UND00,UND00,LDYB4,LDAB5,LDXB6,UND00
	dc.l	CLVB8,LDAB9,TSXBA,UND00,LDYBC,LDABD,LDXBE,UND00
	dc.l	CPYC0,CMPC1,UND00,UND00,CPYC4,CMPC5,DECC6,UND00
	dc.l	INYC8,CMPC9,DEXCA,UND00,CPYCC,CMPCD,DECCE,UND00
	dc.l	BNED0Reg,CMPD1,UND00,UND00,UND00,CMPD5,DECD6,UND00
	dc.l	CLDD8,CMPD9,UND00,UND00,UND00,CMPDD,DECDE,UND00
	dc.l	CPXE0,SBCE1,UND00,UND00,CPXE4,SBCE5,INCE6,UND00
	dc.l	INXE8,SBCE9,NOPEA,UND00,CPXEC,SBCED,INCEE,UND00
	dc.l	BEQF0Reg,SBCF1,UND00,UND00,UND00,SBCF5,INCF6,UND00
	dc.l	SEDF8,SBCF9,UND00,UND00,UND00,SBCFD,INCFE,UND00

	dc.l	BRK00,ORA01,UND00,UND00,UND00,ORA05,ASL06,UND00
	dc.l	PHP08,ORA09,ASL0A,UND00,UND00,ORA0D,ASL0E,UND00
	dc.l	BPL10Reg,ORA11,UND00,UND00,UND00,ORA15,ASL16,UND00
	dc.l	CLC18,ORA19,UND00,UND00,UND00,ORA1D,ASL1E,UND00
	dc.l	JSR20,AND21,UND00,UND00,BIT24,AND25,ROL26,UND00
	dc.l	PLP28,AND29,ROL2A,UND00,BIT2C,AND2D,ROL2E,UND00
	dc.l	BMI30Reg,AND31,UND00,UND00,UND00,AND35,ROL36,UND00
	dc.l	SEC38,AND39,UND00,UND00,UND00,AND3D,ROL3E,UND00
	dc.l	RTI40,EOR41,UND00,UND00,UND00,EOR45,LSR46,UND00
	dc.l	PHA48,EOR49,LSR4A,UND00,JMP4CReg,EOR4D,LSR4E,UND00
	dc.l	BVC50,EOR51,UND00,UND00,UND00,EOR55,LSR56,UND00
	dc.l	CLI58,EOR59,UND00,UND00,UND00,EOR5D,LSR5E,UND00
	dc.l	RTS60,ADC61,UND00,UND00,UND00,ADC65,ROR66,UND00
	dc.l	PLA68,ADC69,ROR6A,UND00,JMP6C,ADC6D,ROR6E,UND00	
	dc.l	BVS70,ADC71,UND00,UND00,UND00,ADC75,ROR76,UND00
	dc.l	SEI78,ADC79,UND00,UND00,UND00,ADC7D,ROR7E,UND00		;end of POSitives...
	dc.l	0,0,0,0

PBPageList:		;$00-$bf - nothing; $c0-$cf hardware; $d0-$ff rom...
	dc.l	00,00,00,00			;$00-$03 - nothing
	dc.l	PBTxt1,PBTxt1,PBTxt1,PBTxt1	;$04-$07 - Text1
	dc.l	PBTxt2,PBTxt2,PBTxt2,PBTxt2	;$08-$0b - Text2
	dc.l    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ;$0c-$1f - nothing
	dc.l	PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1
	dc.l	PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1
	dc.l	PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1
	dc.l	PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1,PBHgr1 ;$20-$3f - HGR1
;	dc.l	PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick
;	dc.l	PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick
;	dc.l	PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick
;	dc.l	PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick,PBHgr1Quick ;$20-$3f - HGR1;
	dc.l	PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2
	dc.l	PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2
	dc.l	PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2
	dc.l	PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2,PBHgr2 ;$40-$5f - HGR2
;	dc.l	PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick
;	dc.l	PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick
;	dc.l	PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick
;	dc.l	PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick,PBHgr2Quick ;$20-$3f - HGR1
	dcb.l	$60,0				;$60-$BF nothing!
	dc.l	PBHardWare
	dc.l	00,PBSlot,00,PBSlot,PBSlot,PBSlot,PBSlot
	dc.l	PBSlot,PBSlot,PBSlot,PBSlot,PBSlot,PBSlot,PBSlot,PBSlot	;$c1-$cf - SLOTS
	dc.l	PBBank2,PBBank2,PBBank2,PBBank2,PBBank2,PBBank2,PBBank2,PBBank2
	dc.l	PBBank2,PBBank2,PBBank2,PBBank2,PBBank2,PBBank2,PBBank2,PBBank2
	dc.l	PBBank,PBBank,PBBank,PBBank,PBBank,PBBank,PBBank,PBBank
	dc.l	PBBank,PBBank,PBBank,PBBank,PBBank,PBBank,PBBank,PBBank
	dc.l	PBBank,PBBank,PBBank,PBBank,PBBank,PBBank,PBBank,PBBank
	dc.l	PBBank,PBBank,PBBank,PBBank,PBBank,PBBank,PBBank,PBBank
	dc.b	~'K',~'E',~'V',~'I',~'N',~'K',~'R',~'A',~'L',~'I',~'A',~'N'

	CNOP	0,4


FreshBootMemory:	;a snapshot of low mem upon a clean power-up of Apple II
   		;Zero Page
	dc.b	$4C,$3C,$D4,$4C,$3A,$DB,$00,$00,$00,$00,$4C,$99,$E1,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$28,$00,$18,$01,$17,$00,$08,$D0,$07,$D0,$07,$00,$00,$00,$00
	dc.b	$00,$00,$FF,$DD,$00,$D4,$F0,$FD,$1B,$FD,$00,$00,$7F,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$38,$B6
	dc.b	$00,$C2,$55,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$01,$08,$03,$08,$03,$08,$03,$08,$00
	dc.b	$C0,$00,$00,$00,$C0,$00,$FF,$00,$00,$00,$00,$00,$00,$00,$08,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$03
	dc.b	$4C,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$03
	dc.b	$08,$E6,$B8,$D0,$02,$E6,$B9,$AD,$00,$08,$C9,$3A,$B0,$0A,$C9,$20
	dc.b	$F0,$EF,$38,$E9,$30,$38,$E9,$D0,$60,$80,$4F,$C7,$52,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

		;Page 1 (stack), page 2 (keybuffer)
	dcb.b	$100,0
	dcb.b	$100,0

		;Page 3 (some system vectors)
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00
	dc.b	$02,$03,$00,$04,$05,$06,$00,$00,$00,$00,$00,$00,$07,$08,$00,$00
	dc.b	$00,$09,$0A,$0B,$0C,$0D,$00,$00,$0E,$0F,$10,$11,$12,$13,$00,$14
	dc.b	$15,$16,$17,$18,$19,$1A,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$1B,$00,$1C,$1D,$1E,$00,$00,$00,$1F,$00,$00,$20,$21,$00,$22
	dc.b	$23,$24,$25,$26,$27,$28,$00,$00,$00,$00,$00,$29,$2A,$2B,$00,$2C
	dc.b	$2D,$2E,$2F,$30,$31,$32,$00,$00,$33,$34,$35,$36,$37,$38,$00,$39
	dc.b	$3A,$3B,$3C,$3D,$3E,$3F,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	dc.b	$59,$FA,$03,$E0,$45,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

		;Page 4,5,6,7 (text- all white space!)
	dcb.b	$400,$a0

