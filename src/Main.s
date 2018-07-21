
DEBUG	EQU	1		;0 or 1 to control "DEBUG" option, Diagnostic output, etc
	OPT	P=68020,USER

	IFNE	DEBUG
	OPT	DEBUG
	ELSE
	OPT	NODEBUG
	ENDC	
		
	include	system
	include libraries/dos_lib.i
	include libraries/dos.i
	include exec/exec_lib.i
	include	exec/interrupts.i
	include	exec/execbase.i
	include graphics/graphics_lib.i
	include graphics/gfxbase.i
	include	intuition/intuition_lib.i
	include	hardware/custom.i
	include	hardware/intbits.i
	include hardware/dmabits.i
	include	devices/console.i		;just for "RawKeyConvert" fnctn
	include	devices/console_lib.i
	include	devices/inputevent.i
	include resources/potgo.i		;for PotBits stuff...
	include resources/potgo_lib.i
	include	hardware/cia.i			;all for CIA stuff
	include	resources/cia.i
	include resources/cia_lib.i
	include	ReqTools/reqtools_lib.i		;for reqTools!!!!
	include	ReqTools/reqtools.i

CALLREQ	MACRO
	move.l	_REQBase,a6
	jsr	_LVO\1(a6)
	ENDM

*-------------------------------------------------------------------------*	
	SECTION	APPLEII,CODE

	jmp	OpenAll				;returns to "Main"
	include	"OpenClose.s"			;opens all, Exits too!
	include	"Decompress.s"			;does DDD & Plain disk loading
	include "Compress.s"			;does DDD & Plain disk saving
	include	"AppleII.s"			;does 6502/hardware/etc !

	IFNE	DEBUG
	include	"Debug.s"			;Debugging/Diagnostic help
	ELSE
	include	"NoDebug.s"
	ENDC	

	include "CharSets.s"			;has bitmap character sets
	
_SysBase	equ	4
Hardware	equ	$dff000

*-------------------------------------------------------------------------*	
	SECTION	APPLEII,CODE
	CNOP	0,4

Main:	
Wait	move.l	([MyWindow.l],wd_UserPort),a0
	CALLEXEC WaitPort
	
	move.l	([MyWindow.l],wd_UserPort),a0
	CALLEXEC GetMsg				;get Intui-Message
	tst.l	d0				;message arrive?
	beq	Wait				;nope, wait some more!
	move.l	d0,a1
	move.l	im_Class(a1),d2
	move.w	im_Code(a1),d3
	move.w	im_Qualifier(a1),d4
	move.l	im_IAddress(a1),d5
	CALLEXEC ReplyMsg
						;d2.l= im_Class
						;d3.w= im_Code
						;d4.w= qualifier
						;d5.l = IAddress
	cmp.l	#RAWKEY,d2
	beq	key
	cmp.l	#MOUSEBUTTONS,d2
	beq	button
	cmp.l	#INTUITICKS,d2
	bne.b	Wait

	tst.l	NewStatusMsgPtr			;Handle StatusMsg's.. New one present?
	beq.b	.NoNew
	jsr	DrawNewStatusMsg

.NoNew	tst.b	StatusCountdown			;and remove msg in timely manner...
	beq.b	.NewVid
	subq.b	#1,StatusCountdown
	bne.b	.NewVid
	jsr	ClearStatusMsg
	
.NewVid	move.l	DoVideoFlag,d2
	beq.b	.skip

	clr.l	DoVideoFlag
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,d5

	move.l	MyScreen,a0			;Rebuild local VPort copper list (but not display)
	CALLINT	MakeScreen
	CALLINT	RethinkDisplay			;and update screen!

	cmp.l	LastColorTable,d2
	beq.b	.sameColor

	move.l	d2,a1
	move.l	MyScreen,a0			;Set Colors... (IF NEEDED!)
	move.l	a1,LastColorTable
	lea	sc_ViewPort(a0),a0		;a0 -> Screen ViewPort
	move.w	(a1)+,d0			;d0 = # of colors, a1 -> ColorList
	CALLGRAF LoadRGB4			;(takes effect immed, but doesn't ReThink display)

.sameColor
	move.l	d5,a0
	CALLINT	UnlockIBase			;(is locking really needed?)

.skip	tst.b	FlashEnableB			;see if flashing is even enabled 1st...
	beq	Wait

	addq.l	#1,TickCount
	cmp.l	#3,TickCount
	bne	Wait
	move.l	#0,TickCount
.flash
	lea	Flash1ColorTbl,a1
	eor.b	#1,Toggle
	beq	.alt
	lea	Flash2ColorTbl,a1
	
.alt	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,d5
	
	move.l	MyScreen,a0			;SET COLORS...
	lea	sc_ViewPort(a0),a0		;a0 -> Screen ViewPort
	move.w	(a1)+,d0			;d0 = # of colors, a1 -> ColorList
	CALLGRAF LoadRGB4		;(takes effect immed, but doesn't ReThink display)

	move.l	d5,a0
	CALLINT	UnlockIBase			;(is locking really needed?)

	bra	Wait

	CNOP	0,4
TickCount	dc.l	0
FlashEnableB	dc.b	1
Toggle		dc.b	0

	CNOP	0,4
*-------------------------------------------------------------------------*	
FlushMsgs:
.flush	move.l	([MyWindow.l],wd_UserPort),a0
	CALLEXEC GetMsg				;get any Intui-Message
	tst.l	d0				;message arrive?
	beq	.done				;nope, wait some more!
	move.l	d0,a1
	CALLEXEC ReplyMsg			;we dont care what it is, just reply...
	bra.b	.flush	
.done	rts
*-------------------------------------------------------------------------*	
	CNOP	0,4
KillApple:					;***** QUIT PROGRAM!!!! *****
	bsr	PauseCPU			;stop 6502 task, clear & show MyScreen
	bsr	ShowMainScreen
	
	moveq.l	#0,d0				;check & warn if drive #1 changed
	bsr	CheckDiskAndWarn
	tst.w	d0
	beq.b	.NoKill	

	moveq.l	#1,d0				;check & warn if drive #2 changed (if present)
	tst.l	disk_Buffer2
	beq.b	.NoD2
	bsr	CheckDiskAndWarn
	tst.w	d0
	beq.b	.NoKill	

.NoD2	move.l	MyWindow,.win

	lea	.QuitMsg,a1
	lea	.QuitAns,a2			; "are you sure?" req
	move.l	EasyReq,a3
	move.l	#0,a4
	lea	.QuitTag,a0
	CALLREQ	rtEZRequestA
	
	tst.l	d0
	beq	.NoKill				; nope! don't quit!
	
.Kill	move.l	MySubTask,a1			;6502 paused- Don't respond, Remove it!
	CALLEXEC RemTask		
	jsr	FadeEffect
	jmp	exit				;instead of NiceExit!!!!!

.NoKill	jsr	FlushMsgs
	bsr	ResumeCPU			;resume 6502 task, restore pointer & video...

	bra	Wait

.QuitMsg	dc.b	"Really Quit?",0
.QuitAns	dc.b	"_Yes|_No",0
.QuitTag	dc.l	RT_Underscore,'_',RT_ReqPos,REQPOS_TOPLEFTSCR
		dc.l	RTEZ_Flags,EZREQF_NORETURNKEY,RT_Window
.win		dc.l	0	;<-- patch in window before using taglist...
		dc.l	RT_LockWindow,1,RTEZ_ReqTitle,.QuitTtl,TAG_DONE
.QuitTtl	dc.b	"Quit?",0
*-------------------------------------------------------------------------*	
	CNOP	0,4
			;----Here for RAWKEY event only!!!------
key:	btst.l	#IEQUALIFIERB_LCOMMAND,d4		;LeftAmiga pressed? Ignore...
	bne	Wait

	lea	.KyTbl,a0				;Look up key, jump to handler
	move.l	(a0,d3.l*4),d0
	beq	.NotHardKey
	move.l	d0,a0
	jmp	(a0)	

		;*** These Raw keycodes are from page 661, RKM Libs&Devs ***
		;* Only handle special 'hard' keys here. Others get parsed by con dev
.KyTbl	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$00-$0f
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00					;$10
	dc.l	$00,$00,$00,$00,$00,$00,.Num2,$00				;$18-$1f
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00					;$20
	dc.l	$00,$00,$00,$00,$00,.Num4,.Num5,.Num6				;$28
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00					;$30
	dc.l	$00,$00,$00,$00,$00,$00,.Num8,$00				;$38
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00					;$40
	dc.l	$00,$00,$00,$00,.Up,.Down,.Right,.Left				;$48
	dc.l	.F1,.F2,.F3,.F4,.F5,$00,$00,.F8					;$50
	dc.l	.F9,.F10,$00,$00,$00,$00,$00,.Help				;$58
	dc.l	$00,$00,$00,$00,.LAltDn,.RAltDn,$00,$00				;$60-$67
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00					;$68-$6f
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$70

	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$80
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$90
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$a0
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$b0
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$c0
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$d0
	dc.l	$00,$00,$00,$00,.LAltUp,.RAltUp,$00,$00				;$e0-$e7
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00					;$e8-$ef
	dc.l	$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;$f0


.LAltDn						;Left-Alt down: set button 0
	move.b	#$ff,([Mem_PtrVar.l],$c061.l)		
	bra	Wait
.LAltUp						;L-Alt up: clear button 0
	move.b	#$00,([Mem_PtrVar.l],$c061.l)
	bra	Wait
.RAltDn						;R-Alt down: set button 1
	move.b	#$ff,([Mem_PtrVar.l],$c062.l)
	bra	Wait
.RAltUp						;R-Alt up: clear button 1
	move.b	#$00,([Mem_PtrVar.l],$c062.l)
	bra	Wait
.Left						;Left Arrow: store as ctrl-h
	move.b	#8+128,([Mem_PtrVar.l],$c000.l)	
	bra	Wait
.Right						;Right Arrow: store as ctrl-u
	move.b	#21+128,([Mem_PtrVar.l],$c000.l)
	bra	Wait
.Up						;Up Arrow: store as ctrl-k
	move.b	#11+128,([Mem_PtrVar.l],$c000.l)
	bra	Wait
.Down						;Down Arrow: store as ctrl-j
	move.b	#10+128,([Mem_PtrVar.l],$c000.l)
	bra	Wait

						;*** Joystick Trimming controls ***
.Num4	sub.w	#11,Pdl0CenterW			;NumPad 4  <--
	bpl.b	.Num4ok
	move.w	#2805,Pdl0CenterW
.Num4ok	bra	Wait

.Num6	add.w	#11,Pdl0CenterW			;NumPad 6  -->
	cmp.w	#2805,Pdl0CenterW
	bls.b	.Num6ok
	move.w	#0,Pdl0CenterW
.Num6ok	bra	Wait

.Num8	sub.w	#11,Pdl1CenterW			;NumPad 8  ^
	bpl.b	.Num8ok
	move.w	#2805,Pdl1CenterW
.Num8ok	bra	Wait

.Num2	add.w	#11,Pdl1CenterW			;NumPad 2  V
	cmp.w	#2805,Pdl1CenterW
	bls.b	.Num2ok
	move.w	#0,Pdl1CenterW
.Num2ok	bra	Wait

.Num5	move.w	#127*11,Pdl0CenterW		;NumPad 5  (Center)
	move.w	#127*11,Pdl1CenterW
	bra	Wait

						;*** Set regulation speed ***
.F1	move.l	#50,d3				;F1 key: Set speed regulation to 50%
	lea	.Speed50Msg,a3
	bra	.F1to5

.F2	move.l	#100,d3				;F2 key: Set speed regulation to 100%
	lea	.Speed100Msg,a3
	bra	.F1to5

.F3	move.l	#150,d3				;F3 key: Set speed regulation to 150%
	lea	.Speed150Msg,a3
	bra	.F1to5

.F4	move.l	#200,d3				;F4 key: Set to 200%
	lea	.Speed200Msg,a3
	bra	.F1to5

.F5	move.l	#-1,d3				;F5 key: Turn off speed regulation
	lea	.SpeedNoneMsg,a3
	bra	.F1to5

.F1to5	bsr	PauseCPU			;enter w/ d3=new speed, a3->NewMsg
	move.l	d3,ResumeWithNewSpeed
	bsr	ResumeCPU
	bsr	FlushMsgs
	move.l	a3,NewStatusMsgPtr
	bra	Wait

.Speed50Msg	dc.b	"      SPEED LIMIT:  50% (0.5 MHZ)",0,25
.Speed100Msg	dc.b	"      SPEED LIMIT: 100% (1.0 MHZ)",0,25
.Speed150Msg	dc.b	"      SPEED LIMIT: 150% (1.5 MHZ)",0,25
.Speed200Msg	dc.b	"      SPEED LIMIT: 200% (2.0 MHZ)",0,25
.SpeedNoneMsg	dc.b	"      LUDICROUS SPEED! (NO LIMIT)",0,25
;		dc.b	"1234567890123456789012345678901234567890"
	CNOP	0,4

.F10						;F10 key: Do Diagnostic print!
	jsr	PauseCPU			;sometime, make Pause w/o screen changes
	move.l	#1,ResumeWithDiagnostic
	jsr	ResumeCPU
	jsr	FlushMsgs
	bra	Wait

.F9						;F9 key: Toggle Joystk/Mouse controls!
	move.l	PotBits,d0			;free any previously allocated PotBits...
	CALLPOTGO FreePotBits
	clr.l	PotBits

	lea	HardwareWriteTbl,a0		;Cycle through Joy/Mouse/Analog
	move.l	$70*4(a0),a1	
	cmpa.l	#HW_C070_Joystick,a1
	beq.b	.wasJoy
	cmpa.l	#HW_C070_Mouse,a1
	beq.b	.wasMouse
	cmpa.l	#HW_C070_AtariPdl,a1
	beq	.wasAtariPdl
	bra	.wasAnalog


.wasJoy						;Was Joystick, now make it mouse!
	move.l	#HW_C070_Mouse,$70*4(a0)	;set to mouse control...
	lea	HardwareReadTbl,a0
	move.l	#HW_C070_Mouse,$70*4(a0)

	move.l	#.MouseMsg,NewStatusMsgPtr
	bra	Wait

.wasMouse
	move.l	#HW_C070_Analog,$70*4(a0)	;set to Analog control...
	lea	HardwareReadTbl,a0
	move.l	#HW_C070_Analog,$70*4(a0)
	move.l	#HWr_C061_Analog,$61*4(a0)	;and buttons...
	move.l	#HWr_C062_Analog,$62*4(a0)

	move.l	#%1111000000000001,d0		;allocate 2 prop bits & Start bit...
	CALLPOTGO AllocPotBits
	move.l	d0,PotBits

	move.l	#%0101000000000000,d0		;bits to set
	move.l	PotBits,d1			;mask
	CALLPOTGO WritePotgo			;set output lines hi to read joy 2nd btn...

	move.w	#$01fe,PotPosition

.10	cmp.w	#$01fe,PotPosition		;wait until intrpt server is running
	beq.b	.10
	move.w	#$01fe,PotPosition
.20	cmp.w	#$01fe,PotPosition
	beq.b	.20

	move.w	PotPosition,d0
	and.w	#%1111111011111110,d0		;Ensure each val is even and <= $FE

	move.b	d0,PotMinY
	addq.b	#1,d0
	move.b	d0,PotMaxY
	
	lsr.w	#8,d0
	move.b	d0,PotMinX
	addq.b	#1,d0
	move.b	d0,PotMaxX

	move.b	#0,PdlMode

	move.l	#.AnalogMsg,NewStatusMsgPtr
	bra	Wait

.wasAnalog					;set to Atari Paddles... Only change
						;from Analog Joystick is a flag setting.

	move.l	#HW_C070_AtariPdl,$70*4(a0)	;set to Atari Pdl control...
	lea	HardwareReadTbl,a0
	move.l	#HW_C070_AtariPdl,$70*4(a0)
	move.l	#HWr_C061_Analog,$61*4(a0)	;and buttons...
	move.l	#HWr_C062_Analog,$62*4(a0)

	move.l	#%1111000000000001,d0		;allocate 2 prop bits & Start bit...
	CALLPOTGO AllocPotBits
	move.l	d0,PotBits

	move.l	#%0101000000000000,d0		;bits to set
	move.l	PotBits,d1			;mask
	CALLPOTGO WritePotgo			;set output lines hi to read joy 2nd btn...


	move.w	#$01fe,PotPosition

.51	cmp.w	#$01fe,PotPosition		;wait until intrpt server is running
	beq.b	.51
	move.w	#$01fe,PotPosition
.52	cmp.w	#$01fe,PotPosition
	beq.b	.52

	move.w	PotPosition,d0
	and.w	#%1111111011111110,d0		;Ensure each val is even and <= $FE

	move.b	d0,PotMinY
	addq.b	#1,d0
	move.b	d0,PotMaxY
	
	lsr.w	#8,d0
	move.b	d0,PotMinX
	addq.b	#1,d0
	move.b	d0,PotMaxX

	move.b	#1,PdlMode

	move.l	#.AtariPdlMsg,NewStatusMsgPtr
	bra	Wait


.wasAtariPdl
	move.l	#HW_C070_Joystick,$70*4(a0)	;set to joystick control...
	lea	HardwareReadTbl,a0
	move.l	#HW_C070_Joystick,$70*4(a0)
	move.l	#HWr_C061,$61*4(a0)		;and buttons...
	move.l	#HWr_C062,$62*4(a0)

	move.l	#$c000,d0			;we want bits #14 & bit #15... (DATRY & OUTRY)
	CALLPOTGO AllocPotBits
	move.l	d0,PotBits

	move.l	#$c000,d0			;bits to set
	move.l	PotBits,d1			;mask
	CALLPOTGO WritePotgo			;set output lines hi to read joy 2nd btn...

	move.l	#.JoyMsg,NewStatusMsgPtr
	bra	Wait

.JoyMsg		dc.b	"        ---==== JOYSTICK ====---        ",0,25
.MouseMsg	dc.b	"         ---==== MOUSE ====---          ",0,25
.AnalogMsg	dc.b	"    ---==== ANALOG JOYSTICK ====---     ",0,25
.AtariPdlMsg	dc.b	"     ---==== ATARI PADDLES ====---      ",0,25
		dc.b	"1234567890123456789012345678901234567890"
	CNOP	0,4

	bra	Wait

.F8						;F8 key? Print Contents of 0 page...
;	jsr	PauseCPU			;sometime, make Pause w/o screen changes

;	move.l	Mem_PtrVar,a0			;Print memory...
;	move.l	#$c600,d1
;.pg0lp	move.b	(a0,d1.l),d0
;	jsr	DB_HexB
;	addq.l	#1,d1
;	cmp.l	#$c700,d1
;	blo	.pg0lp

;	move.l	Mem_PtrVar,a0			;Search memory & print finds...
;	move.l	#$0300,d1
;.srchlp	cmp.w	#$d0fd,(a0,d1.l)
;	bne	.nope		
;	move.w	d1,d0
;	jsr	DB_HexW
;.nope	addq.w	#1,d1
;	bne	.srchlp	


;	move.b	#$ff,([Mem_PtrVar.l],$fca8.l)	;patch rom for experiment...
				
;	jsr	ResumeCPU

;	jsr	Hgr1Refresh

	jsr	FlushMsgs
	bra	Wait

;--------- 


.Help						;<Help> key: Toggle through msgs... 

	move.l	.NextHelpMsg,a0
	move.l	(a0)+,.NextHelpMsg		;setup next message for next time...

	move.l	a0,NewStatusMsgPtr		;and load status message...

	bra	Wait

.NextHelpMsg	dc.l	.Help1			;index to help message...

				;Help_ has ptr to next sequential message & message...
.Help1		dc.l	.Help2
		dc.b	"HELP: @L- LOAD, @S- SAVE, @Q- QUIT...",0,40
.Help2		dc.l	.Help3
		dc.b	"HELP: DEL = RESET, F9 = SET CONTROLS...",0,40
.Help3		dc.l	.Help4
		dc.b	"HELP: F1 -> F5 = SPEED REGULATION...",0,40
.Help4		dc.l	.Help1
		dc.b	"HELP: HELP = HELP (READ THE DOCS! -KK)",0,40
;		dc.b	"1234567890123456789012345678901234567890"
	CNOP	0,4
	


;-----------
					;d2.l= im_Class
					;d3.w= im_Code
					;d4.w= qualifier
					;d5.l = IAddress
.NotHardKey
1$	and.w	#~(IEQUALIFIER_LALT!IEQUALIFIER_RALT),d4		;mask out ALT's
	lea	InputStrct,a0
	move.w	d3,ie_Code(a0)
	tst.b	d3
	bmi	Wait				;key release? ignore!
	move.w	d4,ie_Qualifier(a0)
	move.l	d5,ie_EventAddress(a0)	
	lea	KeyBuffer,a1
	move.l	#80,d1				;buffer len
	move.l	#0,a2				;keymap (0=default)
	move.l	_ConBase,a6
	jsr	_LVORawKeyConvert(a6)
	cmp.l	#1,d0
	bne	Wait				;not a 1 byte simple answer? Ignore it!	

	move.b	KeyBuffer,d3			;new plain ascii keycode in d3!

	btst.l	#IEQUALIFIERB_RCOMMAND,d4	;right amiga?
	bne	.ComKey				;yes!
	cmp.b	#$7f,d3				;KEYBOARD! is it "DEL"?
	beq	.reset

	cmp.b	#'a',d3
	blo.b	.notLo
	cmp.b	#'z',d3
	bhi.b	.notLo
	eor.b	#'a'^'A',d3			;FORCE TO UPPER CASE... (1 bit change)
.notLo	add.b	#$80,d3				;no, put it in $c000 to read later
	move.b	d3,([Mem_PtrVar.l],$c000.l)	;place key...
	bra	Wait

.ComKey	bclr	#5,d3				;force all keys to lower case
	cmp.b	#'Q',d3				;R-Amiga q ?
	beq	KillApple			;Yes, go Quit!
	cmp.b	#'L',d3
	beq	LoadReq
	cmp.b	#'S',d3
	beq	SaveReq

	bra	Wait

.reset	btst.l	#IEQUALIFIERB_CONTROL,d4	;is it CTRL-HELP?
	beq	.CauseReset

.CauseReboot:
	move.w	#00,([Mem_PtrVar.l],$3f2.w)	;blank out soft reset vector...
	move.b	#00,([Mem_PtrVar.l],$3f4.w)	;and checksum byte... WILL REBOOT!
.CauseReset:
	CALLEXEC Forbid
	move.l	#RESET_INST,a0			;Next 6502 inst will be RESET_INST
	move.l	InstTbl_Var,a1			;array of 65536 longs...
	move.w	#65535,d1
.lp	move.l	a0,(a1)+
	dbf	d1,.lp
	CALLEXEC Permit				;done! Will continue at Reset_Vector!!!
	bra	Wait


*-------------------------------------------------------------------------*	
	CNOP	0,4
button:	cmp.w	#SELECTDOWN,d3			;Left mousebutton down? UPDATE button 0 status...
	bne.b	2$
	move.b	#$ff,([Mem_PtrVar.l],$c061.l)	;set button 0
	bra	Wait

2$	cmp.w	#SELECTUP,d3			;Left MB up?
	bne.b	3$
	move.b	#$00,([Mem_PtrVar.l],$c061.l)
	bra	Wait

3$	cmp.w	#MENUDOWN,d3			;Right MB down?
	bne.b	4$
	move.b	#$ff,([Mem_PtrVar.l],$c062.l)
	bra	Wait
	
4$	cmp.w	#MENUUP,d3			;Right MB up?
	bne.b	5$
	move.b	#$00,([Mem_PtrVar.l],$c062.l)

5$	bra	Wait

*-------------------------------------------------------------------------*	
* LOADREQ is the entire function to put up a File Requester, attempt to load
* the file, identify the type of file (or report the error), & jump to the
* beginning of it (run it) if its an executable/snapshot...

LoadReq:
	bsr	PauseCPU			;stop 6502 task, clear & show MyScreen
	bsr	ShowMainScreen
	
	move.l	MyWindow,.win
	move.l	MyWindow,.win4
 	move.l	MyWindow,.win12

	lea	.LoadTag,a0
	move.l	FileReq,a1
	lea	.TempLoadFileNm,a2
	lea	.LoadTtl,a3			; "Load A File" req
	CALLREQ	rtFileRequestA

	tst.l	d0
	beq	.DoneLd				;cancel
		
.build	move.l	FileReq,a0			;Build string w/ full DOS path * filename
	move.l	rtfi_Dir(a0),a0			;* path string
	lea	.FullPath,a1			;dest buffer

	tst.b	(a0)
	beq.b	.bldFl				;No path info, use current dir...

.lp1	move.b	(a0)+,(a1)+
	bne	.lp1
	lea	-1(a1),a1			;back over NULL (pts at that byte now)
	cmp.b	#':',-1(a1)			;end with a   :  ?
	beq.b	.bldFl
	move.b	#'/',(a1)+			;No, tag a trailing / in dest...

.bldFl	lea	.TempLoadFileNm,a0
.lp2	move.b	(a0)+,(a1)+
	bne	.lp2
	move.b	-2(a0),.LastChar		;Keep last char (to check for ddd type)
			
	lea	.FullPath,a0
	jsr	DB_String
;---------------
.load	move.l	#.FullPath,d1			;Load parsed File [arg] name
	move.l	#MODE_OLDFILE,d2
	CALLDOS	Open
	move.l	d0,d4	;temp
	beq	.Err
			
	move.l	d4,d1				;Read entire file specified... (up to 64k)
	move.l	InstTbl_Var,d2			;read temporarily into 256k area "InstTbl"
	move.l	#262140,d3			;256k max read! (should never be that big!)
	CALLDOS	Read				;d0 = # of bytes read
	
	move.l	d0,d6				;d6 = # of bytes read
	
	move.l	d4,d1				;Close file
	CALLDOS Close

*...............................

.PlnImg	cmp.l	#143360,d6			;Check if = # of bytes in "plain" disk image
	beq.b	.IsDisk

	cmp.b	#'>',.LastChar			;Is it a DDD filename (ends in '>' char?)
	bne	.CheckProDosExe

.IsDisk						;Is a disk image! Determine type by D6 (size)
	moveq.l	#0,d0				;default to drive 1
	
	tst.l	disk_Buffer2			;unless 2 drives present...
	beq.b	.DriveSelected

	lea	.Load12Msg,a1			; "Which drive?" req
	lea	.Load12Ans,a2
	move.l	EasyReq,a3
	move.l	#0,a4
	lea	.Load12Tag,a0
	CALLREQ	rtEZRequestA
	tst.l	d0
	beq	.DoneLd				;Cancel

	subq.l	#1,d0				;Else d0 = drive # (0 or 1)

.DriveSelected						;d0 = drive # (0 or 1) !!!
	move.l	(disk_Buffer.l,d0.w*4),Dest_Disk_Buffer	;Set proper destination...
	move.l	d0,d2

	bsr	CheckDiskAndWarn		;check & warn if old drive data changed
	tst.w	d0
	beq	.DoneLd

	clr.b	(disk_Changed.l,d2.w)		;and mark new drive as being UNchanged

	lea	.TempLoadFileNm,a0		;And Keep filename for proper drive...
	move.l	(LoadSaveNmPtrs.l,d2.w*4),a1
	move.w	#120/4-1,d0
.KeepNm	move.l	(a0)+,(a1)+
	dbf	d0,.KeepNm	

	cmp.l	#143360,d6			;Check if = # of bytes in "plain" disk image
	bne.b	.DDD

.Plain	jsr	PlainDiskImage_Load
	bra	.boot
.DDD	jsr	DecompressDisk			;THEN Decompress DDD disk...
	bra	.boot

;-----------------------------

.boot	move.l	disk_Buffer,d0
	cmp.l	Dest_Disk_Buffer,d0		;did we load drive #1?
	bne	.DoneLd

	lea	.BootMsg,a1			;Yes- "Want to Boot disk?" req
	lea	.BootAns,a2
	move.l	EasyReq,a3
	move.l	#0,a4
	lea	.BootTag,a0
	CALLREQ	rtEZRequestA
	
	move.l	d0,RebootApple			;Set flag in "STOP_INST" to reboot...
	bra	.DoneLd

.CheckProDosExe	
	move.l	InstTbl_Var,a3			;IS THIS A PRODOS FILE?
	cmp.w	#$0a47,(a3)			;first 2 bytes in ProDos header...
	bne	.CheckDosExe			;No??? Maybe DOS 3.3?
	cmp.b	#$4c,2(a3)			;Third byte in ProDos header...
	bne	.CheckDosExe
	bra	.ProDos

.CheckDosExe	
	moveq.l	#0,d4				;Dos 3.3 test...
	moveq.l	#0,d5				;assume dos 3.3 unless out of range err...	
	move.w	(a3),d4
	ror.w	#8,d4
	move.w	2(a3),d5
	ror.w	#8,d5

	move.l	d4,d0
	cmp.l	#$c000,d0
	bhs	.Err
	jsr	DB_HexL
	move.l	d5,d0
	jsr	DB_HexL
	move.l	d4,d0
	add.l	d5,d0
	cmp.l	#$c000,d0
	bhs	.Err

	move.l	d4,ResumePCount			;<------ to immediately execute!!!

	lea	FreshBootMemory,a0		;1st reset memory as a "fresh" powerup
	move.l	Mem_PtrVar,a1
	move.w	#$07ff,d0
.frsh	move.b	(a0,d0.w),(a1,d0.w)
	dbf	d0,.frsh	

	move.l	InstTbl_Var,a0
	add.l	#$4,a0				;data source...
	move.l	Mem_PtrVar,a1
	add.l	d4,a1				;data dest...
	subq.w	#1,d5
.mlp2	move.b	(a0)+,(a1)+
	dbf	d5,.mlp2			;Now in Apple Memory!!!
	bra	.DoneLd
	
.ProDos	moveq.l	#0,d4
	moveq.l	#0,d5

	move.w	$5(a3),d4
	ror.w	#8,d4				;starting addr in d4...

	move.w	$14(a3),d5
	ror.w	#8,d5				;length in d5

	move.l	d4,d0
	cmp.l	#$c000,d0
	bhs	.Err
	jsr	DB_HexL
	move.l	d5,d0
	jsr	DB_HexL
	move.l	d4,d0
	add.l	d5,d0
	cmp.l	#$c000,d0
	bhs	.Err

	move.l	d4,ResumePCount			;<------ to immediately execute!!!

	lea	FreshBootMemory,a0		;1st reset memory as a "fresh" powerup
	move.l	Mem_PtrVar,a1
	move.w	#$07ff,d0
.fresh	move.b	(a0,d0.w),(a1,d0.w)
	dbf	d0,.fresh	

.MemMv	move.l	InstTbl_Var,a0
	add.l	#$80,a0				;data source...
	move.l	Mem_PtrVar,a1
	add.l	d4,a1				;data dest...
	subq.w	#1,d5
.mlp	move.b	(a0)+,(a1)+
	dbf	d5,.mlp				;Now in Apple Memory!!!

;----------
.DoneLd	jsr	FlushMsgs
	bsr	ResumeCPU		;resume 6502 task, restore pointer, restore video...

	bra	Wait
;-----------
.Err	move.l	MyWindow,.winErr		;say error happened, then go to ".NoLoad"

	lea	.ErrMsg,a1
	lea	.ErrAns,a2			; "Hey, error loading" req
	move.l	EasyReq,a3
	move.l	#0,a4
	lea	.ErrTag,a0
	CALLREQ	rtEZRequestA
	bra	.DoneLd

.ErrMsg		dc.b	"An Error Occured",10
		dc.b	"With That File.",0
.ErrAns		dc.b	"_OK",0
.ErrTag		dc.l	RT_Underscore,'_',RT_ReqPos,REQPOS_TOPLEFTSCR
		dc.l	RT_Window
.winErr		dc.l	0	;<-- patch in window before using taglist...
		dc.l	RT_LockWindow,1,RTEZ_ReqTitle,.ErrTtl,TAG_DONE
.ErrTtl	dc.b	"Error",0
;------------
.LoadTtl	dc.b	"File to Load:",0

.LoadTag	dc.l	RT_Underscore,'_',RTFI_Flags,FREQF_PATGAD	;RT_ReqPos,REQPOS_TOPLEFTSCR
		dc.l	RT_Window
.win		dc.l	0	;<-- patch in window before using taglist...
		dc.l	RT_LockWindow,1,TAG_DONE
.FullPath	ds.b	255
;-----------
.BootMsg	dc.b	"Disk Loaded.",10,"Boot Disk?",0
.BootAns	dc.b	"_Yes|_No",0
.BootTag	dc.l	RT_Underscore,'_',RT_ReqPos,REQPOS_TOPLEFTSCR
		dc.l	RTEZ_Flags,EZREQF_NORETURNKEY!EZREQF_LAMIGAQUAL,RT_Window
.win4		dc.l	0	;<-- patch in window before using taglist...
		dc.l	RT_LockWindow,1,RTEZ_ReqTitle,.BootTtl,TAG_DONE
.BootTtl	dc.b	"Boot?",0
;-----------
.Load12Msg	dc.b	"Load disk into",10,"drive 1 or 2 ?",0
.Load12Ans	dc.b	"_1|_2|Abort",0
.Load12Tag	dc.l	RT_Underscore,'_',RT_ReqPos,REQPOS_TOPLEFTSCR
		dc.l	RTEZ_Flags,EZREQF_NORETURNKEY!EZREQF_LAMIGAQUAL,RT_Window
.win12		dc.l	0	;<-- patch in window before using taglist...
		dc.l	RT_LockWindow,1,RTEZ_ReqTitle,.Load12Ttl,TAG_DONE
.Load12Ttl	dc.b	"Which Drive?",0

.LastChar	dc.b	0			;last char of filename(to check for ddd type)

.TempLoadFileNm	ds.b	120			;temporary "load" name (until drive # known)

LoadSaveNmPtrs	dc.l	.LoadSaveNm1,.LoadSaveNm2
.LoadSaveNm1	ds.b	120			;drive 1 filename....
.LoadSaveNm2	ds.b	120			;drive 2 filename...
		even
*-------------------------------------------------------------------------*	
SaveReq	bsr	PauseCPU			;stop 6502 task, clear & show MyScreen
	bsr	ShowMainScreen
	
	move.l	MyWindow,.win1
	move.l	MyWindow,.win2
	move.l	MyWindow,.win3
	move.l	MyWindow,.win12
	
;	lea	.SaveMsg1,a1
;	lea	.SaveAns1,a2			; "Save as Disk, Mem, or Cancel?" req
;	move.l	EasyReq,a3			;     type= 1     2
;	move.l	#0,a4
;	lea	.SaveTag1,a0
;	CALLREQ	rtEZRequestA
	
;	move.b	d0,.SaveType
;	beq	.Done				; nope! don't quit!


*...................................
	moveq.l	#0,d0				;default to drive 1
	tst.l	disk_Buffer2			;unless 2 drives present...
	beq.b	.DriveSelected

	lea	.Save12Msg,a1			; "Save Which drive?" req
	lea	.Save12Ans,a2
	move.l	EasyReq,a3
	move.l	#0,a4
	lea	.Save12Tag,a0
	CALLREQ	rtEZRequestA
	tst.l	d0
	beq	.Done				;Cancel

	subq.l	#1,d0				;Else d0 = drive # (0 or 1)

.DriveSelected						;d0 = drive # (0 or 1) !!!
	move.l	(disk_Buffer.l,d0.w*4),Src_Disk_Buffer	;Set proper destination...
	move.l	(LoadSaveNmPtrs.l,d0.w*4),a5		;a5-> Proper Filename for drive
	move.w	d0,.DriveNumW				;keep drive #
*.................................................................................

	lea	.SaveTag2,a0			;*** "Save A File" req ***
	move.l	FileReq,a1
	move.l	a5,a2				;filename for this drive
	lea	.SaveTtl2,a3
	CALLREQ	rtFileRequestA

	tst.l	d0
	beq	.Done

.build	move.l	FileReq,a0			;*** Build string with path + filename ***
	move.l	rtfi_Dir(a0),a0			;path string
	lea	.FullPath,a1			;dest buffer

	tst.b	(a0)
	beq	.bldFl				;No path info, use current dir...
	
.lp1	move.b	(a0)+,(a1)+
	bne	.lp1

	lea	-1(a1),a1			;back over NULL (pts at that byte now)
	cmp.b	#':',-1(a1)			;end with a   :  ?
	beq.s	.bldFl
	
	move.b	#'/',(a1)+			;No, tag a trailing / in dest...

.bldFl	move.l	a5,a0
.lp2	move.b	(a0)+,(a1)+
	bne	.lp2
	move.b	-2(a0),.LastChar		;Keep last char (to check for ddd type)

.verify	move.l	#.FullPath,d1			;CHECK Pre-Existance of file!
	move.l	#ACCESS_READ,d2
	CALLDOS	Lock
	move.l	d0,d1
	beq	.DoSave				;file does not exist, so go ahead & save
	CALLDOS	UnLock
	
.SaveV	lea	.VerMsg,a1			;*** "are you sure?" req ***
	lea	.VerAns,a2
	move.l	EasyReq,a3
	move.l	a5,.FilenamePtr			;filename
	lea	.FilenamePtr,a4
	lea	.VerTag,a0
	CALLREQ	rtEZRequestA
	
	tst.l	d0
	beq	.Done				; nope! don't quit!

.DoSave	;cmp.b	#1,.SaveType
	;bne	.SvMem

.SvDsk	move.l	#.FullPath,d1			;*** And save the image! ***
	move.l	#MODE_NEWFILE,d2		;were saving
	CALLDOS	Open
	move.l	d0,.FileHandle			;file handle
	beq	.SaveErr

	cmp.b	#'>',.LastChar
	bne.b	.Plain
	
.DDD	move.l	InstTbl_Var,a0			;256k dest (will be restored by resumeCpu)
	jsr	CompressDisk			;Save it! Use DDD compression
	bra	.DnSv
	
.Plain	move.l	InstTbl_Var,a0			;256k dest (will be restored by resumeCpu)
	jsr	PlainDiskImage_Save		;Save it as a plain 143,360 byte image
	
.DnSv	move.l	.FileHandle,d1
	move.l	InstTbl_Var,d2
	move.l	d0,d3				;Length of file...
	CALLDOS	Write

	move.l	.FileHandle,d1
	CALLDOS	Close	

	move.w	.DriveNumW,d0
	clr.b	(disk_Changed.l,d0.w)		;mark drive as having no active changes

	bra	.Done

.SvMem	nop
.SaveErr

.Done	jsr	FlushMsgs
	bsr	ResumeCPU		;resume 6502 task, restore pointer, restore video...

	bra	Wait

.DriveNumW	dc.w	0			;Drive we are saving (0 or 1)
.LastChar	dc.b	0			;Last char in filename (is it a '>' ?)
;----------
.SaveMsg1	dc.b	"Save Disk Image or",10
		dc.b	"Memory Snapshot?",0
.SaveAns1	dc.b	"_Disk|_Memory|_Cancel",0
.SaveTag1	dc.l	RT_Underscore,'_',RT_ReqPos,REQPOS_TOPLEFTSCR
		dc.l	RTEZ_Flags,EZREQF_NORETURNKEY|EZREQF_LAMIGAQUAL,RT_Window
.win1		dc.l	0	;<-- patch in window before using taglist...
		dc.l	RT_LockWindow,1,RTEZ_ReqTitle,.SaveTtl1,TAG_DONE
.SaveTtl1	dc.b	"Save Type:",0
.SaveType	dc.b	0	;<-- 1 for Disk Image, 2 for Mem Snapshot
.FileHandle	dc.l	0
;------------
.SaveTtl2	dc.b	"File to Save:",0

.SaveTag2	dc.l	RT_Underscore,'_',RTFI_Flags,FREQF_SAVE!FREQF_PATGAD
		dc.l	RT_Window
.win2		dc.l	0	;<-- patch in window before using taglist...
		dc.l	RT_LockWindow,1,TAG_DONE
.FullPath	ds.b	255
.FilenamePtr	dc.l	00
;-------------
.VerMsg		dc.b	"Replace File",10,'"%s" ?',0
.VerAns		dc.b	"_Yes|_Cancel",0
.VerTag		dc.l	RT_Underscore,'_',RT_ReqPos,REQPOS_TOPLEFTSCR
		dc.l	RTEZ_Flags,EZREQF_NORETURNKEY!EZREQF_LAMIGAQUAL!EZREQF_CENTERTEXT
		dc.l	RT_Window
.win3		dc.l	0	;<-- patch in window before using taglist...
		dc.l	RT_LockWindow,1,RTEZ_ReqTitle,.VerTtl,TAG_DONE
.VerTtl		dc.b	"Sure?",0
;-------------
.Save12Msg	dc.b	"Save disk from",10,"drive 1 or 2 ?",0
.Save12Ans	dc.b	"_1|_2|Abort",0
.Save12Tag	dc.l	RT_Underscore,'_',RT_ReqPos,REQPOS_TOPLEFTSCR
		dc.l	RTEZ_Flags,EZREQF_NORETURNKEY!EZREQF_LAMIGAQUAL,RT_Window
.win12		dc.l	0	;<-- patch in window before using taglist...
		dc.l	RT_LockWindow,1,RTEZ_ReqTitle,.Save12Ttl,TAG_DONE
.Save12Ttl	dc.b	"Save Which Drive?",0

		CNOP	0,4
*-------------------------------------------------------------------------*	
* This routine will cause the 6502 emulation sub-task to stop running.
* It works by setting a global "stop6502" variable, signals that it's stopped,
* and waits to be signaled to restart. When this function returns,
* the sub-task is stopped! (Can remove or resume subtask at this time)
* Does not need 6502 task context!

PauseCPU:

	CALLEXEC Forbid
	
	move.l	#STOP_INST,a0			;Next 6502 inst will be STOP_INST !
	move.l	InstTbl_Var,a1			;array of 65536 longs...
	move.w	#65535,d1	
.lp	move.l	a0,(a1)+
	dbf	d1,.lp
	CALLEXEC Permit

	move.l	ParentSigMask,d0
	CALLEXEC Wait				;Wait for 6502 emulation to stop...
	rts
	

*-------------------------------------------------------------------------*	
* This function turns sets up, shows, and clears the main screen in
* preparation for a requester or graphics.
* This function is usually called AFTER Pausing the Cpu.

ShowMainScreen:

	movem.l	d2-d4,-(sp)
	move.l	#0,d0
	CALLINT	LockIBase
	move.l	d0,IBaseLock

	move.l	MyScreen,a1
	lea	sc_BitMap(a1),a1		;a1 -> Screen.BitMap
	move.l	OrigPlane1,bm_Planes(a1)	;plug in BitMap Plane Ptrs
	move.l	OrigPlane2,bm_Planes+4(a1)	;plug in BitMap Plane Ptrs
	move.l	OrigPlane3,bm_Planes+8(a1)	;plug in BitMap Plane Ptrs
	move.l	OrigPlane4,bm_Planes+12(a1)	;plug in BitMap Plane Ptrs
	move.l	OrigPlane5,bm_Planes+14(a1)	;plug in BitMap Plane Ptrs

	move.w	OrigRows,bm_Rows(a1)
	move.b	OrigDepth,bm_Depth(a1)

	move.l	MyScreen,a0
	lea	sc_ViewPort(a0),a0		;a1 -> Screen ViewPort
	lea	MainColorTable,a1
	move.w	(a1)+,d0
	CALLGRAF LoadRGB4
	move.l	MainColorTable,LastColorTable

	move.l	MyScreen,a0
	CALLINT	MakeScreen

	move.l	IBaseLock,a0
	CALLINT	UnlockIBase

	CALLINT	RethinkDisplay

	move.l	MyWindow,a0
	move.l	wd_RPort(a0),a1			;rastport
	move.b	#0,d0
	CALLGRAF SetAPen
	
	move.l	MyWindow,a0			;blank text screen to black...
	move.l	wd_RPort(a0),a1
	move.w	#0,d0
	move.w	#12,d1
	move.w	#330,d2
	move.w	#210,d3
	CALLGRAF RectFill	

					;------- Show screen w/ no copper magic
;	CALLEXEC Forbid				;Change to default blank screen...
;	move.l	MyScreen,a0			;screen
;	lea	sc_ViewPort(a0),a0		;screen.viewport
;	move.l	#0,vp_UCopIns(a0)		;screen.viewport.UCopIns = 0
;	CALLEXEC Permit	
;	CALLINT RethinkDisplay

	movem.l	(sp)+,d2-d4
	rts

*-------------------------------------------------------------------------*	
* This routine will resume the 6502 emulation sub-task. It restores the
* instruction opcode jump-table and signals the emulation to continue.
* These actions also refresh all graphics memory, restore the window pointer,
* and show the proper video mode. And activates our window! When this routine
* returns, the emulation sub-task is running. Does not need 6502 task context!

ResumeCPU:

.sprite	move.l	MyWindow,a0			;setup an invisible sprite!
	lea	SpriteImage,a1
	move.l	#4,d0				;height
	move.l	#16,d1				;width
	moveq.l	#0,d2
	moveq.l	#0,d3
	CALLINT SetPointer

	move.l	MySubTask,a1
	move.l	ChildSigMask,d0
	CALLEXEC Signal				;Tell 6502 to continue running!

	move.l	ParentSigMask,d0
	CALLEXEC Wait				;Wait for 6502 to respond (list restored)...

	move.l	MyWindow,a0
	CALLINT	ActivateWindow

	rts	

***********************************************************************
* DrawNewStatusMsg- Takes PTR from StatusNewMsgPtr (Null term string + duration) so there
* won't be a conflict between multiple-resettings at any time. Copies string to local buffer,
* prints it on screen, and sets StatusCountdown with duration.
*
* Enter:  NewStatusMsgPtr -> String + Null + Duration byte (in intuiticks)
* Return: Sets "StatusCountdown"
*	  Draws status msg on screen...

DrawNewStatusMsg:
	tst.l	NewStatusMsgPtr			;Handle StatusMsg's.. New one present?
	beq	.done

	move.l	NewStatusMsgPtr,a0		;Copy string...
	clr.l	NewStatusMsgPtr
	lea	.StatusTxt,a1
.copy	move.b	(a0)+,(a1)+
	bne.b	.copy

	move.b	(a0),StatusCountdown		;and set countdown variable...
	
	move.l	#$ffffffff,d0
.draw	move.l	Hgr1_Planes,a1
	add.l	#193*40,a1			;a1 - a4 -> Bplane(s)1 line #193
	move.l	Hgr2_Planes,a2
	add.l	#193*40,a2
	move.l	Gr1_Planes,a3
	add.l	#193*40,a3
	move.l	Gr2_Planes,a4
	add.l	#193*40,a4

	move.w	#7*40/4-1,d1			;clear 32 lines in text window...
.fill	move.l	d0,(a1)+
	move.l	d0,(a2)+
	move.l	d0,(a3)+
	move.l	d0,(a4)+
	dbf	d1,.fill

.DoTxt	move.l	Hgr1_Planes,a1
	add.l	#194*40,a1			;a1 -> Top line to draw into, left side
	move.l	Hgr2_Planes,a2			;a2,a3,a4 -> Other BPlanes to write to...
	add.l	#194*40,a2
	move.l	Gr1_Planes,a3
	add.l	#194*40,a3
	move.l	Gr2_Planes,a4
	add.l	#194*40,a4

	
	lea	.StatusTxt,a0
	
.TxtLp	clr.w	d0
	move.b	(a0)+,d0
	beq	.done
	
	lea	StatusCharSet8X5,a5
	sub.b	#' ',d0				;space is 1st char in table...
	mulu.w	#5,d0
	lea	(a5,d0.w),a5
	
	move.b	(a5),(a1)+			;top rasterline of text to all 4 bplanes...
	move.b	(a5),(a2)+	
	move.b	(a5),(a3)+	
	move.b	(a5)+,(a4)+	

	move.b	(a5),40-1(a1)	
	move.b	(a5),40-1(a2)	
	move.b	(a5),40-1(a3)	
	move.b	(a5)+,40-1(a4)	

	move.b	(a5),40*2-1(a1)	
	move.b	(a5),40*2-1(a2)	
	move.b	(a5),40*2-1(a3)	
	move.b	(a5)+,40*2-1(a4)	

	move.b	(a5),40*3-1(a1)	
	move.b	(a5),40*3-1(a2)	
	move.b	(a5),40*3-1(a3)	
	move.b	(a5)+,40*3-1(a4)	

	move.b	(a5),40*4-1(a1)			;bottom rasterline to all 4 bplanes...
	move.b	(a5),40*4-1(a2)	
	move.b	(a5),40*4-1(a3)	
	move.b	(a5)+,40*4-1(a4)	

	bra.b	.TxtLp	
.done	rts

.StatusTxt	ds.b	40
		dc.l	0,0			;extra NULL's just in case...



*********************************************************************
* ClearStatusMsg - 
* This function is called to erase all status bars from the Apple2000 display.
* Enter: None   Return: None (Status Bar erased)
	
ClearStatusMsg:
	moveq.l	#$0,d0
.draw	move.l	Hgr1_Planes,a1
	add.l	#193*40,a1			;a1 - a4 -> Bplane(s)1 line #193
	move.l	Hgr2_Planes,a2
	add.l	#193*40,a2
	move.l	Gr1_Planes,a3
	add.l	#193*40,a3
	move.l	Gr2_Planes,a4
	add.l	#193*40,a4

	move.w	#7*40/4-1,d1			;clear 32 lines in text window...
.fill	move.l	d0,(a1)+
	move.l	d0,(a2)+
	move.l	d0,(a3)+
	move.l	d0,(a4)+
	dbf	d1,.fill
	rts

NewStatusMsgPtr	dc.l	0			;any new msg ptr's appear here!
StatusCountdown	dc.b	0			;Byte counter... (set in DrawNewStatusMsg)
	even

**************************************888
*
* CheckDiskAndWarn - 
* This function takes a drive # (0 or 1) and checks if the contents of that drive have
* been changed. If so, it places up a "Data has changed. It will be lost. Ok?" requester.
* Usefull during loading new disks and when quiting emulation.
* Note: Only call from "Paused" cpu state
*
* Enter:  d0.w = Drive # (0 or 1)
* Return: d0 = "OK" boolean response (1="Ok" or disk not changed, 0=Cancel, No Way!)

CheckDiskAndWarn:
	move.l	MyWindow,.win1

	tst.b	(disk_Changed.l,d0.w)		;has drive data been changed?
	bne.b	.chngd
	moveq.l	#1,d0				;no, let program proceed
	rts	

.chngd	move.b	d0,d1				;patch in drive # '1' or '2' into msg
	add.b	#'1',d1
	move.b	d1,.WarnMsgDrvNum

	move.l	(LoadSaveNmPtrs.l,d0.w*4),.FilenamePtr	;a5-> Proper Filename for drive

	lea	.WarnMsg,a1			;*** "It will be lost. Ok?" req ***
	lea	.WarnAns,a2
	move.l	EasyReq,a3
	lea	.FilenamePtr,a4			;filename
	lea	.WarnTag,a0
	CALLREQ	rtEZRequestA

	rts

;-------------
.WarnMsg	dc.b	"Disk Image in drive "
.WarnMsgDrvNum	dc.b	"1",10,'"%s"',10,"was changed & will be lost!",0
.WarnAns	dc.b	"_Ok|_Cancel",0
.WarnTag	dc.l	RT_Underscore,'_',RT_ReqPos,REQPOS_TOPLEFTSCR
		dc.l	RTEZ_Flags,EZREQF_NORETURNKEY!EZREQF_LAMIGAQUAL!EZREQF_CENTERTEXT
		dc.l	RT_Window
.win1		dc.l	0	;<-- patch in window before using taglist...
		dc.l	RT_LockWindow,1,RTEZ_ReqTitle,.WarnTtl,TAG_DONE
.WarnTtl	dc.b	"Pardon Me, but...",0
.FilenamePtr	dc.l	0


	EVEN
