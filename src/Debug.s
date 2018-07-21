** DEBUGGING Routines... Print Strings, hex numbers, dec numbers, etc...
** to MYCHANNEL... (opened prior)
	SECTION	APPLEII,CODE
	EVEN
	rts
DB_String:		** A0 -> Null Terminated String to Print
	movem.l	a1-a6/d0-d4,-(sp)
	move.l	a0,.ds			;move str ptr into data stream...
	lea	.fs,a0			;format string ptr
	lea	.ds,a1			;data stream ptr
	lea	DB_PutChProc,a2
	move.l	#0,a3			;indx to buffer
	lea	DB_Buffer,a4
	CALLEXEC RawDoFmt
	movem.l	(sp)+,a1-a6/d0-d4
	rts	
.ds	ds.l	1
.fs	dc.b	'%s',0
	
	even
DB_HexB	movem.l	a0-a6/d0-d4,-(sp) ** D0 = a word value to print
	move.w	d0,.ds			;move # into data stream
	lea	.fs,a0			;format string ptr
	lea	.ds,a1			;data stream ptr
	lea	DB_PutChProc,a2
	move.l	#0,a3
	lea	DB_Buffer,a4
	CALLEXEC RawDoFmt
	movem.l	(sp)+,a0-a6/d0-d4
	rts
.ds	ds.w	1
.fs	dc.b	'$%02x ',0	

	even
DB_HexW	movem.l	a0-a6/d0-d4,-(sp) ** D0 = a word value to print
	move.w	d0,.ds			;move # into data stream
	lea	.fs,a0			;format string ptr
	lea	.ds,a1			;data stream ptr
	lea	DB_PutChProc,a2
	move.l	#0,a3
	lea	DB_Buffer,a4
	CALLEXEC RawDoFmt
	movem.l	(sp)+,a0-a6/d0-d4
	rts
.ds	ds.w	1
.fs	dc.b	'$%04x',0	
	
	even
DB_HexL	movem.l	a0-a6/d0-d4,-(sp)  ** D0 = A LONG value to print
	move.l	d0,.ds			;move # into data stream
	lea	.fs,a0			;format string ptr
	lea	.ds,a1			;data stream ptr
	lea	DB_PutChProc,a2
	move.l	#0,a3
	lea	DB_Buffer,a4
	CALLEXEC RawDoFmt
	movem.l	(sp)+,a0-a6/d0-d4
	rts
.ds	ds.l	1
.fs	dc.b	'$%08lx',0
	EVEN	

DB_PutChProc:
	move.b	d0,(a4,a3.w)		;buffer + indx
	beq	DB_PrintBuffer
	addq.w	#1,a3
	rts

	even
DB_PrintBuffer:		** Called with a3= Length of Buffer (excludes NULL)
	move.l	MyChannel,d1
	move.l	#DB_Buffer,d2
	move.l	a3,d3		;length
	CALLDOS Write
	rts

DataStream	dc.w	9,8,7,6,5
DB_Buffer	ds.b	85
