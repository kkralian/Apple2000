* Needs to be included in file with "WinRastPort" set and FPutByte available.
* enter with: a0=address to write ($2000-$3fff for PBHgr1), and d1=data.
* MAJOR OPT: At entry to PBHgrx, checks to see if mem is already same as data, if so, return!

	SECTION	HiResAPPLEII,CODE
	CNOP	0,4
PBHgr1	cmp.b	(Mem_Ptr,a0.l),d1
	beq.b	.NoShow
	move.b	d1,(Mem_Ptr,a0.l)	;FPUTBYTE
	move.l	-2(Mem_Ptr,a0.l),d0	;2 pre-data bytes & 1 post-data byte!!!!!
					;bits we want: (H= HighBit, #=PixelData)
					;xxxx xxxx H654 3210 H654 3210 H654 3210  (14 sig bits)
					;          ---       --------- -      --

	swap	d0		[04]
	lsr.b	#5,d0		[04]	;now got:
	swap	d0		[04]	;xxxx xxxx oooo oH65 H654 3210 Hxxx xx10
					;                --- ---- ---- -      --
	lsl.l	#1,d0		[04]	;xxxx xxxo oooo H65H 6543 210H xxxx x10x
	lsl.b	#5,d0		[04]	;xxxx xxxo oooo H65H 6543 210H 10xo oooo
	lsr.l	#6,d0		[04]	;xxxx xxxx xxxx xooo ooH6 5H65 4321 0H10
					;	               -- ---- ---- ----
					; total = [ 24 ] !
					;d0 = clean word of all video data (index w/ it later)
	move.l	d0,a6
	
	move.l	a0,d0
	lea	HgrHorByteLookup,a1
	and.w	#$7f,d0			[06]	;(later make list 256 words & remove this line?)
	move.w	(a1,d0.w*2),d0	;D0 has horizontal byte offset! (0->39 *7+20 for BFINS)  CLEAN!
	beq.b	.NoShow			;its a video hole... no draw....
	bmi	.Hgr1Edge
.AfterEdge:
	move.l	a0,d1
	lsr.w	#3,d1			;/8 for list optimization... (from 8192 bytes to 1024)
	and.w	#$3ff,d1		;mask off  above #1023... (we didnt -$2000 from addr)
	lea	HgrLineLookup,a1
	move.w	(a1,d1.w*2),d1		;D1 Has vertical Line # !!!! (0-191) * 40 (BytesPerRow)
					;d1 = Y*40(bytesPerRow)
	move.l	Hgr1_Planes,a0
	add.l	d1,a0		; a0 = Planet1 Line StartLoc
	btst.l	#0,d0
	bne.b	.Odd
.Even				;a0 = Plane1 Line StartLoc
				;d0 = BitField Offset (for solid grfx & centered)
	move.l	HiResDrawTbl,a1
	move.l	a6,d1	;swap nibbles on %H765 4321 to 1234 567o

	btst.l	#10,d1
	sne.b	d1
	bfins	d1,(LINES*2*40.w,a0){d0:7}		;if data hi bit set, make $ff, else $00

	subq.l	#1,d0

	move.l	(a1,a6.w*4),d1
	bfins	d1,(a0){d0:9}		;plane 1
	
	swap	d1
	bfins	d1,(LINES*40.w,a0){d0:9}	;plane 2

.NoShow
	rts		
	CNOP	0,4
.Odd				;a0 = Plane1 StartLoc
			;d0 = Bitfield offset
	move.l	HiResDrawTbl,a1
	move.l	a6,d1	;swap nibbles on %H765 4321 to 1234 567o

	btst.l	#10,d1
	sne.b	d1
	bfins	d1,(LINES*40*2.w,a0){d0:7}		;if data hi bit set, make $ff, else $00

	subq.l	#1,d0

	move.l	(a1,a6.w*4),d1
	bfins	d1,(LINES*40.w,a0){d0:9}	;plane 2

	swap	d1
	bfins	d1,(a0){d0:9}		;plane 1
	
	rts		

.Hgr1Edge:		;determine which edge & mask off...
	move.l	a6,d1
	btst.l	#14,d0
	beq.b	.RtEdge
.LtEdge	and.l	#$07ff,d1		;dont want data from previous line
	move.l	d1,a6
	and.w	#$0fff,d0		;and remove this Edge Identifying bits...
	bra	.AfterEdge
.RtEdge	and.l	#$fff8,d1		;dont want data from next line
	move.l	d1,a6
	and.w	#$0fff,d0
	bra	.AfterEdge

	CNOP	0,4
.addr	ds.l	1
	dc.b	'K'+1,'E'+2,'V'+3,'I'+4,'N'+5,'K'+6,'R'+7,'A'+8,'L'+9,'I'+10,'A'+11,'N'+12

************************************************8

	CNOP	0,4
PBHgr2	cmp.b	(Mem_Ptr,a0.l),d1
	beq.b	.NoShow
	move.b	d1,(Mem_Ptr,a0.l)	;FPUTBYTE
	move.l	-2(Mem_Ptr,a0.l),d0	;2 pre-data bytes & 1 post-data byte!!!!!
					;bits we want: (H= HighBit, #=PixelData)
					;xxxx xxxx H654 3210 H654 3210 H654 3210  (14 sig bits)
					;          ---       --------- -      --

	swap	d0		[04]
	lsr.b	#5,d0		[04]	;now got:
	swap	d0		[04]	;xxxx xxxx xxxx xH65 H654 3210 Hxxx xx10
					;                --- ---- ---- -      --
	lsl.l	#1,d0		[04]	;xxxx xxxx xxxx H65H 6543 210H xxxx x10x
	lsl.b	#5,d0		[04]	;xxxx xxxx xxxx H65H 6543 210H 10xx xxxx
	lsr.l	#6,d0		[04]	;xxxx xxxx xxxx xxxx xxH6 5H65 4321 0H10
					;	               -- ---- ---- ----
					; total = [ 24 ] !
	move.l	d0,a6

	move.l	a0,d0
	lea	HgrHorByteLookup,a1
	and.w	#$7f,d0			[06]	;(later make list 256 words & remove this line?)
	move.w	(a1,d0.w*2),d0	;D0 has horizontal byte offset! (0->39 *7+20 for BFINS)  CLEAN!
	beq.b	.NoShow			;its a video hole... no draw....
	bmi.b	.Hgr2Edge
.AfterEdge:
	move.l	a0,d1
	lsr.w	#3,d1			;/8 for list optimization... (from 8192 bytes to 1024)
	and.w	#$3ff,d1		;mask off  above #1023... (we didnt -$4000 from addr)
	lea	HgrLineLookup,a1
	move.w	(a1,d1.w*2),d1		;D1 Has vertical Line # !!!! (0-191) * 40 (bytesPerRow)
	
	move.l	Hgr2_Planes,a0
	add.l	d1,a0		; a0 = Planet1 Line StartLoc
	btst.l	#0,d0
	bne.b	.Odd
.Even				;a0 = Plane1 Line StartLoc
				;d0 = BitField Offset (for solid grfx & centered)
	move.l	HiResDrawTbl,a1
	move.l	a6,d1		;swap nibbles on %H765 4321 to 1234 567o

	btst.l	#10,d1
	sne.b	d1
	bfins	d1,(LINES*40*2.w,a0){d0:7}		;if data hi bit set, make $ff, else $00

	subq.l	#1,d0

	move.l	(a1,a6.w*4),d1		;+1 for OddOffset...
	bfins	d1,(a0){d0:9}		;plane 1

	swap	d1
	bfins	d1,(LINES*40.w,a0){d0:9}	;plane 2

.NoShow
	rts		
	CNOP	0,4
.Odd				;a0 = Plane1 StartLoc
			;d0 = Bitfield offset
	move.l	HiResDrawTbl,a1
	move.l	a6,d1	;swap nibbles on %H765 4321 to 1234 567o

	btst.l	#10,d1
	sne.b	d1
	bfins	d1,(LINES*40*2.w,a0){d0:7}		;if data hi bit set, make $ff, else $00

	subq.l	#1,d0

	move.l	(a1,a6.w*4),d1
	bfins	d1,(LINES*40.w,a0){d0:9}	;plane 2

	swap	d1
	bfins	d1,(a0){d0:9}		;plane 1
	
	rts		

.Hgr2Edge:		;determine which edge & mask off...
	move.l	a6,d1
	btst.l	#14,d0
	beq.b	.RtEdge
.LtEdge	and.l	#$07ff,d1		;dont want data from previous line
	move.l	d1,a6
	and.w	#$0fff,d0		;and remove this Edge Identifying bits...
	bra	.AfterEdge
.RtEdge	and.l	#$fff8,d1		;dont want data from next line
	move.l	d1,a6
	and.w	#$0fff,d0
	bra	.AfterEdge

************************************************8

	CNOP	0,4
PBHgr1Top:
	cmp.b	(Mem_Ptr,a0.l),d1
	beq.b	.NoShow
	move.b	d1,(Mem_Ptr,a0.l)	;FPUTBYTE
	move.l	-2(Mem_Ptr,a0.l),d0	;2 pre-data bytes & 1 post-data byte!!!!!
					;bits we want: (H= HighBit, #=PixelData)
					;xxxx xxxx H654 3210 H654 3210 H654 3210  (14 sig bits)
					;          ---       --------- -      --

	swap	d0		[04]
	lsr.b	#5,d0		[04]	;now got:
	swap	d0		[04]	;xxxx xxxx oooo oH65 H654 3210 Hxxx xx10
					;                --- ---- ---- -      --
	lsl.l	#1,d0		[04]	;xxxx xxxo oooo H65H 6543 210H xxxx x10x
	lsl.b	#5,d0		[04]	;xxxx xxxo oooo H65H 6543 210H 10xo oooo
	lsr.l	#6,d0		[04]	;xxxx xxxx xxxx xooo ooH6 5H65 4321 0H10
					;	               -- ---- ---- ----
					; total = [ 24 ] !
					;d0 = clean word of all video data (index w/ it later)
	move.l	d0,a6
	
	move.l	a0,d0
	lea	HgrHorByteLookup,a1
	and.w	#$7f,d0			[06]	;(later make list 256 words & remove this line?)
	move.w	(a1,d0.w*2),d0	;D0 has horizontal byte offset! (0->39 *7+20 for BFINS)  CLEAN!
	beq.b	.NoShow			;its a video hole... no draw....
	bmi	.Hgr1Edge
.AfterEdge:
	move.l	a0,d1
	lsr.w	#3,d1			;/8 for list optimization... (from 8192 bytes to 1024)
	and.w	#$3ff,d1		;mask off  above #1023... (we didnt -$2000 from addr)
	lea	HgrLineLookup,a1
	move.w	(a1,d1.w*2),d1		;D1 Has vertical Line # !!!! (0-191) * 40 (BytesPerRow)
					;d1 = Y*40(bytesPerRow)
	cmp.w	#40*160,d1
	bhs.b	.NoShow
					
	move.l	Hgr1_Planes,a0
	add.l	d1,a0		; a0 = Planet1 Line StartLoc
	btst.l	#0,d0
	bne.b	.Odd
.Even				;a0 = Plane1 Line StartLoc
				;d0 = BitField Offset (for solid grfx & centered)
	move.l	HiResDrawTbl,a1
	move.l	a6,d1	;swap nibbles on %H765 4321 to 1234 567o

	btst.l	#10,d1
	sne.b	d1
	bfins	d1,(LINES*2*40.w,a0){d0:7}		;if data hi bit set, make $ff, else $00

	subq.l	#1,d0

	move.l	(a1,a6.w*4),d1
	bfins	d1,(a0){d0:9}		;plane 1
	
	swap	d1
	bfins	d1,(LINES*40.w,a0){d0:9}	;plane 2

.NoShow
	rts		
	CNOP	0,4
.Odd				;a0 = Plane1 StartLoc
			;d0 = Bitfield offset
	move.l	HiResDrawTbl,a1
	move.l	a6,d1	;swap nibbles on %H765 4321 to 1234 567o

	btst.l	#10,d1
	sne.b	d1
	bfins	d1,(LINES*40*2.w,a0){d0:7}		;if data hi bit set, make $ff, else $00

	subq.l	#1,d0

	move.l	(a1,a6.w*4),d1
	bfins	d1,(LINES*40.w,a0){d0:9}	;plane 2

	swap	d1
	bfins	d1,(a0){d0:9}		;plane 1
	
	rts		

.Hgr1Edge:		;determine which edge & mask off...
	move.l	a6,d1
	btst.l	#14,d0
	beq.b	.RtEdge
.LtEdge	and.l	#$07ff,d1		;dont want data from previous line
	move.l	d1,a6
	and.w	#$0fff,d0		;and remove this Edge Identifying bits...
	bra	.AfterEdge
.RtEdge	and.l	#$fff8,d1		;dont want data from next line
	move.l	d1,a6
	and.w	#$0fff,d0
	bra	.AfterEdge

	CNOP	0,4
.addr	ds.l	1
	dc.b	'K'+1,'E'+2,'V'+3,'I'+4,'N'+5,'K'+6,'R'+7,'A'+8,'L'+9,'I'+10,'A'+11,'N'+12

************************************************8

	CNOP	0,4
PBHgr2Top:
	cmp.b	(Mem_Ptr,a0.l),d1
	beq.b	.NoShow
	move.b	d1,(Mem_Ptr,a0.l)	;FPUTBYTE
	move.l	-2(Mem_Ptr,a0.l),d0	;2 pre-data bytes & 1 post-data byte!!!!!
					;bits we want: (H= HighBit, #=PixelData)
					;xxxx xxxx H654 3210 H654 3210 H654 3210  (14 sig bits)
					;          ---       --------- -      --

	swap	d0		[04]
	lsr.b	#5,d0		[04]	;now got:
	swap	d0		[04]	;xxxx xxxx xxxx xH65 H654 3210 Hxxx xx10
					;                --- ---- ---- -      --
	lsl.l	#1,d0		[04]	;xxxx xxxx xxxx H65H 6543 210H xxxx x10x
	lsl.b	#5,d0		[04]	;xxxx xxxx xxxx H65H 6543 210H 10xx xxxx
	lsr.l	#6,d0		[04]	;xxxx xxxx xxxx xxxx xxH6 5H65 4321 0H10
					;	               -- ---- ---- ----
					; total = [ 24 ] !
	move.l	d0,a6

	move.l	a0,d0
	lea	HgrHorByteLookup,a1
	and.w	#$7f,d0			[06]	;(later make list 256 words & remove this line?)
	move.w	(a1,d0.w*2),d0	;D0 has horizontal byte offset! (0->39 *7+20 for BFINS)  CLEAN!
	beq.b	.NoShow			;its a video hole... no draw....
	bmi.b	.Hgr2Edge
.AfterEdge:
	move.l	a0,d1
	lsr.w	#3,d1			;/8 for list optimization... (from 8192 bytes to 1024)
	and.w	#$3ff,d1		;mask off  above #1023... (we didnt -$4000 from addr)
	lea	HgrLineLookup,a1
	move.w	(a1,d1.w*2),d1		;D1 Has vertical Line # !!!! (0-191) * 40 (bytesPerRow)

	cmp.w	#40*160,d1
	bhs.b	.NoShow
	
	move.l	Hgr2_Planes,a0
	add.l	d1,a0		; a0 = Planet1 Line StartLoc
	btst.l	#0,d0
	bne.b	.Odd
.Even				;a0 = Plane1 Line StartLoc
				;d0 = BitField Offset (for solid grfx & centered)
	move.l	HiResDrawTbl,a1
	move.l	a6,d1		;swap nibbles on %H765 4321 to 1234 567o

	btst.l	#10,d1
	sne.b	d1
	bfins	d1,(LINES*40*2.w,a0){d0:7}		;if data hi bit set, make $ff, else $00

	subq.l	#1,d0

	move.l	(a1,a6.w*4),d1		;+1 for OddOffset...
	bfins	d1,(a0){d0:9}		;plane 1

	swap	d1
	bfins	d1,(LINES*40.w,a0){d0:9}	;plane 2

.NoShow
	rts		
	CNOP	0,4
.Odd				;a0 = Plane1 StartLoc
			;d0 = Bitfield offset
	move.l	HiResDrawTbl,a1
	move.l	a6,d1	;swap nibbles on %H765 4321 to 1234 567o

	btst.l	#10,d1
	sne.b	d1
	bfins	d1,(LINES*40*2.w,a0){d0:7}		;if data hi bit set, make $ff, else $00

	subq.l	#1,d0

	move.l	(a1,a6.w*4),d1
	bfins	d1,(LINES*40.w,a0){d0:9}	;plane 2

	swap	d1
	bfins	d1,(a0){d0:9}		;plane 1
	
	rts		

.Hgr2Edge:		;determine which edge & mask off...
	move.l	a6,d1
	btst.l	#14,d0
	beq.b	.RtEdge
.LtEdge	and.l	#$07ff,d1		;dont want data from previous line
	move.l	d1,a6
	and.w	#$0fff,d0		;and remove this Edge Identifying bits...
	bra	.AfterEdge
.RtEdge	and.l	#$fff8,d1		;dont want data from next line
	move.l	d1,a6
	and.w	#$0fff,d0
	bra	.AfterEdge

	CNOP	0,4
************************************************8
* enter with: a0=address to write ($2000-$3fff for PBHgr1), and d1=data.
* 

Hgr1Refresh:
	movem.l	a2-a5/d2-d7,-(sp)
	move.l	Mem_PtrVar,Mem_Ptr		;a4 (USED!)
	move.l	HiResDrawTbl,a1			;a1 -> HiRes Draw Table (Preserve!)

* Step through all the HGR line addresses...

	move.l	#$2000,a3	;a3 = main base counter...
	move.l	a3,a2		;a2 -> Base of HGR line we are doing (preserve!)
	move.l	Hgr1_Planes,a5	;a5 -> Base of BitPlane memory (top line to start)
	
.loop
	move.l	a2,a0

;PBHgr1-EntireLine!
	move.w	#$0,d5		;byte counter for full line...
	move.w	#20-7,d4		;always start with leftmost byte...!


.NextByte	

	move.l	-2(Mem_Ptr,a0.l),d0	;2 pre-data bytes & 1 post-data byte!!!!!
					;bits we want: (H= HighBit, #=PixelData)
					;xxxx xxxx H654 3210 H654 3210 H654 3210  (14 sig bits)
					;          ---       --------- -      --

	swap	d0		[04]
	lsr.b	#5,d0		[04]	;now got:
	swap	d0		[04]	;xxxx xxxx xxxx xH65 H654 3210 Hxxx xx10
					;                --- ---- ---- -      --
	lsl.l	#1,d0		[04]	;xxxx xxxx xxxx H65H 6543 210H xxxx x10x
	lsl.b	#5,d0		[04]	;xxxx xxxx xxxx H65H 6543 210H 10xx xxxx
	lsr.l	#6,d0		[04]	;xxxx xxxx xxxx xxxx xxH6 5H65 4321 0H10
					;	               -- ---- ---- ----
					; total = [ 24 ] !
	move.l	d0,a6
					;d0 = clean word of all video data (index w/ it later)


	addq.w	#7,d4		;next byte Horizontal Offset


	btst.l	#0,d4		;is this an even or odd byte?
	bne.b	.Odd
.Even				;a0 = Plane1 Line StartLoc
				;d0 = BitField Offset (for solid grfx & centered)
;	move.l	HiResDrawTbl,a1
	move.l	a6,d1	;swap nibbles on %H765 4321 to 1234 567o

	btst.l	#10,d1
	sne.b	d1
	bfins	d1,(LINES*40*2.w,a5){d4:7}		;if data hi bit set, make $ff, else $00

	subq.l	#1,d4

	move.l	(a1,a6.w*4),d1
	bfins	d1,(a5){d4:9}		;plane 1
	
	swap	d1
	bfins	d1,(LINES*40.w,a5){d4:9}	;plane 2
	addq.l	#1,d4

.again
	move.l	-1(Mem_Ptr,a0.l),d0	;2 pre-data bytes & 1 post-data byte!!!!!
					;bits we want: (H= HighBit, #=PixelData)
					;xxxx xxxx H654 3210 H654 3210 H654 3210  (14 sig bits)
					;          ---       --------- -      --

	swap	d0		[04]
	lsr.b	#5,d0		[04]	;now got:
	swap	d0		[04]	;xxxx xxxx xxxx xH65 H654 3210 Hxxx xx10
					;                --- ---- ---- -      --
	lsl.l	#1,d0		[04]	;xxxx xxxx xxxx H65H 6543 210H xxxx x10x
	lsl.b	#5,d0		[04]	;xxxx xxxx xxxx H65H 6543 210H 10xx xxxx
	lsr.l	#6,d0		[04]	;xxxx xxxx xxxx xxxx xxH6 5H65 4321 0H10
					;	               -- ---- ---- ----
					; total = [ 24 ] !
	move.l	d0,a6
					;d0 = clean word of all video data (index w/ it later)

	addq.w	#7,d4		;next byte Horizontal Offset

.Odd				;a0 = Plane1 StartLoc
			;d0 = Bitfield offset
;	move.l	HiResDrawTbl,a1
	move.l	a6,d1	;swap nibbles on %H765 4321 to 1234 567o

	btst.l	#10,d1
	sne.b	d1
	bfins	d1,(LINES*40*2.w,a5){d4:7}		;if data hi bit set, make $ff, else $00

	subq.l	#1,d4

	move.l	(a1,a6.w*4),d1
	bfins	d1,(LINES*40.w,a5){d4:9}	;plane 2

	swap	d1
	bfins	d1,(a5){d4:9}		;plane 1

	addq.l	#1,d4	
	bra	.done


.done
	addq.w	#2,d5
	lea	(a2,d5.w),a0		;set next byte addr...
	cmp.w	#40,d5
	blo	.NextByte

	add.l	#40,a5			;inc BitPlane ptr to next line...

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

	movem.l	(sp)+,a2-a5/d2-d7
	rts

**************************************************
** HorizByte is 127-Word lookup table. Using the low-end mask of the txt / hgr address,
** This returns the HORizontal BYTE offset...!!! (0-39! (+ special))
** (00 -> 39, *7 for BFINS offset field (7 pixels per byte), + 20 to center on screen)
** 0 = Video Hole (no visible effect)
** - = Its an edge byte (left or right extreme)  (numeric offset in lower 9 bits, 0 -> 293)
**     bit 14 = boolean LEFT side (1=left side, 0=right side)
** so, left edge bytes add +$C000, right edge bytes add +$8000

	SECTION	TABLES,DATA
	CNOP	0,4
HgrHorByteLookup
	dc.w	00*7+20+$c000,01*7+20,02*7+20,03*7+20,04*7+20,05*7+20,06*7+20,07*7+20,08*7+20,09*7+20
	dc.w	10*7+20,11*7+20,12*7+20,13*7+20,14*7+20,15*7+20,16*7+20,17*7+20,18*7+20,19*7+20
	dc.w	20*7+20,21*7+20,22*7+20,23*7+20,24*7+20,25*7+20,26*7+20,27*7+20,28*7+20,29*7+20
	dc.w	30*7+20,31*7+20,32*7+20,33*7+20,34*7+20,35*7+20,36*7+20,37*7+20,38*7+20,39*7+20+$8000
	dc.w	00*7+20+$c000,01*7+20,02*7+20,03*7+20,04*7+20,05*7+20,06*7+20,07*7+20,08*7+20,09*7+20
	dc.w	10*7+20,11*7+20,12*7+20,13*7+20,14*7+20,15*7+20,16*7+20,17*7+20,18*7+20,19*7+20
	dc.w	20*7+20,21*7+20,22*7+20,23*7+20,24*7+20,25*7+20,26*7+20,27*7+20,28*7+20,29*7+20
	dc.w	30*7+20,31*7+20,32*7+20,33*7+20,34*7+20,35*7+20,36*7+20,37*7+20,38*7+20,39*7+20+$8000
	dc.w	00*7+20+$c000,01*7+20,02*7+20,03*7+20,04*7+20,05*7+20,06*7+20,07*7+20,08*7+20,09*7+20
	dc.w	10*7+20,11*7+20,12*7+20,13*7+20,14*7+20,15*7+20,16*7+20,17*7+20,18*7+20,19*7+20
	dc.w	20*7+20,21*7+20,22*7+20,23*7+20,24*7+20,25*7+20,26*7+20,27*7+20,28*7+20,29*7+20
	dc.w	30*7+20,31*7+20,32*7+20,33*7+20,34*7+20,35*7+20,36*7+20,37*7+20,38*7+20,39*7+20+$8000
	dc.w	0,0,0,0,0,0,0,0

* Test for "Quick & Dirty" video display. List of byte offsets from left of screen...
QuickHgrHorByteLookup
	dc.w	00,01,02,03,04,05,06,07,08,09
	dc.w	10,11,12,13,14,15,16,17,18,19
	dc.w	20,21,22,23,24,25,26,27,28,29
	dc.w	30,31,32,33,34,35,36,37,38,39
	dc.w	00,01,02,03,04,05,06,07,08,09
	dc.w	10,11,12,13,14,15,16,17,18,19
	dc.w	20,21,22,23,24,25,26,27,28,29
	dc.w	30,31,32,33,34,35,36,37,38,39
	dc.w	00,01,02,03,04,05,06,07,08,09
	dc.w	10,11,12,13,14,15,16,17,18,19
	dc.w	20,21,22,23,24,25,26,27,28,29
	dc.w	30,31,32,33,34,35,36,37,38,39
	dc.w	-1,-1,-1,-1,-1,-1,-1,-1

** 1024 Entries. Take HGR address, offset to $0000, /8 (to eliminate repetition)
** Video "Holes" are listed as 0, for safety. Catch by checking horizontal byte offset.
** Line # * 40 (bytes per line) as optimization...

HgrLineLookup:
	dc.w	0*40,0*40,0*40,0*40,0*40,64*40,64*40,64*40,64*40,64*40,128*40,128*40,128*40,128*40,128*40,0
	dc.w	8*40,8*40,8*40,8*40,8*40,72*40,72*40,72*40,72*40,72*40,136*40,136*40,136*40,136*40,136*40,0
	dc.w	16*40,16*40,16*40,16*40,16*40,80*40,80*40,80*40,80*40,80*40,144*40,144*40,144*40,144*40,144*40,0
	dc.w	24*40,24*40,24*40,24*40,24*40,88*40,88*40,88*40,88*40,88*40,152*40,152*40,152*40,152*40,152*40,0
	dc.w	32*40,32*40,32*40,32*40,32*40,96*40,96*40,96*40,96*40,96*40,160*40,160*40,160*40,160*40,160*40,0
	dc.w	40*40,40*40,40*40,40*40,40*40,104*40,104*40,104*40,104*40,104*40,168*40,168*40,168*40,168*40,168*40,0
	dc.w	48*40,48*40,48*40,48*40,48*40,112*40,112*40,112*40,112*40,112*40,176*40,176*40,176*40,176*40,176*40,0
	dc.w	56*40,56*40,56*40,56*40,56*40,120*40,120*40,120*40,120*40,120*40,184*40,184*40,184*40,184*40,184*40,0
	dc.w	1*40,1*40,1*40,1*40,1*40,65*40,65*40,65*40,65*40,65*40,129*40,129*40,129*40,129*40,129*40,0
	dc.w	9*40,9*40,9*40,9*40,9*40,73*40,73*40,73*40,73*40,73*40,137*40,137*40,137*40,137*40,137*40,0
	dc.w	17*40,17*40,17*40,17*40,17*40,81*40,81*40,81*40,81*40,81*40,145*40,145*40,145*40,145*40,145*40,0
	dc.w	25*40,25*40,25*40,25*40,25*40,89*40,89*40,89*40,89*40,89*40,153*40,153*40,153*40,153*40,153*40,0
	dc.w	33*40,33*40,33*40,33*40,33*40,97*40,97*40,97*40,97*40,97*40,161*40,161*40,161*40,161*40,161*40,0
	dc.w	41*40,41*40,41*40,41*40,41*40,105*40,105*40,105*40,105*40,105*40,169*40,169*40,169*40,169*40,169*40,0
	dc.w	49*40,49*40,49*40,49*40,49*40,113*40,113*40,113*40,113*40,113*40,177*40,177*40,177*40,177*40,177*40,0
	dc.w	57*40,57*40,57*40,57*40,57*40,121*40,121*40,121*40,121*40,121*40,185*40,185*40,185*40,185*40,185*40,0
	dc.w	2*40,2*40,2*40,2*40,2*40,66*40,66*40,66*40,66*40,66*40,130*40,130*40,130*40,130*40,130*40,0
	dc.w	10*40,10*40,10*40,10*40,10*40,74*40,74*40,74*40,74*40,74*40,138*40,138*40,138*40,138*40,138*40,0
	dc.w	18*40,18*40,18*40,18*40,18*40,82*40,82*40,82*40,82*40,82*40,146*40,146*40,146*40,146*40,146*40,0
	dc.w	26*40,26*40,26*40,26*40,26*40,90*40,90*40,90*40,90*40,90*40,154*40,154*40,154*40,154*40,154*40,0
	dc.w	34*40,34*40,34*40,34*40,34*40,98*40,98*40,98*40,98*40,98*40,162*40,162*40,162*40,162*40,162*40,0
	dc.w	42*40,42*40,42*40,42*40,42*40,106*40,106*40,106*40,106*40,106*40,170*40,170*40,170*40,170*40,170*40,0
	dc.w	50*40,50*40,50*40,50*40,50*40,114*40,114*40,114*40,114*40,114*40,178*40,178*40,178*40,178*40,178*40,0
	dc.w	58*40,58*40,58*40,58*40,58*40,122*40,122*40,122*40,122*40,122*40,186*40,186*40,186*40,186*40,186*40,0
	dc.w	3*40,3*40,3*40,3*40,3*40,67*40,67*40,67*40,67*40,67*40,131*40,131*40,131*40,131*40,131*40,0
	dc.w	11*40,11*40,11*40,11*40,11*40,75*40,75*40,75*40,75*40,75*40,139*40,139*40,139*40,139*40,139*40,0
	dc.w	19*40,19*40,19*40,19*40,19*40,83*40,83*40,83*40,83*40,83*40,147*40,147*40,147*40,147*40,147*40,0
	dc.w	27*40,27*40,27*40,27*40,27*40,91*40,91*40,91*40,91*40,91*40,155*40,155*40,155*40,155*40,155*40,0
	dc.w	35*40,35*40,35*40,35*40,35*40,99*40,99*40,99*40,99*40,99*40,163*40,163*40,163*40,163*40,163*40,0
	dc.w	43*40,43*40,43*40,43*40,43*40,107*40,107*40,107*40,107*40,107*40,171*40,171*40,171*40,171*40,171*40,0
	dc.w	51*40,51*40,51*40,51*40,51*40,115*40,115*40,115*40,115*40,115*40,179*40,179*40,179*40,179*40,179*40,0
	dc.w	59*40,59*40,59*40,59*40,59*40,123*40,123*40,123*40,123*40,123*40,187*40,187*40,187*40,187*40,187*40,0
	dc.w	4*40,4*40,4*40,4*40,4*40,68*40,68*40,68*40,68*40,68*40,132*40,132*40,132*40,132*40,132*40,0
	dc.w	12*40,12*40,12*40,12*40,12*40,76*40,76*40,76*40,76*40,76*40,140*40,140*40,140*40,140*40,140*40,0
	dc.w	20*40,20*40,20*40,20*40,20*40,84*40,84*40,84*40,84*40,84*40,148*40,148*40,148*40,148*40,148*40,0
	dc.w	28*40,28*40,28*40,28*40,28*40,92*40,92*40,92*40,92*40,92*40,156*40,156*40,156*40,156*40,156*40,0
	dc.w	36*40,36*40,36*40,36*40,36*40,100*40,100*40,100*40,100*40,100*40,164*40,164*40,164*40,164*40,164*40,0
	dc.w	44*40,44*40,44*40,44*40,44*40,108*40,108*40,108*40,108*40,108*40,172*40,172*40,172*40,172*40,172*40,0
	dc.w	52*40,52*40,52*40,52*40,52*40,116*40,116*40,116*40,116*40,116*40,180*40,180*40,180*40,180*40,180*40,0
	dc.w	60*40,60*40,60*40,60*40,60*40,124*40,124*40,124*40,124*40,124*40,188*40,188*40,188*40,188*40,188*40,0
	dc.w	5*40,5*40,5*40,5*40,5*40,69*40,69*40,69*40,69*40,69*40,133*40,133*40,133*40,133*40,133*40,0
	dc.w	13*40,13*40,13*40,13*40,13*40,77*40,77*40,77*40,77*40,77*40,141*40,141*40,141*40,141*40,141*40,0
	dc.w	21*40,21*40,21*40,21*40,21*40,85*40,85*40,85*40,85*40,85*40,149*40,149*40,149*40,149*40,149*40,0
	dc.w	29*40,29*40,29*40,29*40,29*40,93*40,93*40,93*40,93*40,93*40,157*40,157*40,157*40,157*40,157*40,0
	dc.w	37*40,37*40,37*40,37*40,37*40,101*40,101*40,101*40,101*40,101*40,165*40,165*40,165*40,165*40,165*40,0
	dc.w	45*40,45*40,45*40,45*40,45*40,109*40,109*40,109*40,109*40,109*40,173*40,173*40,173*40,173*40,173*40,0
	dc.w	53*40,53*40,53*40,53*40,53*40,117*40,117*40,117*40,117*40,117*40,181*40,181*40,181*40,181*40,181*40,0
	dc.w	61*40,61*40,61*40,61*40,61*40,125*40,125*40,125*40,125*40,125*40,189*40,189*40,189*40,189*40,189*40,0
	dc.w	6*40,6*40,6*40,6*40,6*40,70*40,70*40,70*40,70*40,70*40,134*40,134*40,134*40,134*40,134*40,0
	dc.w	14*40,14*40,14*40,14*40,14*40,78*40,78*40,78*40,78*40,78*40,142*40,142*40,142*40,142*40,142*40,0
	dc.w	22*40,22*40,22*40,22*40,22*40,86*40,86*40,86*40,86*40,86*40,150*40,150*40,150*40,150*40,150*40,0
	dc.w	30*40,30*40,30*40,30*40,30*40,94*40,94*40,94*40,94*40,94*40,158*40,158*40,158*40,158*40,158*40,0
	dc.w	38*40,38*40,38*40,38*40,38*40,102*40,102*40,102*40,102*40,102*40,166*40,166*40,166*40,166*40,166*40,0
	dc.w	46*40,46*40,46*40,46*40,46*40,110*40,110*40,110*40,110*40,110*40,174*40,174*40,174*40,174*40,174*40,0
	dc.w	54*40,54*40,54*40,54*40,54*40,118*40,118*40,118*40,118*40,118*40,182*40,182*40,182*40,182*40,182*40,0
	dc.w	62*40,62*40,62*40,62*40,62*40,126*40,126*40,126*40,126*40,126*40,190*40,190*40,190*40,190*40,190*40,0
	dc.w	7*40,7*40,7*40,7*40,7*40,71*40,71*40,71*40,71*40,71*40,135*40,135*40,135*40,135*40,135*40,0
	dc.w	15*40,15*40,15*40,15*40,15*40,79*40,79*40,79*40,79*40,79*40,143*40,143*40,143*40,143*40,143*40,0
	dc.w	23*40,23*40,23*40,23*40,23*40,87*40,87*40,87*40,87*40,87*40,151*40,151*40,151*40,151*40,151*40,0
	dc.w	31*40,31*40,31*40,31*40,31*40,95*40,95*40,95*40,95*40,95*40,159*40,159*40,159*40,159*40,159*40,0
	dc.w	39*40,39*40,39*40,39*40,39*40,103*40,103*40,103*40,103*40,103*40,167*40,167*40,167*40,167*40,167*40,0
	dc.w	47*40,47*40,47*40,47*40,47*40,111*40,111*40,111*40,111*40,111*40,175*40,175*40,175*40,175*40,175*40,0
	dc.w	55*40,55*40,55*40,55*40,55*40,119*40,119*40,119*40,119*40,119*40,183*40,183*40,183*40,183*40,183*40,0
	dc.w	63*40,63*40,63*40,63*40,63*40,127*40,127*40,127*40,127*40,127*40,191*40,191*40,191*40,191*40,191*40,0
