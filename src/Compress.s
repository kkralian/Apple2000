	SECTION	APPLEII,CODE
	EVEN

** NOTE ** Disk image is extraced from "Src_Disk_Buffer" (either buffer1 or buffer2)!!!

**************************************
*
* CompressDisk:
* This function does an entire Decoding & Compression of a disk image,
* using the "Dalton Disk Disintegrator 2.0" algorithm. (see DDD, $1b41-$1b86, $1bc6...)
*
* Inputs: Floppy Image in disk_Buffer memory
*	  A0 -> Destination Buffer (at least 145,000 bytes to be safe)
*
* Output: Compressed Disk Image (DDD) in Destination Buffer w/ simple ProDos header
*	  D0 = FileSize; Total length of compressedData (including header) (0 if failed)
*	

CompressDisk:

	movem.l	a2-a6/d2-d5,-(sp)
	
	move.l	a0,a5				;A5-> Base of Dest Buffer!
	moveq.l	#0,d5				;D5 = Dest buffer bit offset!
	lea	ReverseByteTbl,a6		;a6-> Reverse byte lookup tbl

	
	move.l	#$00000000,(a5)			;Make blank 4 byte Dos3.3 header
	add.l	#32,d5

	move.l	#0,.TrackNum
	
.loop	move.l	.TrackNum,d0		;trk num		<----- main loop
	lea	.TrackData,a0		;dest buffer
	jsr	ReadTrack
	tst	d0
	beq	.Error
	
	lea	.TrackData,a0		;track data
	lea	.FreqTable,a1		;dest for Frequency Count Tbl
	jsr	FrequencyCount	

	tst.l	.TrackNum
	bne	.Not0
.Trk0	moveq.b	#0,d0		;val     Encode DDD header (Three 0 bits)
	moveq.w	#3,d1		;# of bits
	bsr	.WriteNBits
	moveq.l	#0,d0
	move.b	SecVolumeNumB,d0	;Encode Volume # of disk
	move.b	(a6,d0.w),d0		;ReverseByte & add 8 bits at once...
	bfins	d0,(a5){d5:8}
	addq.l	#8,d5
.Not0	

	lea	.FreqTable,a0		;freq count table
	lea	.Top20List,a1		;top 20 values list destination
	bsr	Top20Search

	lea	.Top20List,a2
	move.w	#19,d2			;Encode the 20 top values...
	moveq.l	#0,d0
.Encd20 move.b	(a2)+,d0
	move.b	(a6,d0.w),d0		;ReverseByte & add 8 bits at once...
	bfins	d0,(a5){d5:8}
	addq.l	#8,d5
	dbf	d2,.Encd20	

	;-------------------
.CompressTrackData			;The real compression algorithm, using top 20 values

	lea	.TrackData,a2			;A2 -> TrackData (4096 bytes)
	moveq.w	#0,d2				;D2 = main index into track data

.CompLp	move.b	(a2,d2.w),d0			;read 1st val

	move.w	d2,d3			;d3 = local index. How many times does D0 repeat?
.Count	cmp.b	(a2,d3.w),d0	;1st check against self to maintain bounds check & get >=1
	bne.b	5$
	addq.w	#1,d3
	cmp.w	#4095,d3			;still in range?
	bls.b	.Count

5$	sub.w	d2,d3				;d3 = # of times byte D0 is there (at least 1)!
	cmp.w	#4,d3
	blo.b	.Single

.Multiple					;------- value D0 repeats D3 times...  (D3 >= #4)
	cmp.w	#$100,d3
	bls.b	10$
	move.w	#$100,d3			;limit of $100 repeat values (written as #$00)

10$	move.b	d0,d1				;save value temporarily...
	move.w	#$97,d0				;Encode code for "Repeat"
	move.b	(a6,d0.w),d0			;ReverseByte & add 8 bits at once...
	bfins	d0,(a5){d5:8}
	addq.l	#8,d5
	
	move.b	d1,d0				;Encode data value
	move.b	(a6,d0.w),d0			;ReverseByte & add 8 bits at once...
	bfins	d0,(a5){d5:8}
	addq.l	#8,d5
	
	move.b	d3,d0				;Encode # of times repeats
	move.b	(a6,d0.w),d0			;ReverseByte & add 8 bits at once...
	bfins	d0,(a5){d5:8}
	addq.l	#8,d5

	add.w	d3,d2				;update main index into track data...
	bra	100$

.Single						;------- single value D0...
	move.w	#19,d3				;d3 = index to search Top20List with (19 -> 0)
	lea	.Top20List,a0			;a0 -> Top20List, UBYTES[20]
20$	cmp.b	(a0,d3.w),d0
	beq.b	.SingleTop20			;is D0 a top 20 value?
	dbf	d3,20$
						;Not top20, so encode this byte by itself...
	move.b	d0,-(sp)
	move.b	#0,d0
	moveq.b	#1,d1				;Encode "SingleByte" code
	bsr	.WriteNBits
	
	moveq.l	#0,d0
	move.b	(sp)+,d0
	move.b	(a6,d0.w),d0			;ReverseByte & add 8 bits at once...
	bfins	d0,(a5){d5:8}
	addq.l	#8,d5
	
	bra	99$

.SingleTop20					;-- single value is a Top 20 value! UBYTE[d3.w]
	lea	.Top20Codes,a0
	move.b	(a0,d3.w),d0
	lea	.Top20Lengths,a0
	move.b	(a0,d3.w),d1
	bsr	.WriteNBits
	
;	bra	99$ (fall through!)

99$	addq.w	#1,d2
100$	cmp.w	#4095,d2
	bls	.CompLp				;Do all 4096 bytes of track data...

;---------------------	

	addq.l	#1,.TrackNum
	cmp.l	#34,.TrackNum			;Do all 35 tracks!
	bls	.loop		
						;Disk image compressed, now report len in bytes
						;D5 = Length in bits...
	addq.l	#7,d5				;we want any partial bytes...
	divu.l	#8,d5				;d5 = Length in bytes!
	
	move.l	d5,d0				;lenth of compression!
	movem.l	(sp)+,a2-a6/d2-d5
	rts

.Error	movem.l	(sp)+,a2-a6/d2-d5
	moveq.l	#0,d0
	rts

.WriteNBits:		;Enter: d0 = Data to write (low order bits), d1 = # of bits to write
			;	a5 -> Base of destination memory, d5 = Offset in BITS...
			;Output: Memory written, updated Offset (d5)
			;		(see DDD $1d28 - $1d54)

	subq.b	#1,d1
	and.w	#$000f,d1
.lp	bfins	d0,(a5){d5:1}		;place low order bit in memory
	lsr.l	d0
	addq.l	#1,d5			;1 at a time until done...
	dbf	d1,.lp
	rts

	CNOP	0,4

.Top20Codes	dc.b	$03,$09,$1f,$0f,$07,$1b,$0b,$0d,$15,$37		;(see DDD $1d55-$1d7c)
		dc.b	$3d,$25,$05,$b1,$11,$21,$01,$57,$5d,$1d
.Top20Lengths	dc.b	$04,$04,$05,$05,$05,$05,$05,$05,$05,$06
		dc.b	$06,$06,$06,$06,$06,$06,$06,$07,$07,$07

.TrackNum	ds.l	1

.TrackData	ds.b	4096
.FreqTable	ds.w	256
.Top20List	ds.b	20

	even
**************************************
*
* PlainDiskImage_Save:
* This function takes a disk image in ram, and saved it to disk in the "plain image"
* format, which is the normalilzed track/sector data (143,360 bytes) in order.
*
* Inputs: Floppy Image in disk_Buffer memory
*	  A0 -> Destination Buffer (at least 143,360 bytes to be safe)
*
* Output: Plain Image Data in Destination Buffer w/ no headers/trailor/checksums
*	  D0 = FileSize; Total length of data (should be 143,360) (0 if failed)
*	

PlainDiskImage_Save:

	movem.l	a2-a5/d2-d5,-(sp)
	
	move.l	a0,a5				;A5-> Base of Dest Buffer!
	moveq.l	#0,d5				;D5 = Dest buffer bit offset!
	
	move.l	#0,.TrackNum
	
.loop	move.l	.TrackNum,d0			;trk num		<----- main loop
	
	move.l	d0,d1
	mulu.w	#4096,d1
	lea	(a5,d1.l),a0			;A0 = dest buffer...	
	jsr	ReadTrack			;fix dest buffer
	tst	d0
	beq	.Error
	

	addq.l	#1,.TrackNum
	cmp.l	#34,.TrackNum			;Do all 35 tracks!
	bls	.loop		
						;Disk image compressed, now report len in bytes
	move.l	#143360,d0			;d0 = lenth of disk data
	movem.l	(sp)+,a2-a5/d2-d5
	rts

.Error	movem.l	(sp)+,a2-a5/d2-d5
	moveq.l	#0,d0
	rts

	CNOP	0,4
.TrackNum	ds.l	1

.TrackData	ds.b	4096
.FreqTable	ds.w	256
.Top20List	ds.b	20
	EVEN
*****************************************
*
* ReadTrack:
* This function will extract 1 track of normalized data from a disk image, 
* using Apple Dos's normal decoding algorithms. (uses ReadSector)
* 
* Inputs: Floppy Image in disk_Buffer memory (normal)
*	  D0 = Track number to extract from
*	  A0 -> Destination for data (4096 bytes!)
*
* Output: 4096 bytes of sector data in dest. buffer
*	  D0 = True (1) for Successful extraction
*	       False (0) for ERROR
*

ReadTrack:
	movem.l	a2/d2-d3,-(sp)

	cmp.w	#34,d0
	bhi.b	.Error			;track out of range...
	
	move.l	d0,d2			;d2 = Track #
	moveq.l	#0,d3			;d3 = Sector #
	move.l	a0,a2			;a2 -> Ptr to destination buffer

.loop	move.l	d2,d0		;track num
	move.l	d3,d1		;sector num
	move.l	a2,a0		;dest buffer
	jsr	ReadSector
	tst	d0		;did it work?
	beq.b	.Error
	
	add.l	#$100,a2	;set dest to next page of mem
	addq.l	#1,d3		;inc sector
	cmp.b	#15,d3
	bls.b	.loop		;read all 16 sectors!
	
	moveq.l	#1,d0
	movem.l	(sp)+,a2/d2-d3
	rts

.Error	moveq.l	#0,d0
	movem.l	(sp)+,a2/d2-d3
	rts

;----------------------------------------------------------------------------------------

*****************************************
*
* ReadSector:
* This function will extract 1 sector of normalized data from a disk image, 
* using Apple Dos's normal decoding algorithms.
* 
* Inputs: Floppy Image in disk_Buffer memory (normal)
*	  D0 = Track number (0 - 34)
*	  D1 = Sector number (0 - 15)
*	  A0 -> Destination for data (256 bytes)
*
* Output: 256 bytes of sector data in dest. buffer
*	  D0 = True (1) for Successful extraction
*	       False (0) for ERROR
*

ReadSector:
	movem.l	a2-a5/d2-d4,-(sp)

	mulu.w	#disk_TrackLen,d0
	add.l	Src_Disk_Buffer,d0
	move.l	d0,a2			;a2 -> Start of raw track data
	move.l	a2,a3			;a3 -> CURRENT ptr to raw track data (a2 <= a3 <= a4)	
	lea	disk_TrackLen-1(a2),a4	;a4 -> Last byte of raw track data	
	move.l	d1,d4			;d4 = Sector # we want...
	move.l	#disk_TrackLen+500,d3	;d3 = Max "GetNext" searches allowed!
	bra.b	.addr

.NxtSec
	lea	345(a3),a3
	cmp.l	a4,a3			;make sure addr still in range
	bls.b	.addr
	sub.l	#disk_TrackLen,a3		;if not, reset it

.addr	jsr	.GetNext
	cmp.b	#$D5,d0				;Search for D5 AA 96 address header
	bne	.addr
	jsr	.GetNext
	cmp.b	#$AA,d0
	bne	.addr	
	jsr	.GetNext
	cmp.b	#$96,d0
	bne	.addr	
						;***** Extract 4X4 address data *****
	jsr	.Get4X4				;Volume Number
	move.b	d0,SecVolumeNumB

	jsr	.Get4X4				;Track Number

	jsr	.Get4X4				;HARD sector num...
	cmp.b	#$0f,d0	
	bhi.b	.addr				;outside legal range? Keep searching...
	move.b	(.HardToSoft.l,d0.w),d0		;d0 = SOFT sector num...
	cmp.b	d0,d4
	bne	.NxtSec				;doh! Not the right sector!

	jsr	.Get4X4				;Checksum  (ignore for now)
		
	jsr	.GetNext			;better get DE AA address epilog
	cmp.b	#$DE,d0
	bne	.addr	
	jsr	.GetNext
	cmp.b	#$AA,d0
	bne	.addr	

.data	jsr	.GetNext
	cmp.b	#$D5,d0				;Wait for D5 AA AD data header
	bne	.data
	jsr	.GetNext
	cmp.b	#$AA,d0
	bne	.data	
	jsr	.GetNext
	cmp.b	#$AD,d0
	bne	.data	

	moveq.w	#$55,d1				;d1 = cntr...
	lea	.NibbleHunk56+$56,a1		;a1 -> destination
	moveq.l	#0,d4				;d4 == ACC == running EOR checksum
	lea	.DecodeTable-$96,a5		;a5 -> DecodeTable (-$96)

	moveq.l	#0,d0				;See $b8ff -> $b911 in DOS 3.3
.loop	jsr	.GetNext			;LDY $c0ec
	move.b	(a5,d0.w),d0			;eor $ba00,y
	eor.b	d0,d4				;   ...
	move.b	d4,-(a1)			;sta $bc00,y   (y decrements)
	dbf	d1,.loop
	
	move.w	#$ff,d1				;d1 = cntr
	lea	.NibbleHunk100,a1		;a1 -> dest

.loop2	jsr	.GetNext			;See $b915 -> $b923
	move.b	(a5,d0.w),d0			;eor $ba00,y
	eor.b	d0,d4				;
	move.b	d4,(a1)+			;sta $bb00,y (y inc's)
	dbf	d1,.loop2

.cksum	jsr	.GetNext			;See $b925  (checksum)
	cmp.b	(a5,d0.w),d4			;cmp $ba00,y
	bne	.Error

	jsr	.GetNext			;better get DE AA data epilog
	cmp.b	#$DE,d0
	bne	.Error
	jsr	.GetNext
	cmp.b	#$AA,d0
	bne	.Error	

* DeNibble-ize:		(see dos 3.3 code at $b800)
* Takes 342 bytes of "6 & 2" disk encoded crap as returns 256 bytes of normal data.
*
*     Nib1:      Nib2:          Dest:
*   $ff-+- o                    $ff-+- o
*       |  ^     $55-+- vvo         |  ^
*       |  ^         |  vvv         |  ^
*       |  ^         |  vvv         |  ^
*   $00-+- o     $00-+- ovv     $00-+- o
*
* Inputs: a0 = destination for 256 bytes of data
*         NibbleHunk100 & 56 = Contain nibble data

				;at this point, all nibble data is read...!
				;Now, DE-NIBBLIZE !!! (dos 3.3 $b8c2 - $b8db)
				; d1 = YReg, d2 = XReg, d3 = ACC

	lea	.NibbleHunk100,a2		;a2-> NibbleHunk1[$00]  ($100)
	lea	.NibbleHunk56,a3			;a3-> NibbleHunk2[$00]  ($56)
	move.l	a0,a4				;a4-> Dest buffer (256 bytes)
	move.w	#$00ff,d1			;d1 = Byte Countdown
	
.DeNibl
.b8c4	move.w	#$56,d2			;LDX #$56
.b8c6	subq.b	#1,d2			;DEX
	bmi.b	.b8c4			;BMI $b8c4
	move.b	(a2)+,d3 		;LDA $bb00,Y
	move.b	(a3,d2.w),d0
	lsr.b	d0			;LSR $bc00,X
	roxl.b	d3			;ROL
	lsr.b	d0			;LSR $bc00,X
	roxl.b	d3			;ROL
	move.b	d0,(a3,d2.w)
	move.b	d3,(a4)+			;store another byte of normal data!
	dbf	d1,.b8c6	

	movem.l	(sp)+,a2-a5/d2-d4
	move.l	#1,d0			;success...
	rts

.GetNext:		;a2 -> Start track data, a3 -> Current track data
			;a4 -> Max track data ptr, d3=remaining tries
			; Return: d0 = byte
	move.b	(a3)+,d0
	cmp.l	a4,a3		;make sure addr still in range
	bls.b	.ok
	move.l	a2,a3		;if not, reset it
.ok	subq.l	#1,d3
	bmi	.GNErr
	rts

.GNErr	move.l	(sp)+,d0	;Exceeded # of tries, POP STACK, and fall to error...
	

.Error	moveq.l	#0,d0		;Failed (d0 = False)....
	movem.l	(sp)+,a2-a5/d2-d4
	rts

.Get4X4
			;a2 -> Start track data, a3 -> Current track data
			;a4 -> Max track data ptr
			;Return: d0 = byte
	moveq.l	#0,d0
	move.b	(a3)+,d0
	cmp.l	a4,a3		;make sure addr still in range
	bls.b	.ok3
	move.l	a2,a3		;if not, reset it
.ok3	lsl.b	d0
	or.b	#$01,d0

	and.b	(a3)+,d0
	cmp.l	a4,a3		;make sure addr still in range
	bls.b	.ok4
	move.l	a2,a3		;if not, reset it
.ok4	rts



.HardToSoft	dc.b	$0,$7,$e,$6,$d,$5,$c,$4,$b,$3,$a,$2,$9,$1,$8,$f

			;The lookup table used as $ba00,y ALWAYS had Y >= $96,
			;so here is the pertinant portion of the table ($ba96-$baff)
			;Make sure to subtract $96 when getting the address of it.
.DecodeTable:
	dc.b	$00,$01,$98,$99,$02,$03,$9C,$04,$05,$06,$A0,$A1,$A2,$A3,$A4
	dc.b	$A5,$07,$08,$A8,$A9,$AA,$09,$0A,$0B,$0C,$0D,$B0,$B1,$0E,$0F
	dc.b	$10,$11,$12,$13,$B8,$14,$15,$16,$17,$18,$19,$1A,$C0,$C1,$C2
	dc.b	$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$1B,$CC,$1C,$1D,$1E,$D0,$D1
	dc.b	$D2,$1F,$D4,$D5,$20,$21,$D8,$22,$23,$24,$25,$26,$27,$28,$E0
	dc.b	$E1,$E2,$E3,$E4,$29,$2A,$2B,$E8,$2C,$2D,$2E,$2F,$30,$31,$32
	dc.b	$F0,$F1,$33,$34,$35,$36,$37,$38,$F8,$39,$3A,$3B,$3C,$3D,$3E,$3F

.NibbleHunk100	ds.b	$100	;342 bytes of storage space...
.NibbleHunk56	ds.b	$56
.NibblePad	dc.b	$ff,$ff,$ff,$ff

SecVolumeNumB	ds.b	1

;---------------------------------------------------------------------------------------

	EVEN
*****************************************
*
* FrequencyCount:	(see DDD $1bc6 - $1c2b)
* This function will scan through 4096 bytes of data, counting occurances
* of each byte value and will create a list of UWORDS[256].
* As in DDD, any Repeating value of 4 or more will be skipped.
*
* Inputs: A0 -> 4096 bytes of data to analyze
*	  A1 -> Destination for UWORDS[256] result (512 bytes!)
*
* Output: list of UWORDS[256] frequency count in dest. buffer
*

FrequencyCount:		;---- Produce list of most popular bytes...

	movem.l	a2-a5/d2-d5,-(sp)

	move.l	a0,a2				;a2 -> 4096 bytes of data
	move.l	a1,a3				;a3 -> Destination table of UWORDS[256]
	
	move.w	#255,d0
.ClrLp	move.w	#0,(a1,d0.w*2)			;clear dest table
	dbf	d0,.ClrLp

	move.w	#0,d2				;d2 = main index
	moveq.l	#0,d0
.MnLoop	move.b	(a2,d2.w),d0			;read 1st val

	move.w	d2,d3				;d3 = local index. How many times does D0 repeat?
.repeat	cmp.b	(a2,d3.w),d0
	bne.b	5$
	addq.w	#1,d3
	cmp.w	#4095,d3
	bls.b	.repeat

5$	sub.w	d2,d3			;d3 = # of times byte D0 is there (at least 1)!
	cmp.w	#4,d3
	bhs.b	.NoCnt
.Cnt	addq.w	#1,(a3,d0.w*2)
	move.w	#1,d3		;only inc 1 byte
.NoCnt	add.w	d3,d2

	cmp.w	#4095,d2
	bls.b	.MnLoop
	
	movem.l	(sp)+,a2-a5/d2-d5
	rts

*****************************************
*
* Top20Search:		(see DDD $1c2c - $1c75)
* This function will scan through the frequency list UWORDS[256], create a list
* of the top 20 values in UBYTE[20]. During each search, this function will examine
* each entry, and if >= current high number, replace the current high number with it.
*
* Inputs: A0 -> Source of frequency count UWORDS[256]
*	  A1 -> Destination for top 20 values UBYTES[20] (20 bytes!)
* Output: list of most popular values UBYTES[20] in dest. buffer
*

Top20Search:		;---- Produce list of most popular bytes...

	movem.l	a2-a5/d2-d5,-(sp)

	move.w	#0,d1			;D1 = value index # (0 -> 19) we are looking for
.loop20	moveq.l	#0,d0			;D0 = max frequency number found so far...
	move.w	#0,d2			;D2 = search index (0 - 255)

.search	cmp.w	(a0,d2.w*2),d0
	bhi.b	.nope
	move.w	(a0,d2.w*2),d0	    ;new max frequency number
	move.b	d2,(a1,d1.w)	    ;temporarily (?) keep the number in the table...

.nope	addq.b	#1,d2		    ;search/compare all 256 values...
	bne.b	.search
	
	move.b	(a1,d1.w),d2	    ;get that popular value...
	move.w	#0,(a0,d2.w*2)	    ;and blank its entry out in the freq table...
	
	addq.b	#1,d1		    ;get 20 values total...
	cmp.b	#19,d1
	bls.b	.loop20
	
	movem.l	(sp)+,a2-a5/d2-d5
	rts

	