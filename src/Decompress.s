	SECTION	APPLEII,CODE
*** NOTE ***
* The Destination drive is chosen by putting the disk buffer address in the
* the global var Dest_Disk_Buffer !!!!! 


**************************************
*
* DecompressDisk:
* This function does an entire decompression & encoding of a disk file
* compressed with "Dalton Disk Disintegrator 2.0" to the emulated 5 1/4" floppy.
*
* Inputs: D_FileSize = total length of compressedData (including header)
* 	  CompressedData (with prodos header) at mem ptd by InstTbl_Var
*
* Output: Floppy Image in disk_Buffer memory...
*	

	even
DecompressDisk:
	movem.l	d2-d4/a2-a5,-(sp)

	jsr	Decomp_Init
	
	move.l	#3,d0				;should be a 0...
	bsr	ReadNBitsOld

	move.l	#8,d0
	bsr	ReadNBitsOld

	lea	ReverseByteTbl,a0
	and.l	#$00ff,d0
	move.b	(a0,d0.w),d0			;ReverseByte

	move.l	d0,D_VolumeNum

	******************
	move.l	#0,d3				;D3 = Track #
.lp35tk	
	lea	.TrackBuffer,a0
	bsr	DecompressTrackData		;decompress track data...

	move.l	#15,d2				;D2 = Sector #	

.lp16	lea	.TrackBuffer,a0
	move.l	d2,d0
	mulu.w	#$100,d0
	lea	(a0,d0.w),a0	;get offset into track data...
	
	bsr	Nibbleize		;take page at A0 & prepare it for encoding (rtns A0)

	move.l	d3,d0
	move.l	d2,d1
	bsr	EncodeSector		;Encode Nibble Data into disk image...

	dbf	d2,.lp16		;do all sectors on this track...

	addq.b	#1,d3			;do all 35 tracks...
	cmp.b	#34,d3
	bls.b	.lp35tk
	
	movem.l	(sp)+,d2-d4/a2-a5
	rts

.TrackBuffer	ds.b	$3000
	EVEN
**************************************
* PlainDiskImage_Load:
* This function takes an plain disk image (standard Unix/PC disk file format)
* and builds a perfectly encoded 16 sector disk image in memory...
*
* Note: A "plain disk image" is exactly 143,360 bytes long, and consists of
* the normal data bytes in sequential order (tracks 0 - 34, sectors 0 - 15)
* with NO header/trailer/checksums.
*
* Inputs: Plain image in buffer pointed to by InstTbl_Var
*
* Output: Floppy Image in disk_Buffer memory...
*	
	EVEN
	
PlainDiskImage_Load:
	movem.l	d2-d4/a2-a5,-(sp)

	jsr	Decomp_Init
	move.l	InstTbl_Var,D_Source	;shouldn't be any headers in this file...

	move.l	#254,D_VolumeNum

	******************
	move.l	#0,d3			;d3 = Track #
.lp35tk	
	move.l	#15,d2			;d2 = Sector #
.lp16	move.l	InstTbl_Var,a0
	move.l	d3,d0
	mulu.w	#$1000,d0	;trk offset
	lea	(a0,d0.l),a0
	move.l	d2,d0		;sect offset
	mulu.w	#$100,d0
	lea	(a0,d0.w),a0		;get offset into track data...
	
	bsr	Nibbleize		;take page at A0 at prepare it for encoding

	move.l	d3,d0		;trk
	move.l	d2,d1		;sec
	bsr	EncodeSector		;Auto-Inc'ing write to floppy area...

	dbf	d2,.lp16		;do all 16 sectors in each track....

	addq.b	#1,d3			;do all 35 tracks...
	cmp.b	#34,d3
	bls.b	.lp35tk

	movem.l	(sp)+,d2-d4/a2-a5
	
	rts


	even

	even

****************************************
* EncodeSector:           (see dos 3.3 code at $b82a)
* Takes 342 bytes of "Nibble-ized" data and encodes data to
* disk drive. (+ sector header/trailers, etc).
*
* WARNING! This function writes sector data into a specific offset in a track,
* NOT 1st searching for an area to replace! Only use to write ALL 16 sectors of a track.
*
* Inputs: Floppy Image in disk_Buffer memory (normal)
*	  D_VolumeNum = disk Volume #
*	  A0 -> 342 bytes of Nibbilized data
*	  D0 = Track number (0 - 34)
*	  D1 = Sector number (0 - 15)
*
* Output: One sector of disk data encoded to "Dest_Buffer" !!! (either buffer 1 or 2)
* Note: Rewrite these damn routines to handle simple source/dest ptrs later!!!!!

EncodeSector:
						;do any track/sector error checking here...!
	movem.l	d2-d4/a2-a5,-(sp)

	move.l	a0,a5				;a5-> NibbleHunk Source
	move.l	d0,d3				;d3 = track #
	move.l	d1,d4				;d4 = sector #

	move.l	Dest_Disk_Buffer,a4		;Compute Track offset
	mulu.w	#disk_TrackLen,d0
	lea	(a4,d0.l),a4

	move.w	(SectorReMapTbl.l,d4.w*2),d1	;and sector offset
	mulu.w	#$18a,d1
	lea	(a4,d1.l),a4			;a4 -> Hard sector offset in disk memory

	move.w	#22/2-1,d0			;write 22 FF's (sync field)  (MUST BE < 26!!)
.sync1	move.w	#$ffff,(a4)+
	dbf	d0,.sync1

.head1	move.l	#$ffd5aa96,(a4)+		;FF + addr field header D5 AA 96     [26]

						;*** 4X4 encode the following: ***
.Vol	move.l	D_VolumeNum,d0			;Volume #
	move.l	d0,d2		;chksum
	bsr	.Do4x4
.Trk	move.l	d3,d0				;Track #
	eor.l	d0,d2
	bsr.b	.Do4x4
.Sec	move.l	d4,d0				;Soft Sector num...
	lea	SectorReMapTbl,a0		;remap soft to hard sector #
	move.w	(a0,d0.w*2),d0
	eor.l	d0,d2
	bsr.b	.Do4x4
.ChkSum	move.l	d2,d0
	bsr.b	.Do4x4				;EOR'd checksum                   [34 bytes]
	
.trail1	move.l	#$deaaebff,(a4)+		;addr field trailer DE AA EB...
.sync2	move.l	#$ffffffff,(a4)+		;5 FF's (sync field, exactly 5!)
.head2	move.w	#$d5aa,(a4)+			;Data header D5 AA AD             [45 bytes]
	move.b	#$ad,(a4)+
	
	lea	$100(a5),a2		;a2 -> NibbleHunk2
	lea	EncodeLkUpTbl,a3	;a3 -> LookUpTable (apple $ba29) (6 bit val -> disk num)

	moveq.l	#0,d2			;d2 = Accumulator
	moveq.l	#$56,d3			;d3 = YReg

	bra.b	.first			;Now go do 343 bytes of Data!!!

.lp1	move.b	(a2,d3),d2	;lda $bc00,y
.first	move.b	-1(a2,d3),d4	;eor $bbff,y
	eor.b	d4,d2
	move.b	(a3,d2),(a4)+	;tax, lda $ba29,x, write it
	subq.b	#1,d3		;dey
	bne.b	.lp1
	
	move.b	(a2),d2		;lda $bc00
	move.l	a5,a2		;a2 -> NibbleHunk

.lp2	move.b	(a2,d3),d4	;eor $bb00,y
	eor.b	d4,d2
	move.b	(a3,d2),(a4)+	;tax, lda $ba29,x, write it
	move.b	(a2,d3),d2	;lda $bb00,y
	addq.b	#1,d3		;iny
	bne.b	.lp2
	move.b	(a3,d2),(a4)+	;tax, lda $ba29,x, write it   (jsr $b8bb) last byte

.trail2	move.w	#$deaa,(a4)+	;data field trailer DE AA EB...                 [391 bytes]
	move.b	#$eb,(a4)+

	move.w	#$ffff,(a4)+	;fill in the rest like dos 3.3 does...          [394 bytes]
	move.b	#$eb,(a4)+
		
	movem.l	(sp)+,d2-d4/a2-a5
	rts

.Do4x4	move.b	d0,d1				;do 4X4 encoding of byte in D0!!!
	lsl.w	#7,d0
	move.b	d1,d0
	or.w	#$aaaa,d0
	move.w	d0,(a4)+
	rts


;.Do4x4	move.b	d0,d1				;OLD 4X4 encoding of byte in D0!!!
	lsr.b	#1,d0
	or.b	#$aa,d0
	move.b	d0,(a4)+
	or.b	#$aa,d1
	move.b	d1,(a4)+
	rts


EncodeLkUpTbl:		;($ba29 - $ba68 in Dos 3.3)
	dc.b	$96,$97,$9A,$9B,$9D,$9E,$9F,$A6,$A7,$AB,$AC,$AD,$AE,$AF
	dc.b	$B2,$B3,$B4,$B5,$B6,$B7,$B9,$BA,$BB,$BC,$BD,$BE,$BF,$CB
	dc.b	$CD,$CE,$CF,$D3,$D6,$D7,$D9,$DA,$DB,$DC,$DD,$DE,$DF,$E5
	dc.b	$E6,$E7,$E9,$EA,$EB,$EC,$ED,$EE,$EF,$F2,$F3,$F4,$F5,$F6
	dc.b	$F7,$F9,$FA,$FB,$FC,$FD,$FE,$FF

SectorReMapTbl:		;(soft to hard remap, $bfb8-$bfc7 in Dos 3.3)
	dc.w	$00,$0d,$0b,$09,$07,$05,$03,$01,$0e,$0c,$0a,$08,$06,$04,$02,$0f	
	dc.w	$00,$0d,$0b,$09,$07,$05,$03,$01,$0e,$0c,$0a,$08,$06,$04,$02,$0f	
	even

*****************************************
* Nibble-ize:		(see dos 3.3 code at $b800)
* Takes a page (256 bytes) of memory & translates it into 342 bytes of "6 & 2" disk
* encoded crap as done by Apple 5 1/4" dos routines.
*
* SrcData:    Nib1:    Nib2:        (note wrap around of sourcedata & redundant bits
* $ff-+-  v   (same)                for first 2 entries into Nib2)
*     |   v 
*     |   v            $55-+- ^^o
*     |  ov                |  ^^^
* $00-+- vo            $00-+- o^^
*
* Inputs: a0 = addr of page of memory
* Return: a0 = addr of 342 'nibble-ized' bytes (note: this is an internal static buffer)


Nibbleize:
	movem.l	d2-d4/a2-a4,-(sp)

	lea	.NibbleHunk,a1			;a1-> NibbleHunk1[$00] ($100)
	lea	.NibbleHunk2,a2			;a2-> NibbleHunk2[$00] ($56)

*						;*** Do 1st wrap-around case ***
	moveq.l	#0,d1				;d1- shifted 2 bits from each of 3 #'s
	move.b	1(a0),d0			;First of 3
	lsr.b	#1,d0
	roxl.b	#1,d1
	lsr.b	#1,d0
	roxl.b	#1,d1				;IGNORE these remaining 6 bits...

	move.b	$ab(a0),d0			;2 of 3
	lsr.b	#1,d0
	roxl.b	#1,d1
	lsr.b	#1,d0
	roxl.b	#1,d1
	move.b	d0,$ab(a1)			;and keep remaining 6 bits...
	
	move.b	$55(a0),d0			;3 of 3
	lsr.b	#1,d0
	roxl.b	#1,d1
	lsr.b	#1,d0
	roxl.b	#1,d1
	move.b	d0,$55(a1)			;and keep remaining 6 bits...
	move.b	d1,(a2)+			;and keep those 6 packed bits...
	
*						;*** Do 2nd wrap-around case ***
	moveq.l	#0,d1				;d1- shifted 2 bits from each of 3 #'s
	move.b	(a0),d0				;First of 3
	lsr.b	#1,d0
	roxl.b	#1,d1
	lsr.b	#1,d0
	roxl.b	#1,d1				;IGNORE remaining 6 bits...

	move.b	$aa(a0),d0			;2 of 3
	lsr.b	#1,d0
	roxl.b	#1,d1
	lsr.b	#1,d0
	roxl.b	#1,d1
	move.b	d0,$aa(a1)			;and keep remaining 6 bits...
	
	move.b	$54(a0),d0			;3 of 3
	lsr.b	#1,d0
	roxl.b	#1,d1
	lsr.b	#1,d0
	roxl.b	#1,d1
	move.b	d0,$54(a1)			;and keep remaining 6 bits...
	move.b	d1,(a2)+			;and keep those 6 packed bits...
	
						;*** And do rest of them! ***
	lea	.NibbleHunk+$100,a1		;a1-> NibbleHunk1[$100]
						;a2-> NibbleHunk2[$02]  ($56)

	lea	$100(a0),a0			;a0-> SourceByte[$100]

	move.w	#$54-1,d3			;d3= countdown
.lp	moveq.l	#0,d1				;d1- shifted 2 bits from each of 3 #'s
	move.b	-(a0),d0			;First of 3
	lsr.b	#1,d0
	roxl.b	#1,d1
	lsr.b	#1,d0
	roxl.b	#1,d1
	move.b	d0,-(a1)			;and keep remaining 6 bits...

	move.b	-$56(a0),d0			;2 of 3
	lsr.b	#1,d0
	roxl.b	#1,d1
	lsr.b	#1,d0
	roxl.b	#1,d1
	move.b	d0,-$56(a1)			;and keep remaining 6 bits...
	
	move.b	-$56*2(a0),d0			;3 of 3
	lsr.b	#1,d0
	roxl.b	#1,d1
	lsr.b	#1,d0
	roxl.b	#1,d1
	move.b	d0,-$56*2(a1)			;and keep remaining 6 bits...
	
	move.b	d1,(a2)+			;and keep those 6 packed bits...
	
	dbf	d3,.lp
	lea	.NibbleHunk,a0
	movem.l	(sp)+,d2-d4/a2-a4
	rts

	even

.NibbleHunk	ds.b	256	;342 bytes of storage space...
.NibbleHunk2	ds.b	86
.NibblePadding	ds.b	8
	even

******************************************
*
*read into D_TrackMem
*
* Inputs: A0 -> Track Data dest buffer... (AT LEAST large enough to hold track data!)

DecompressTrackData:
	movem.l	d2-d4/d6/a2-a6,-(sp)

						;********** Function constants ***********
	move.l	a0,a5				;A5-> Track Dest ptr (for entire function)

	lea	ReverseByteTbl,a4		;A4-> Reverse Byte Lookup table... (bytes[])

	lea	.Top_Values,a2			;A2-> Values[] (for entire function)
	move.l	#16*256-1,d3			;d4 = Num of bytes to read (incld. 0)

	move.l	D_Source,a6			;A6-> DataSource (entire function)
	move.l	D_SourceBit,d6			;D6 = Bit offset (entire function)


	move.l	#19,d2				;*** Read 20 most frequent bytes for track ***
.vals	bfextu	(a6){d6:8},d0			;Read 8 Bits from stream...
	addq.l	#8,d6
	move.b	(a4,d0.w),(a2,d2.w)		;ReverseByte & add to list...
	dbf	d2,.vals

.StartNextDecode
	bfextu	(a6){d6:1},d0			;read first bit from stream...
	addq.l	#1,d6
	tst.l	d0
	bne.b	.ReadCode			;if bit=0 then read unique byte, else code...

.ReadUniqueByte
	bfextu	(a6){d6:8},d0			;*** bit=0; read 8 bits as a unique byte ***
	addq.l	#8,d6
	move.b	(a4,d0.w),(a5)+			;ReverseByte & AddByte

	subq.l	#1,d3				;More to read?
	bpl.b	.StartNextDecode
	bra.b	.done

.ReadCode:
	lea	.CmpList,a3			;a3 = addr of CmpList (rdCode section only)
	move.l	#19,d4				;d4 = Index (of Values[]) (rdCode section only)
	
	moveq.l	#0,d0
	
.CodeLp	move.b	(a3)+,d1			;cmpList
	bpl.b	.CdVal

.Cd1Bit	lsl.b	#1,d0				;cmpTbl=-1 *** Read another bit into code ***
	move.b	d0,-(sp)
	bfextu	(a6){d6:1},d0			;Read 1 Bit from stream...
	addq.l	#1,d6
	or.b	(sp)+,d0
	bra.b	.CodeLp				;and repeat
	
.CdVal	cmp.b	d1,d0				;it the code so far = the val in table?
	beq.b	.match
	subq.l	#1,d4
	bpl.b	.CodeLp				;go again if still possible table match...

	bfextu	(a6){d6:1},d0			;else read 1 last bit from stream (discarded)
	addq.l	#1,d6

.repeat						;*** REPEATing byte case ***
	bfextu	(a6){d6:8},d0			;Read 8 Bits from stream...
	addq.l	#8,d6
	move.b	(a4,d0.w),d2			;ReverseByte, d2 = RepeatValue
	bfextu	(a6){d6:8},d4			;Read 8 Bits from stream... (into d4)
	addq.l	#8,d6
	move.b	(a4,d4.w),d4			;ReverseByte, d4 = Count (0=255)
	subq.b	#1,d4				;Prepare it for DBF countdown (to -1)
.rptLp	move.b	d2,(a5)+			;AddByte 
	subq.l	#1,d3
	dbmi	d4,.rptLp			;Do 'x' times or until max bytes read!

	tst.l	d3				;More to read?
	bpl.b	.StartNextDecode
	bra.b	.done
	
.match	move.b	(a2,d4),(a5)+			;else cmpList[]==code; use Values[d4] & AddByte 

	subq.l	#1,d3				;More to read?
	bpl.b	.StartNextDecode
;	bra.b	.done

.done
	move.l	d6,D_SourceBit			;D6 = Bit offset (entire function)


	movem.l	(sp)+,d2-d4/d6/a2-a6
	rts


.Top_Values	ds.b	20

.CmpList	dc.b	-1,-1,-1,4,1,-1,15,14,12,11,10,6,5,-1
		dc.b	27,15,9,8,3,2,1,0,-1,53,29,28,-1,-2	;-1 & -2 no longer needed...


	CNOP	0,4

************************************
* Inputs: d0 = # of bits to read (normally 8, 3, 2, or 1)
* Return: d0 = bits read (low order)

ReadNBitsOld:

	move.l	D_Source,a0
	move.l	D_SourceBit,d1
	add.l	d0,D_SourceBit		;inc bit cntr by bits read...
	bfextu	(a0){d1:d0},d0

	rts


ReverseByteTbl: *** This is a lookup table of bytes with bit order reversed ***
	dc.b	$00,$80,$40,$c0,$20,$a0,$60,$e0,$10,$90,$50,$d0,$30,$b0,$70,$f0
	dc.b	$08,$88,$48,$c8,$28,$a8,$68,$e8,$18,$98,$58,$d8,$38,$b8,$78,$f8
	dc.b	$04,$84,$44,$c4,$24,$a4,$64,$e4,$14,$94,$54,$d4,$34,$b4,$74,$f4
	dc.b	$0c,$8c,$4c,$cc,$2c,$ac,$6c,$ec,$1c,$9c,$5c,$dc,$3c,$bc,$7c,$fc
	dc.b	$02,$82,$42,$c2,$22,$a2,$62,$e2,$12,$92,$52,$d2,$32,$b2,$72,$f2
	dc.b	$0a,$8a,$4a,$ca,$2a,$aa,$6a,$ea,$1a,$9a,$5a,$da,$3a,$ba,$7a,$fa
	dc.b	$06,$86,$46,$c6,$26,$a6,$66,$e6,$16,$96,$56,$d6,$36,$b6,$76,$f6
	dc.b	$0e,$8e,$4e,$ce,$2e,$ae,$6e,$ee,$1e,$9e,$5e,$de,$3e,$be,$7e,$fe
	dc.b	$01,$81,$41,$c1,$21,$a1,$61,$e1,$11,$91,$51,$d1,$31,$b1,$71,$f1
	dc.b	$09,$89,$49,$c9,$29,$a9,$69,$e9,$19,$99,$59,$d9,$39,$b9,$79,$f9
	dc.b	$05,$85,$45,$c5,$25,$a5,$65,$e5,$15,$95,$55,$d5,$35,$b5,$75,$f5
	dc.b	$0d,$8d,$4d,$cd,$2d,$ad,$6d,$ed,$1d,$9d,$5d,$dd,$3d,$bd,$7d,$fd
	dc.b	$03,$83,$43,$c3,$23,$a3,$63,$e3,$13,$93,$53,$d3,$33,$b3,$73,$f3
	dc.b	$0b,$8b,$4b,$cb,$2b,$ab,$6b,$eb,$1b,$9b,$5b,$db,$3b,$bb,$7b,$fb
	dc.b	$07,$87,$47,$c7,$27,$a7,$67,$e7,$17,$97,$57,$d7,$37,$b7,$77,$f7
	dc.b	$0f,$8f,$4f,$cf,$2f,$af,$6f,$ef,$1f,$9f,$5f,$df,$3f,$bf,$7f,$ff
	even

*************************************
* Inputs: "D_FileSize"
*
* Output: Sets up "D_Source" "D_SourceBit" "D_SourceBitEnd"

Decomp_Init:

	move.l	InstTbl_Var,D_Source
.Header	move.l	InstTbl_Var,a0
	cmp.w	#$0a47,(a0)		;first 2 bytes in ProDos header...
	bne	.Dos3_3
	cmp.b	#$4c,2(a0)		;Third byte in ProDos header...
	bne	.Dos3_3

.ProDos	add.l	#$7c,D_Source		;ProDos header; add $80 bytes...
.Dos3_3	add.l	#$04,D_Source		;Dos 3.3; 4 byte header...

	move.l	#0,D_SourceBit

	move.l	D_FileSize,d0			;do we ever set this??? FIX LATER!!!
	sub.l	#$80,d0
	mulu.l	#8,d0
	move.l	d0,D_SourceBitEnd
	rts

* Globals for these functions...   *
************************************
D_Source	ds.l	1		;Source addr of decompression data...
D_SourceBit	ds.l	1		;Source bit offset of data...
D_SourceBitEnd	ds.l	1		;Highest bit offset allowable...

D_FileSize	ds.l	1		;Size in Bytes of file read...
D_VolumeNum	ds.l	1		;Volume # of disk...
;D_TrackNum	ds.l	1
;D_SectorNum	ds.l	1

*************************************
	even
