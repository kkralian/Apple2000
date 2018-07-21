** Useless Header which simply contains Version string (does not get compressed **

	nop
	moveq.l	#0,d0
	nop
	rts
	dc.b	"$VER: Apple2000_v1.3 (68020+, non-MMU version)",0	
	even
	rts
		