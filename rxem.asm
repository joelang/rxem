;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;rx01 emulator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Copyright 2018 Joseph Lang
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This project emulates a large disk using the 
; DEC rx01 interface. it connects to a RXV11 
; Q-bus interface (M7946) 
;
; It is not software compatable with the real
; rx01 because:
;
; transfers are 512 bytes
; no sector interleave
; sector numbers are 0 to 255
; track numbers are 0 to 255
; (65536  blocks)
; the storage media is a single CF card
; that supports 2 disk images
; 12 bit mode not implemented (it could be)
;
; it requires a custom handler
;
;Whats on the board:
; atmega32 16 mhz.
; 16 mhz can oscillator
; buffers (74hct14,2n7000)
; connnectors,caps,resistors
; Thats all. It's a compact project
;
;
;AVR atmega32a fuses (avrdude syntax):
;-U lfuse:w:0xe0:m -U hfuse:w:0xd9:m
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;port A
; bidirectional: databus for CF card
;
;port B
; output: rxv11 control bits
; 0 done
; 1 txrq
; 2 err
; 3 out
; 4 shift
;
;port C
; output: CF card control bits
; 0 a0
; 1 a1
; 2 a2
; 3 cf_cs-
; 4 rd-
; 5 wr-
; 6 cf_rst-
; output: rxv11 data bit
; 7 rx data out
;
;port D
; inputs: rxv11 status bits
; 0
; 1
; 2 init (pos edge interrupt)
; 3 rx go
; 4
; 5
; 6 12 bit
; input: rxv11 data bit
; 7 rx data in
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;constants
;
; RX01 commands (not referenced in the code)
; CM_fill =	0x00
; CM_mty =	0x02
; CM_write =	0x04
; CM_read =	0x06
; CM_noop =	0x08
; CM_rdsr =	0x0a
; CM_wrdel =	0x0c
; CM_rdes =	0x0e
;
.equ CM_mask =	0x0e
;
;unit select bit
.equ CM_unit =	4
;	
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; rx01 error/status bits
.equ ES_ready =	0x80
.equ ES_idone =	0x04
.equ ES_pare =	0x02
.equ ES_crc =	0x01
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; rx01 status register bits
;
; none of the errors in SR_reg
; are possible for a CF card 
; so SR_reg will always be zero
; any CF card errors will be reported 
; as ES_crc in the ES_reg
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CF card registers (port C)
; ored with wr,rd,rst (0x70)
.equ CF_data =	0x70
.equ CF_feat =	0x71
.equ CF_err =	0x71
.equ CF_count =	0x72
.equ CF_lba0 =	0x73
.equ CF_lba1 =	0x74
.equ CF_lba2 =	0x75
.equ CF_lba3 =	0x76
.equ CF_sdh =	0x76
.equ CF_cmd =	0x77
.equ CF_stat =	0x77
;
;read,write and select are expected to be
;set or cleared by cbi sbi instructions
.equ CF_csel =	3
.equ CF_read =	4
.equ CF_write =	5
.equ CF_rst =	6
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CF card status register bits
.equ CF_busy	= 7	;CF busy bit
.equ CF_drq	= 3	;CF data request
.equ CF_errb	= 0	;error bit
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CF card commands/bits
.equ CF_8bit	= 0x01	;set 8bit feature
.equ CF_drv0	= 0xe0	;drive 0+lba mode
.equ CF_drv1	= 0xf0	;drive 1+lba mode
.equ CF_dread	= 0x20	;read command
.equ CF_dwrite	= 0x30	;write command
.equ CF_sfeat	= 0xef	;set features command
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;RX01 interface bits (port B out)
.equ RX_done =	0
.equ RX_txrq =	1
.equ RX_err =	2
.equ RX_out =	3
.equ RX_shift =	4
;
;RX01 interface bits (port D in)
.equ RX_init =	2
.equ RX_go =	3
.equ RX_12bit =	6
.equ RX_data =	7
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;resister variables
.def ACC = R16
.def TEMP = R17
.def FL_crit = R18
.def COUNT = R19
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;data variables
;
	.DSEG
	.ORG 0x100
BUFFER:	.BYTE 512	;CF card data buffer
UNIT:	.BYTE 1		;drive select
CUR_BL:	.BYTE 1		;block num low
CUR_BH:	.BYTE 1		;block num hi
ES_reg:	.BYTE 1		;error register
SR_reg:	.BYTE 1		;status register
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	.CSEG
	.ORG 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;power on reset
;interrupt vectors
	jmp	rx_exec		;1 reset
	jmp	irq0_srv	;2 int0
	jmp	dummy	;3 int1
	jmp	dummy	;4 int2
	jmp	dummy	;5 tmr2 cmp
	jmp	dummy	;6 tmr2 ovf
	jmp	dummy	;7 tmr1 cap
	jmp	dummy	;8 tmr1 cmpa
	jmp	dummy	;9 tmr1 cmpb
	jmp	dummy	;10 tmr1 ovf
	jmp	dummy	;11 tmr0 comp
	jmp	dummy	;12 tmr0 ovf
	jmp	dummy	;13 spi
	jmp	dummy	;14 usart rx
	jmp	dummy	;15 usart dre
	jmp	dummy	;16 usart tx
	jmp	dummy	;17 adc
	jmp	dummy	;18 eerdy
	jmp	dummy	;19 ana cmp
	jmp	dummy	;20 twi
	jmp	dummy	;21 spm rdy
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; unexpected int handler
; just crash
dummy:
	rjmp dummy
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;subroutine: get_reg
; get byte from CF card register
; R17= register (preserved)
; R16= byte (return)	
;
get_reg:
	ldi R16,0x00		;direction=in
	out DDRA,R16		;set port A DDR
	out PORTA,R16		;set port out to 0
	out PORTC,R17		;set reg select bits
	cbi PORTC,CF_read	;assert read (low)
	nop			;delay for read pulse
	nop			;width
	nop
	in R16,PINA		;read bus
	sbi PORTC,CF_read	;remove strobe
	ret
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;subroutine: put_reg
; send byte to CF card register
; R17= register (preserved)
; R16= byte (return)	
;
put_reg:
	out PORTC,R17		;select reg
	out PORTA,R16		;set data bus
	ldi R16,0xFF		;direction=out
	out DDRA,R16		;set port A DDR
	cbi PORTC,CF_write	;assert strobe (low)
	nop			;delay for write pulse
	nop			;width
	nop
	sbi PORTC,CF_write	;remove strobe
	ret
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;subroutine: delay
; about 1us per count
; kill time
; R17= delay count
;
delay:
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	dec R17
	brne delay
	ret
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;subroutine: rd_blk
; fill buffer from CF card
; block num set on entry
; R16= status (return)
;
rd_blk:
	call set_task		;set task registers
;
	ldi FL_crit,0xFF	;set critical flag
	ldi r17,CF_cmd		;point to cmd reg
	ldi r16,CF_dread
	call put_reg		;send read command
;
;wait for drq or not busy
rd_wait:
	ldi r17,CF_stat		;point to status reg
	call get_reg		;get status
	sbrc r16,CF_drq		;test DRQ bit
	rjmp rd_drq		;drq set?
	sbrc r16,CF_busy	;test BUSY bit
	rjmp rd_wait		;busy set?
;
;busy cleared so check error
rd_done:
	sbi PORTD,CF_csel	;deselect CF card
	tst FL_crit		;did init occur?
	breq rd_abt		;then abort 
	clr FL_crit		;
	andi r16,CF_errb	;mask to error bit
	ret			;good exit
;
rd_abt:
	rjmp init_sw		;jump to init	
;
;move data to buffer
;4 bytes per loop
rd_drq:
	ldi R19,128		;loop counter
	ldi r17,CF_data		;point to data reg
	ldi r28,low(buffer)	;point to CF buffer
	ldi r29,high(buffer)
cf_rdlp:
	call get_reg		;get byte
	st Y+,r16		;save byte
	call get_reg		;repeat
	st Y+,r16
	call get_reg
	st Y+,r16
	call get_reg
	st Y+,r16
	dec R19
	brne cf_rdlp		;loop till done
	call cf_nbsy		;wait for not busy
	rjmp rd_done		;check status
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;subroutine: wr_blk
; write buffer to CF card
; LBN set
; R16= status (return)
;
wr_blk:
	call set_task		;set task registers
;
	ldi FL_crit,0xFF	;set critical section
	ldi r17,CF_cmd		;point to cmd reg
	ldi r16,CF_dwrite
	call put_reg		;issue write cmd
;
;wait for drq or not busy
wr_wait:
	ldi r17,CF_stat		;point to status reg
	call get_reg		;get status
	sbrc r16,CF_drq
	rjmp wr_drq		;drq set?
	sbrc r16,CF_busy
	rjmp wr_wait		;loop if busy set
;
;error check
wr_eck:
	sbi PORTC,CF_csel	;deselect CF card
	tst FL_crit		;did init occur?
	breq wr_abt		;then abort
	clr FL_crit		;
	andi r16,CF_errb	;mask to error bit
	ret			;good exit
;
wr_abt:
	rjmp init_sw		;jump to init	
;
;move data to CF card (4 bytes per loop)
wr_drq:
	ldi r19,128		;loop counter
	ldi r17,CF_data		;point to data reg
	ldi r28,low(buffer)	;point to CF buffer
	ldi r29,high(buffer)
;
cf_wrlp:
	ld r16,Y+		;get byte
	call put_reg		;put byte
	ld r16,Y+		;repeat
	call put_reg
	ld r16,Y+
	call put_reg
	ld r16,Y+
	call put_reg
	dec r19
	brne cf_wrlp		;all bytes done?
;
;wait for not busy
	call cf_nbsy		;wait for not busy
	rjmp wr_eck		;check for error
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;subroutine: set task file
;wait for not busy
;load CF card registers
;
set_task:
	call cf_nbsy		;wait for CF ready
;
	ldi r17,CF_count	;set sector count
	ldi r16,1		;always 1
	call put_reg
;
	ldi r17,CF_lba0		;set lba0
	lds r16,CUR_BL
	call put_reg
;
	ldi r17,CF_lba1		;set lba1
	lds r16,CUR_BH
	call put_reg
;
	ldi r17,CF_lba2		;set lba2
	lds r16,UNIT
	call put_reg

	ldi r17,CF_lba3		;set lba3
	ldi r16,CF_drv0
	call put_reg
;
	ret
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;subroutine: cf_nbsy
; Select CF card and poll for not busy
; R16=CF card status (return)
;
cf_nbsy:
	ldi	r17,CF_stat	;point to status reg
not_by1:
	call	get_reg		;get status
	sbrc	r16,CF_busy	;mask to busy bit
	rjmp	not_by1		;loop if busy
	ret
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;subroutine: initialize CF card
; Select CF card 0
; Set 8 bit mode
;
cf_init:
	call cf_nbsy		;wait for CF ready
;
;select CF drive 0
	ldi r17,CF_lba3		;select drive/lba
	ldi r16,CF_drv0		;lba+drive 0
	call put_reg		;shift out data
;
;set 8 bit mode
	ldi r17,CF_feat		;select feature
	ldi r16,CF_8bit		;8 bit mode
	call put_reg		;shift out data
	ldi r17,CF_cmd		;select cmd register
	ldi r16,CF_sfeat	;set features
	call put_reg		;shift out data
;
;wait for not busy
	call cf_nbsy		;read status
	sbi PORTC,CF_csel	;deselect CF card
	ret
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;subroutine: wait_in
; Wait for byte from RXV11
; Set direction=from RXV11
; wait for rxgo to set
; clear RXreq RX_done and RX_err 
; then get byte from rxv11
;
wait_in:
	cbi PORTC,RX_data		;dont drive rx data
	cbi PORTB,RX_out	;dir=from RXV11
in_nrdy:
	sbis PIND,RX_go		;skip next if go is set
	rjmp in_nrdy		;loop till set
	ldi R17,0		;clear err,txrq,done
	out PORTB,R17		;setup for transfer
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;subroutine: get_byte
; get byte from rxv11 without waiting
; byte returned in R16
;
get_byte:
	cbi PORTC,RX_data	;dont drive data
	cbi PORTB,RX_out	;set direction=from RXV11
	ldi R17,8		;set bit count
;
;shift a byte in
shiftr:
;
;shift result left
;sample RX_data line
;put in R16 bit 0
	lsl R16			;shift zero into bit 0
	sbis PIND,RX_data	;test input bit
	rjmp shifi0
	ori R16,1		;set bit zero to 1

shifi0:
;
;toggle clock for next bit
	sbi PORTB,RX_shift	;set clock
	nop
	nop
	cbi PORTB,RX_shift	;clear clock
;
	dec R17		;dec bit counter
	brne shiftr		;loop
	ret
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;subroutine: put_byte
; send byte to rxv11 without waiting
; R16 byte to send (destroyed)
;
put_byte:
	sbi PORTB,RX_out	;direction=to RXV11
	ldi R17,8		;set bit count
;
;shift a byte out
shiftw:
	sbrc R16,7		;test bit 7
	rjmp shift1
	cbi PORTC,RX_data	;output 0 data bit
	rjmp shiftx
;
shift1:
	sbi PORTC,RX_data	;output 1 data bit
	nop			;equalize timing
;
;toggle shift clock
shiftx:
	sbi PORTB,RX_shift	;set clock
	nop
	cbi PORTB,RX_shift	;clear clock
;
;shift to next bit
;loop if more bits to do
	lsl R16			;next bit
	dec R17
	brne shiftw		;loop
	ret
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;get block number
; get 2 bytes from RXV11
;
get_blkn:
	sbi PORTB,RX_txrq	;set request
	call wait_in		;wait for byte
	sts cur_bh,r16		;save as block num hi
;
	sbi PORTB,RX_txrq	;set request
	call wait_in		;wait for byte
	sts cur_bl,r16		;save as bock num low
	ret
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;interrupt service routine
;int0 is triggered on the active edge of
;the init signal from the RXV11
;
irq0_srv:
	push R16	;save state
	in R16,SREG
	push R16
;
;reset RXV11 control port
;turn off txrq,done,error
	in R16,PORTB	;get current port
	andi R16,0x08	;mask off all but RX_out
	out PORTB,R16	;clear RX interface status
;
	tst FL_crit	;in critical code section?
	brne in_crit	;then return
;
	rjmp init_sw	;else abort and init
;
;we are in a critical code section so return 
;and let it finish. dont re-enable irq0 
in_crit:
	clr FL_crit	;reset flag
	pop R16		;restore SREG
	out SREG,R16
	pop R16		;restore R16
	reti
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;program start
;initialize hardware
;
rx_exec:
;
;init stack
	cli			;disable IRQ
	ldi r16,low(RAMEND)	;set stack
	out SPL,r16
	ldi r16,high(RAMEND)
	out SPH,r16
;
;setup ports
;port A input
	clr R16
	out DDRA,R16		;set A to input (data bus)
;
;port B output (all pins low)
	ldi R16,0x1f		;0-4 outputs
	out DDRB,R16
	ldi R16,0x00
	out PORTB,R16		;stop driving all rx signals
;
;port C output (cf cmd reg,reset)
	ldi R16,0xFF		;all outputs
	out DDRC,R16
	ldi R16,CF_cmd
	out PORTC,R16		;select cmd reg
	cbi PORTC,CF_rst		;drive reset low
;
;port D input (init,go,12bit,data)
	clr R16
	out DDRD,R16		;set D to input
;
;setup irq0
	ldi r16,0x03
	out MCUCR,R16		;ext0 pos edge
	ldi R16,0x40
	out GIFR,R16		;reset pending irq
;
;temp debug allow irq
	sei
;reset CF card
	ldi r17,2
	call delay
	sbi PORTC,CF_rst	;remove reset
	ldi r17,25		;delay for CF startup
	call delay
;
;init CF card
	call cf_init
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;initialize software
; the PDP init line comes here (eventually)
init_sw:
;
;reset stack
	cli			;disable IRQ
	ldi r16,low(RAMEND)	;set stack
	out SPL,r16
	ldi r16,high(RAMEND)
	out SPH,r16
;
;clear errors,unit,block num
	ldi R16,0
	sts UNIT,R16	;clear unit
	sts CUR_BL,R16	;clear block
	sts CUR_BH,R16	;
	sts SR_reg,R16	;clear errors
;
	ldi R16,0x84	;init done+ready
	sts ES_reg,R16	;set init done
;
;temp debug allow irq
	sei
;read sector zero
	call rd_blk
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;wait for init to clear
;
init_wait:
	sbic PIND,RX_init	;sample init signal
	rjmp init_wait		;loop till init clear
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;enable irq0
	ldi R16,0x40
	out GIFR,R16		;reset pending
	out GICR,R16		;allow irq0
	sei			;allow irq
	rjmp okdone
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;main program loop
; functions return to either erdone or okdone
;
erdone:
	sbi PORTB,RX_err	;set error bit
;
okdone:
	lds R16,ES_reg		;get status
	call put_byte		;send status
;
xxdone:
	cbi PORTB,RX_out	;direction from RXV11
	sbi PORTB,RX_done	;set done bit
	call wait_in		;get command
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;decode command
; command=R16
decode:
;
;set unit from command byte
	clr R17			;preset unit 0
	sbrc R16,CM_unit	;unit in cmd clear?
	ldi R17,1		;set unit 1 bit
	sts UNIT,R17		;else its unit 0
;
;add command to table base
;push on stack
;execute (via ret)
;
	andi R16,CM_mask	;mask to commmand bits
	ldi R17,low(cmd_tbl)
	add R16,R17		;add cmd to table start
	push R16		;push exec low
	ldi r16,high(cmd_tbl)
	brcc dec01
	inc R16			;propigate carry
dec01:
	push R16		;push exec high
;
	ret			;execute command
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;command jump table
;
cmd_tbl:
	jmp	cm_fill
	jmp	cm_mty
	jmp	cm_write
	jmp	cm_read
	jmp	cm_noop
	jmp	cm_stat
	jmp	cm_wrdel
	jmp	cm_estat
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;command processors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;command: fill buffer
; get 512 bytes from host
; store in buffer
;
cm_fill:
	cbi PORTB,RX_out	;dir=from RXV11
;
;reset errors
	ldi R16,0x80		;set drive ready
	sts ES_reg,R16		;in ES_reg
;
;setup count and pointer
	ldi r19,0		;loop counter=256
	ldi r28,low(buffer)	;point to CF buffer
	ldi r29,high(buffer)
;
;get data from host
;2 bytes per loop
fill01:
	sbi PORTB,RX_txrq	;set tx request
	call wait_in		;wait for input
	st Y+,R16		;save byte
;
	sbi PORTB,RX_txrq	;set tx request
	call wait_in		;wait for input
	st Y+,R16		;save byte
;
	dec r19
	brne fill01		;loop till buffer done
;
	rjmp okdone
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;command: empty buffer
; send 512 bytes to host
;
cm_mty:
	sbi PORTB,RX_out	;dir=to RXV11
;
;clear errors
	ldi R16,0x80		;set drive ready
	sts ES_reg,R16		;in ES_reg
;
;set count and pointer
	ldi r19,0		;loop counter
	ldi r28,low(buffer)	;point to CF buffer
	ldi r29,high(buffer)
;
;send data to host
;2 bytes per loop
mty01:
	ld R16,Y+		;get 1st byte
	call put_byte		;send it
	sbi PORTB,RX_txrq	;set tx request
mty02:
	sbis PIND,RX_go	;wait for req to negate
	rjmp mty02		;loop till go
	cbi PORTB,RX_txrq	;clear request
;
	ld R16,Y+		;get 2nd byte
	call put_byte		;send it
	sbi PORTB,RX_txrq	;set tx request
mty03:
	sbis PIND,RX_go	;wait for req to negate
	rjmp mty03		;loop till go
	cbi PORTB,RX_txrq	;clear request
;
	dec r19			;dec loop counter
	brne mty01		;loop till buffer done
;
	rjmp okdone
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;command: write/write deleted
cm_write:
cm_wrdel:
	ldi R16,0		;clear errors
	sts SR_reg,R16		;clear SR_reg
	ori R16,0x80		;set drive ready
	sts ES_reg,R16		;in ES_reg
	call get_blkn		;get block number
	call wr_blk		;write to CF card
;
cm_wrok:
	rjmp okdone
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;command:	read
cm_read:
	ldi R16,0		;clear errors
	sts SR_reg,R16		;clear SR_reg
	ldi R16,0x80		;set drive ready
	sts ES_reg,R16		;in ES_reg
	call get_blkn		;get block number
	call rd_blk		;read from CF card
;
cm_rdok:
	rjmp okdone
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;command: noop
cm_noop:
	lds R16,ES_reg
	andi R16,~ES_idone
	sts ES_reg,R16
	rjmp okdone
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;command: get status (ES_reg)
cm_stat:
;
;nothing to do
	rjmp okdone
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;command: get error reg (SR_reg)
cm_estat:
	lds R16,SR_reg
	sbi PORTB,RX_out	;direction to RXV11
	call put_byte		;put SR in rx data reg
	rjmp xxdone
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	.end
