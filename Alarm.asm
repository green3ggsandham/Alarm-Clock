; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 1000    ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P4.4
NEXT1         equ P0.1
MASTER		  equ P0.2
SWITCH		  equ P0.6
SNOOZE		  equ P0.3
LAST		equ P0.0
BUTT equ P0.4
; Reset vector
org 0x0000
    ljmp main
; External interrupt 0 vector (not used in this code)
org 0x0003
	reti
; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR
; External interrupt 1 vector (not used in this code)
org 0x0013
	reti
; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti
; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR
; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
minute:		  ds 1
second:		  ds 1
hour:		  ds 1
day:	 	  ds 1
choose:		  ds 1
choosea:	  ds 1
minutea:	  ds 1
seconda:	  ds 1
houra:		  ds 1
daya:	 	  ds 1
swch:		  ds 1
pma:		  ds 1
hitthat:	  ds 1
am_flag: 	  ds 1
pm_flag: 	  ds 1
snz:		  ds 1
weekend:	  ds 1
olay:		ds 1
twen:		ds 1
test:		ds 1
yolo: 		ds 1
; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
no_alarm: dbit 0
cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST
Initial_Message:  db 'xxx xx:xx:xx xx ', 0
Alarm_Message: 	  db 'xx  xx:xx:00 xx ', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
;	 Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 
  
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	clr TF0  ; According to the data sheet this is done for us already.
	jnb no_alarm, don
	cpl SOUND_OUT ; Connect speaker to P3.7!
	don:
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	push acc
	push psw
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:			
	
	

	mov a, daya				;alarm 2 (saturday sunday)
	cjne a, #2, al1
	mov a, snz
	cjne a, #0, donee
	mov a, hour
	cjne a, houra, donee
	mov a, minute
	cjne a, minutea, donee
	mov a, day
	cjne a, #1, al2
	mov a, pm_flag
	cjne a, pma, checkam
	good:
	setb no_alarm
	sjmp check
	al2:
	cjne a, #7, donee
	mov a, pm_flag
	cjne a, pma, checkam
	setb no_alarm
	sjmp check	
	checkam:
	mov a, pma
	cpl a
	cjne a, am_flag, donee
	sjmp good
	
	al1:					;alarm1 for weekdays
	mov a, daya
	cjne a, #1, donee
	mov a, snz
	cjne a, #0, donee
	mov a, hour
	cjne a, houra, donee
	mov a, minute
	cjne a, minutea, donee	
	mov a, day
	cjne a, #2, nextday
	mov a, pm_flag
	cjne a, pma, checkam
	setb no_alarm
	sjmp check				
	nextday:
	cjne a, #3, nextday1
	mov a, pm_flag
	cjne a, pma, checkam
	setb no_alarm
	sjmp check
	nextday1:
	cjne a, #4, nextday2
	mov a, pm_flag
	cjne a, pma, checkam
	setb no_alarm
	sjmp check
	nextday2:
	cjne a, #5, nextday3
	mov a, pm_flag
	cjne a, pma, checkam
	setb no_alarm
	sjmp check
	nextday3:
	cjne a, #6, donee
	mov a, pm_flag
	cjne a, pma, checkam
	setb no_alarm
	sjmp check
	donee:						;no alarm has been set
	mov a, snz
	cjne a, #0, gogo
	kk:
	clr no_alarm
	clr SOUND_OUT
	sjmp check
	gogo:
	mov a, test
	cjne a, second, kk
	mov snz, #0
	check:
	mov a, Count1ms+0
	cjne a, #low(500), Timer2_ISR_done
	mov a, Count1ms+1
	cjne a, #high(500), Timer2_ISR_done
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	mov a, twen
	cjne a, #1, now
	ljmp annoy
	
	now:	
	mov a, second
	cjne a, #0x59, addsec1			;checks for need to increment minute
	mov a, #0
	mov second, a	
	mov a, minute		
	cjne a, #0x59, addmin1			;checks for need to increment hour
	mov a, #0
	mov minute, a
	mov a, hour
	cjne a, #0x12, next
	ljmp yo_son
next:
	cjne a, #0x11, notpm			;checks for need to change to am or pm
	mov a, pm_flag
	cpl a
	mov pm_flag, a
	mov a, am_flag
	cpl a
	mov am_flag, a
	cjne a, #0 , dontaddday1		;checks for need to increment day
	mov a, day
	cjne a, #0x7, addday1
	mov a, #1
	mov day, #1
addsec1:
	mov a, second
	add a, #1
	da a
	mov second, a
	mov a, #0
	sjmp Timer2_ISR_done
addmin1:
	mov a, minute
	add a, #1
	da a
	mov minute, a
	mov a, #0
	sjmp Timer2_ISR_done
notpm:	
	mov a, hour
	add a, #0x01
	da a
	mov hour, a
	mov a, #0
	sjmp Timer2_ISR_done
	Timer2_ISR_done:
	pop psw
	pop acc
	reti
addday1:
	mov a, day
	add a, #1
	da a
	mov day, a
	mov a, #0x12
	da a
	mov hour, a
	mov a, #0
	sjmp Timer2_ISR_done	
dontaddday1:
	mov a, day
	mov a, #0x12
	da a
	mov hour, a
	sjmp Timer2_ISR_done
yo_son:	
	mov a, #0x01
	da a
	mov hour, a
	mov a, #0
	sjmp Timer2_ISR_done
	
annoy:			
	mov a, second
	cjne a, #0x59, addsec11			;checks for need to increment minute
	mov a, #0
	mov second, a	
	mov a, minute		
	cjne a, #0x59, addmin11			;checks for need to increment hour
	mov a, #0
	mov minute, a
	mov a, hour
	cjne a, #0x23, wow
	ljmp res
	wow:
	sjmp next11
	
	addsec11:
	mov a, second
	add a, #1
	da a
	mov second, a
	mov a, #0
	sjmp Timer2_ISR_done
	
	addmin11:
	mov a, minute
	add a, #1
	da a
	mov minute, a
	mov a, #0
	sjmp Timer2_ISR_done
	
	next11: 
	mov a, hour
	cjne a, #0x23, noswitch
	mov a, day
	cjne a, #0x7, nopee
	mov day, #1
	sjmp Timer2_ISR_done
	nopee:
	add a, #1
	mov day, a
	mov a, #0x23
	da a 
	mov hour, a
	sjmp Timer2_ISR_done
	noswitch:
	mov a, hour
	add a, #1
	da a
	mov hour, a
	sjmp Timer2_ISR_done
	
	res:
	mov hour, #0
	ljmp Timer2_ISR_done
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    mov P0M0, #0
    mov P0M1, #0
    setb EA 
    lcall LCD_4BIT
    clr SOUND_OUT
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    Set_Cursor(2,1)
    Send_Constant_String(#Alarm_Message)
    setb half_seconds_flag
	mov BCD_counter, #0x00
	mov second, #0
	mov minute, #0
	mov hour, #1
	mov no_alarm, #0
	mov day, #1
	mov choosea, #0
	mov seconda, #0
	mov minutea, #0
	mov weekend, #0
	mov houra, #0
	mov daya, #1
	mov snz, #0
	mov a, #0x12
	da a
	mov yolo, a
	mov choose, #0
	mov swch, #0
	mov am_flag, #0
	mov pm_flag, #1
	mov pma, #1
	mov BCD_counter, #'A'
	Set_Cursor(1, 14) 
    Display_char(BCD_counter)
    mov BCD_counter, #'A'
	Set_Cursor(2, 14)
	mov olay, #0
	mov twen, #0
	mov a, #0x12
	
;BUTTONS	
	
loop:
	jb BOOT_BUTTON, switchy
	Wait_Milli_Seconds(#250)	
	jb BOOT_BUTTON, switchy
	jnb BOOT_BUTTON, $		
	clr TR2                
	clr a
	mov hour, #1
	mov day, #1
	mov daya, #1
	mov minute, #0
	mov second, #0
	mov minutea, #0
	mov houra, #0
	mov pm_flag, #1
	mov am_flag, #0
	mov pma, #1
	mov twen, #0
	setb TR2    
	ljmp loop_b 	   
	          
switchy:
	jb SWITCH, masterbutton
	Wait_Milli_Seconds(#100)
	jb SWITCH, masterbutton
	jnb SWITCH, $
	mov a, swch
	cpl a
	mov swch, a
masterbutton:
	mov a, swch
	cjne a, #0, setalarm
	jb MASTER, addday
	Wait_Milli_Seconds(#50)
	jb MASTER, addday
	jnb MASTER, $
	mov a, choose
	cjne a, #3, regmaster
	mov a, #0
	mov choose, a
	ljmp loop_b
	regmaster:
	add a, #1
	mov choose, a
	ljmp loop_b	
setalarm:
	jb MASTER, adddaya
	Wait_Milli_Seconds(#50)
	jb MASTER, adddaya
	jnb MASTER, $
	mov a, choose
	cjne a, #3, regmastera
	mov a, #0
	mov choose, a
	ljmp loop_b
	regmastera:
	add a, #1
	mov choose, a
	ljmp loop_b	
addday:
	mov a, choose
	cjne a, #0, addhour
	jb NEXT1, addhour
	Wait_Milli_Seconds(#50)
	jb NEXT1, addhour
	jnb NEXT1, $
	mov a, day
	cjne a, #0x7, regday
	mov a, #1
	mov day, a
    Q1: ljmp loop_b	
    regday:
	add a, #1
	da a
	mov day, a
	mov a, #0
	ljmp Q1	
adddaya:
	mov a, choose
	cjne a, #0, addhoura
	jb NEXT1, addhoura
	Wait_Milli_Seconds(#50)
	jb NEXT1, addhoura
	jnb NEXT1, $
	mov a, daya
	cjne a, #0x2, regdaya
	mov a, #1
	mov daya, a
    W1: ljmp loop_b	
    regdaya:
	add a, #1
	da a
	mov daya, a
	mov a, #0
	ljmp W1	
addhour:
	mov a, choose
	cjne a, #1, addmin
	jb NEXT1, addmin
	Wait_Milli_Seconds(#50)
	jb NEXT1, addmin
	jnb NEXT1, $
	mov a, twen
	cjne a, #1, mad
	mov a, yolo
	mov a, #0x24
	da a 
	mov yolo, a
	sjmp hooo
	mad:
	mov a, #0x12
	mov yolo, a
	hooo:
	mov a, hour
	cjne a, yolo , reghour
	da a
	mov a, #1
	mov hour, a
    Q2: ljmp loop_b
    reghour:
	add a, #1
	da a
	mov hour, a
	mov a, #0
	ljmp Q2	
addhoura:
	mov a, choose
	cjne a, #1, addmina
	jb NEXT1, addmina
	Wait_Milli_Seconds(#50)
	jb NEXT1, addmina
	jnb NEXT1, $
	mov a, twen
	cjne a, #1, madd
	mov a,yolo
	mov a, #0x23
	da a
	mov a, yolo
	sjmp ho
	madd:
	mov a, #0x12
	da a
	mov a, yolo
	ho:
	mov a, houra
	cjne a, yolo, reghoura
	da a
	mov a, #0
	mov houra, a
    W2: ljmp loop_b
    reghoura:
	add a, #1
	da a
	mov houra, a
	mov a, #0
	ljmp W2		
addmin:
	mov a, choose
	cjne a, #2, changeampm
	jb NEXT1, changeampm
	Wait_Milli_Seconds(#50)
	jb NEXT1, changeampm
	jnb NEXT1, $
	mov a, minute
	cjne a, #0x59, regmin
	mov a, #0
	mov minute, a
    Q: ljmp loop_b	
    regmin:
	add a, #1
	da a
	mov minute, a
	mov a, #0
	ljmp Q	
addmina:
	mov a, choose
	cjne a, #2, changeampma
	jb NEXT1,  changeampma
	Wait_Milli_Seconds(#50)
	jb NEXT1,  changeampma
	jnb NEXT1, $
	mov a, minutea
	cjne a, #0x59, regmina
	mov a, #0
	mov minutea, a
    W: ljmp loop_b	
    regmina:
	add a, #1
	da a
	mov minutea, a
	mov a, #0
	ljmp W		
changeampm:	
	mov a, choose
	cjne a, #3, snoozer
	jb NEXT1, snoozer
	Wait_Milli_Seconds(#50)
	jb NEXT1, snoozer
	jnb NEXT1, $
	mov a, pm_flag
	cpl a
	mov pm_flag, a
	mov a, am_flag
	cpl a
	mov am_flag, a
	ljmp loop_b	
changeampma:	
	mov a, choose
	cjne a, #3, snoozer
	jb NEXT1, snoozer
	Wait_Milli_Seconds(#50)
	jb NEXT1, snoozer
	jnb NEXT1, $
	mov a, pma
	cjne a, #0, nope
	mov a, #1
	mov pma, a
	ljmp loop_b	
	nope:
	mov a, #0
	mov pma, a
	ljmp loop_b	
snoozer:	
	jb SNOOZE, dam
	Wait_Milli_Seconds(#50)
	jb SNOOZE, dam
	jnb SNOOZE, $
	mov a, snz
	cpl a
	mov snz, a
	mov a, second
	add a, #0x10
	da a
	mov test, a
	sjmp loop_b
dam:
	jb BUTT, setit
	Wait_Milli_Seconds(#50)
	jb BUTT, setit
	jnb BUTT, $
	mov a, olay
	cpl a
	mov olay, a
	sjmp loop_b
	
		
setit:
	jb LAST, loop_b
	Wait_Milli_Seconds(#50)
	jb LAST, loop_b
	jnb LAST, $
	mov a, twen
	mov a, #1
	mov twen , a
	sjmp loop_b
;DISPLAY

loop_b:					
    clr half_seconds_flag 
    mov BCD_counter, hour
	Set_Cursor(1, 5)     
	Display_BCD(BCD_counter)
	mov BCD_counter, minute
	Set_Cursor(1, 8)     
	Display_BCD(BCD_counter)
	mov BCD_counter, second
	Set_Cursor(1, 11)    
	Display_BCD(BCD_counter)
	 mov BCD_counter, houra
	Set_Cursor(2, 5)  
	Display_BCD(BCD_counter) 
	mov BCD_counter, minutea
	Set_Cursor(2, 8)
	Display_BCD(BCD_counter)
    mov a, day 
  day1:  
    cjne a, #1, day2
    mov BCD_counter, #'S'
	Set_Cursor(1, 1) 
    Display_char(BCD_counter)
    mov BCD_counter, #'U'
	Set_Cursor(1, 2) 
    Display_char(BCD_counter)
    mov BCD_counter, #'N'
	Set_Cursor(1, 3) 
    Display_char(BCD_counter)
    ljmp yo
  day2:
  	cjne a, #2, day3
  	mov BCD_counter, #'M'
	Set_Cursor(1, 1) 
    Display_char(BCD_counter)
    mov BCD_counter, #'O'
	Set_Cursor(1, 2) 
    Display_char(BCD_counter)
    mov BCD_counter, #'N'
	Set_Cursor(1, 3) 
    Display_char(BCD_counter)  
    ljmp yo
  day3:
  	cjne a, #3, day4
    mov BCD_counter, #'T'
	Set_Cursor(1, 1) 
    Display_char(BCD_counter)
    mov BCD_counter, #'U'
	Set_Cursor(1, 2) 
    Display_char(BCD_counter)
    mov BCD_counter, #'E'
	Set_Cursor(1, 3) 
    Display_char(BCD_counter)
    ljmp yo
  day4:
  	cjne a, #4, day5
    mov BCD_counter, #'W'
	Set_Cursor(1, 1) 
    Display_char(BCD_counter)
    mov BCD_counter, #'E'
	Set_Cursor(1, 2) 
    Display_char(BCD_counter)
    mov BCD_counter, #'D'
	Set_Cursor(1, 3) 
    Display_char(BCD_counter)
    ljmp yo
  day5:
  	cjne a, #5, day6
  	mov BCD_counter, #'T'
	Set_Cursor(1, 1) 
    Display_char(BCD_counter)
    mov BCD_counter, #'H'
	Set_Cursor(1, 2) 
    Display_char(BCD_counter)
    mov BCD_counter, #'U'
	Set_Cursor(1, 3) 
    Display_char(BCD_counter)
    ljmp yo
  day6:	 
  	cjne a, #6, day7
  	mov BCD_counter, #'F'
	Set_Cursor(1, 1) 
    Display_char(BCD_counter)
    mov BCD_counter, #'R'
	Set_Cursor(1, 2) 
    Display_char(BCD_counter)
    mov BCD_counter, #'I'
	Set_Cursor(1, 3) 
    Display_char(BCD_counter)  
    ljmp yo
  day7:	 
  	cjne a, #7, yo
  	mov BCD_counter, #'S'
	Set_Cursor(1, 1) 
    Display_char(BCD_counter)
    mov BCD_counter, #'A'
	Set_Cursor(1, 2) 
    Display_char(BCD_counter)
    mov BCD_counter, #'T'
	Set_Cursor(1, 3) 
    Display_char(BCD_counter)
  yo: 
  mov a, twen
  cjne a, #0, woww
    mov a, am_flag  
    cjne a, #0, pm
    mov BCD_counter, #'A'
    Set_Cursor(1, 14) 
    Display_char(BCD_counter)
    mov BCD_counter, #'M'
	Set_Cursor(1, 15) 
    Display_char(BCD_counter)
	sjmp go 
	pm:
	mov BCD_counter, #'P'
	Set_Cursor(1, 14) 
    Display_char(BCD_counter)
    mov BCD_counter, #'M'
	Set_Cursor(1, 15) 
    Display_char(BCD_counter)
    sjmp go
    woww:
   mov BCD_counter, #' '
    Set_Cursor(1, 14) 
    Display_char(BCD_counter)
    mov BCD_counter, #' '
    Set_Cursor(1, 15) 
    Display_char(BCD_counter)
    go:   
	mov a, daya 
  day11:  
    cjne a, #1, day22
    mov BCD_counter, #'A'
	Set_Cursor(2, 1) 
    Display_char(BCD_counter)
    mov BCD_counter, #'1'
	Set_Cursor(2, 2) 
    Display_char(BCD_counter)
    ljmp yoo
  day22:
  	cjne a, #2, yoo
  	mov BCD_counter, #'A'
	Set_Cursor(2, 1) 
    Display_char(BCD_counter)
    mov BCD_counter, #'2'
	Set_Cursor(2, 2) 
    Display_char(BCD_counter)        
  yoo:   
  mov a, twen
  cjne a, #0, wowww
	mov a, pma
	cjne a, #0, pmm
    mov BCD_counter, #'P'
    Set_Cursor(2, 14) 
    Display_char(BCD_counter)
    mov BCD_counter, #'M'
    Set_Cursor(2, 15) 
    Display_char(BCD_counter)
    ljmp loop 
  pmm:
	mov BCD_counter, #'A'
	Set_Cursor(2, 14) 
    Display_char(BCD_counter)
    mov BCD_counter, #'M'
    Set_Cursor(2, 15) 
    Display_char(BCD_counter)
    ljmp loop
    
   wowww:
   	mov BCD_counter, #' '
	Set_Cursor(2, 14) 
    Display_char(BCD_counter)
    mov BCD_counter, #' '
    Set_Cursor(2, 15) 
    Display_char(BCD_counter)
    ljmp loop
END