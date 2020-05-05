data segment 
	startMsg DB 0DH,0AH,'Start TETRIS game? 1/0',0DH,0AH,'$' 
	losemsg	DB 'GAME OVER!' 
    scoremsg1 db 201,19 dup(205),187 
    scoremsg2 db 186,'  score:           ',186 
    scoremsg3 db 200,19 dup(205),188 

	playTable	dw	24	dup(?),0ffffh 
	currentY	db	? 
	nextY	db	? 
	rightOffset	db	? 
	leftOffset	db	? 
	currentFigure	db	? 
	currentRotation db	?  
	currentTime	db 0 
	currentSpeed	db	15 
	isTouched	db 0 
	isFixed	db 0 
	flg	db 0 
	figureTypes	dw	0h,3c0h,0h,0h
		dw	100h,100h,100h,100h 
		dw	0h,3c0h,0h,0h 
		dw	100h,100h,100h,100h
		 
		dw	0h,180h,180h,0h	 
		dw	0h,180h,180h,0h 
		dw	0h,180h,180h,0h 
		dw	0h,180h,180h,0h 
		
		dw	0h,380h,200h,0h	
		dw	200h,200h,300h,0h 
		dw	0h,80h,380h,0h 
		dw	0h,300h,100h,100h 
		
		dw	0h,380h,80h,0h	
		dw	300h,200h,200h,0h 
		dw	0h,200h,380h,0h 
		dw	100h,100h,300h,0h  
		
		dw	0h,180h,300h,0h	
		dw	100h,180h,80h,0h 
		dw	0h,180h,300h,0h 
		dw	100h,180h,80h,0h 
		
		dw	0h,300h,180h,0h	
		dw	80h,180h,100h,0h 
		dw	0h,300h,180h,0h 
		dw	80h,180h,100h,0h 
		
		dw	0h,380h,100h,0h  
		dw	100h,180h,100h,0h 
		dw	100h,380h,0h,0h	 
		dw	100h,300h,100h,0h 
	currentPosition	dw	4	dup(?) 
	nextPosition	dw	4	dup(?) 
    color db 00001001b,00001010b,00001011b,00001100b,00001101b,00001110b,00000001b  
	currentColor db ? 
	score db 5 dup('0'),'$'
	prevscore db 1 dup ('0')
	newFigureFlag db 1 dup ('2') 
	emptyString	db	25	dup(219) 
data ends 
 
stack segment	stack 
	db	200	dup(?) 
stack	ends 
 
code segment 
	assume 	cs:code,ds:data,es:data,ss:stack 
start:  mov	ax,data 
	mov	ds,ax 
	push ds
	 
	;mov	al,1ch 
;	mov	ah,35h
;	int	21h  
	

	;mov	dx,offset int1c 
;	mov	ax,seg	int1c 
;	mov	ds,ax 
;	mov	al,1ch 
;	mov	ah,25h
;	int	21h 
;	pop	ds 
	 
gameover:	 
	mov	ah,00h 
	mov	al,03h 
	int 10h    
askForStart: 
    mov	ah,09h 
	mov	dx,offset startMsg
	int	21h
	mov	ah,08h 
	int 21h 
	sub	al,'0'
	cmp al, 0
	je exit
	cmp al, 1
	jne askForStart	
			 
 
	mov	ah,00h 
	mov	al,12h 
    int	10h   
	   
	mov	ah,0bh
	mov	bh,01
	mov	bl,00h 
	int	10h 
	

	call startGame 
	call begin 
	call delay 
	mov	currentTime,0h 
inputLoop: 	
    sti 
    mov ah, 00h
    mov cx, 0000h
    mov dx, 0000h
    int 1ah
    mov al, dl
	;mov	al,currentTime 
	cmp	al,currentSpeed 
	jg time
	 
    mov si,03h 
    xor ax, ax
    mov al, score[si]
    cmp al, prevscore[0]  
    je continue
    cmp currentSpeed, 0
    jle continue
    dec currentSpeed
    inc newFigureFlag[0]
    mov prevscore[0], al
    continue: 

    mov ah,1  
    int 16h 
    jz inputLoop 
    mov ah,0 
    int 16h 
    cmp	al,'a' 
    jz leftPressed 
    cmp	al,'w' 
    jz rotetePressed 
    cmp	al,'d' 
    jz rightPressed 
    cmp al,'s' 
    jnz time 
downPressed:
    call delay 
	call down 
    cmp isTouched,1 
    jne	downPressed 
    call begin 
    jmp inputLoop  
    
leftPressed:	
    call left 
	jmp inputLoop 
rotetePressed:	
    call rotate 
	jmp	inputLoop 
rightPressed:	
    call right 
	jmp	inputLoop 
time:
	mov	currentTime,0h
	mov ah, 01h
	mov cx, 0000h
	mov dx, 0000h
	int 1ah 
	call down 
	cmp	isTouched,0 
	je inputLoop 
	call begin 
	jmp	inputLoop 
 
exit:	
	mov	ax,0003h 
	int 	10h 
	mov	ax,data 
	mov	ds,ax 
	  
	mov ah,04Ch
	mov al,1h
	int 21h 
 
int1c proc 
	sti 
	push ax 
	push dx 
	mov	ax,data 
	mov	ds,ax 
	inc	currentTime 
	pop dx 
	pop	ax 
	iret 
int1c endp 
 
delay proc near 
	push cx 
    mov cx,00ffh 
loop20:
    loop loop20  
	pop	cx 
	ret 
delay endp 
	 
rotate proc near 
	mov	si,offset figureTypes 
	mov	al,currentFigure 
	mov	ah,0h 
	mov	cl,32 
	mul	cl 
	add	si,ax 
	mov	al,currentRotation 
    inc al 
	and	al,03h 
	mov	ah,0h 
	mov	cl,8 
	mul	cl 
	add	si,ax 
	mov	di,offset nextPosition 
	mov	cx,04h 
	cld 
rotation: 
    push cx 
    lodsw 
    mov cl,rightOffset 
    shr ax,cl 
    mov cl,leftOffset 
    shl ax,cl 
	stosw 
    pop cx 
    loop rotation 
 
	call check 
	cmp	al,0h 
	jne	exitRotation 
	mov	bx,0000h 
	call outputField 
	 
	call copy2to1 
	inc	currentRotation 
	and	currentRotation,3h	 
	mov	bh,00h 
	mov	bl,currentColor 
	call outputField 
exitRotation:
	ret 
rotate	endp 
 
right proc near 
	call copy1to2 
	mov	si,offset nextPosition 
	mov	cx,04h 
moveRight:
	mov	ax,[si] 
	shr	ax,1 
	mov	[si],ax 
	inc	si 
	inc	si 
	loop moveRight 
	call check 
	cmp	al,0h 
	jne	moveRightExit 
	mov	bx,0000h 
	call outputField 
	call copy2to1 
	 
	cmp	leftOffset,0 
	je	moveRightSuccessful 
	dec	leftOffset 
	dec	rightOffset 
moveRightSuccessful:
    inc	rightOffset 
	mov	bh,00h 
	mov	bl,currentColor 
	call 	outputField 
moveRightExit:	ret 
right	endp 
 
left proc near 
	call copy1to2 
	mov	si,offset nextPosition 
	mov	cx,04h 
moveLeft:	mov	ax,[si] 
	shl	ax,1 
	mov	[si],ax 
	inc	si 
	inc	si 
	loop moveLeft 
	call check 
	cmp	al,0h 
	jne	moveLeftExit 
	mov	bx,0000h 
	call outputField 
	call copy2to1 
	 
	cmp	rightOffset,0 
	je moveLeftSuccessful 
	dec	rightOffset 
	dec	leftOffset 
moveLeftSuccessful:
    inc	leftOffset 
	mov	bh,00h 
	mov	bl,currentColor 
	call 	outputField 
moveLeftExit:	
    ret 
left	endp 
 
down proc near 
	call copy1to2 
	inc	nextY 
	call check 
	cmp	al,0h 
	jne	downSuccessful 
	 
	mov	bx,0000h 
	call outputField 
	call copy2to1 
	mov	bh,00h 
	mov	bl,currentColor 
	call outputField 
	mov	isTouched,00h 
	ret 
downSuccessful:
    call fixFigure 
	mov	isTouched,01h
	ret 
down endp 
 
fixFigure proc near 
    push ax
	mov	bh,0h 
    mov bl,0h 
	call outputField 
	mov	bh,0h 
    mov bl,01011001b 
	call outputField 
    
	inc	score[4]
	mov	isFixed,01h 
	mov	ah,0h 
	mov	al,currentY 
	add	al,currentY 
	mov	si,offset playTable 
	add	si,ax 
	mov	di,00h  
	mov	cx,04h 
	cld 
checkTouch: 
    lodsw 
    or ax,currentPosition[di]
    mov [si-2],ax 
	add di,2 
    loop checkTouch 
 
    mov si,offset playTable 
    add si,23*2 
    mov di,si 
    mov cx,20 
    mov bh,00h 
         
    mov	flg,00h 
    std 
    
checkBorder: 
    lodsw 
    cmp ax,0ffffh 
    jne clearFigure  
    mov	flg,0ffh 
    mov	al,isFixed 
    sal	al,1  
    mov	isFixed,al 
    jmp checkBorder 
clearFigure: 
    stosw 
	
	cmp	flg,0h 
	je erasingMove 
	push cx 
    mov dh,cl 
    add dh,03h 
 
    mov dl,0ah 
    mov bx,0000h 
    mov bp,offset emptyString 
    mov cx,20 
    push ax 
    mov ax,1300h 
    int 10h 
    pop ax 
 
	mov cl,03h 
	shl	ax,cl 
	mov	cx,0ah 
	mov	dl,08h 
borders: 
    add dl,2 
    mov bl,0h 
	shl	ax,1 
    jnc moveCorrected 
    mov bl,01011001b 
moveCorrected: 
    call outputBlock 
    loop borders 
	pop	cx 
erasingMove:
    loop checkBorder 
         
    mov	al,isFixed 
    sar	al,1 
    add	score[3],al 
    mov	cx,05h 
    mov	si,04h
     
scoreFormatting:	
    cmp	score[si],'9' 
	jng	figureFixed 
	inc	score[si-1] 
	sub	score[si],0ah 
figureFixed:	
    dec	si
	loop scoreFormatting
	pop ax 
    ret 
fixFigure endp 
 
outputScore proc near  
	mov	ax,data 
	mov	es,ax 
	mov	bp,offset score 
	mov	cx,05h 
    mov dx,0635h 
	mov	bh,0h 
	mov	al,0h 
	mov	bl,00110100b 
	mov	ah,13h 
	int	10h 
	ret 
outputScore endp 
 
copy2to1 proc near 
	cld 
	mov	si,offset nextPosition 
	mov	di,offset currentPosition 
	mov	cx,08 
	rep	movsb 
	mov cl,nextY 
	mov	currentY,cl 
	ret 
copy2to1 endp 
 
copy1to2 proc near 
	cld 
	mov	si,offset currentPosition 
	mov	di,offset nextPosition 
	mov	cx,08 
	rep	movsb 
	mov cl,currentY 
	mov	nextY,cl 
	ret 
copy1to2 endp 
 
begin proc near 
	call getRandomValue
	mov currentFigure, al 
	call outputScore 
	 
	mov	currentRotation,0 
	mov	currentY,4 
	mov	nextY,4 
	mov	rightOffset,0 
	mov	leftOffset,0 
	mov	ah,0 
	mov	al,currentFigure 
	mov	si,ax 
	mov	cl,color[si] 
	mov	currentColor,cl 
	mov	di,offset nextPosition 
	mov	si,offset figureTypes  
	mov	bl,32 
	mul	bl 
	add	si,ax  
 
	mov	cx,08 
	cld 
	rep movsb 
	
	
	call copy2to1 
	mov	bh,0h  
	mov	bl,currentColor  
	call outputField  
	call check 
	cmp	al,0 
	je	figureAppeared
    mov	bp,offset losemsg 
    mov dx,0e30h
    mov cx,10 
    mov	al,0h
    mov	bh,0h 
    mov bl,01011010b 
	mov	ah,13h 
	int	10h

	mov	dl,07h  
	int 21h 
	mov	ah,08h 
	int 21h 
	jmp	gameover 
figureAppeared:	
    call delay 
	mov	currentTime,0h 
	ret 
begin endp 
 
check proc near	 
	mov	ah,0h 
	mov	al,nextY 
	add	al,nextY  
	mov	si,offset playTable 
	add	si,ax 
	mov	di,00h 
	mov	cx,04h 
	cld 
checkBelowTouch:	
    lodsw 
	and ax,nextPosition[di] 
	jnz	endOfCheck 
	add di, 2 
	loop checkBelowTouch 
	mov	al,00h 
	ret 
endOfCheck:  
    mov	al,0fh 
	ret 
check endp 
 
outputField	proc near   
	mov si,offset currentPosition 
	mov cx,04h 
	mov	dl,08h 
	mov dh,currentY 
	add	dh,04h 
	push dx 
	cld 
locateFigure:	
    lodsw 	 
	pop	dx 
	push dx 
	sub	dh,cl  
	push cx 
	mov cl,03h 
	shl	ax,cl  
	
	
	mov	cx,0ah  
checkFigureBlock:	
    add dl, 2 
	shl	ax,1 
	jnc	skip3 
	call outputBlock 
		 
skip3:	
    loop checkFigureBlock 
	pop	cx  
	loop locateFigure   
	pop	dx 
	ret 
outputField	endp 
  
  
  
outputBlock proc near	 
	push ax 
	push bx 
	push cx 
	push dx 
	push di 
	push si 
	mov	bp,offset emptyString  
	mov	cx,02h 
	mov	ax,1300h  
	int 10h
	 
	 
	cmp	bl,0h  
	je	blockIsPut 
	
	 
	mov	ah,0h 
	mov	al,dh 
    mov cl,16 
	mul	cl 
	mov	si,ax  
	
 
	mov	ah,0h 
	mov	al,dl 
	mov	cl,8 
	mul	cl 
	mov	di,ax 
	
 
	mov	ax,0c00h ;AH=0с, AL=00 
	mov	dx,si ;ряд
    add dx,15 
	mov	cx,16 
horizontalBorder:	
    add	cx,di 
	dec	cx 
	int	10h 
	inc	cx 
	sub	cx,di 
	loop horizontalBorder 
	 
    mov dx,si 
    mov cx,15 
    add di,15     
verticalBorder:  
    push cx 
    mov cx,di 
    int 10h 
    inc dx 
    pop cx 
    loop verticalBorder 
 
blockIsPut:	pop	si 
	pop	di 
	pop	dx 
	pop	cx 
	pop	bx 
	pop	ax 
	ret 
outputBlock endp 
	 
cls	proc near 
	mov	cx,0 
	mov	dh,24 
	mov	dl,79 
	mov	bh,0 
	mov	ax,600h  
	int	10h
	ret 
cls	endp 
 
getRandomValue proc near 
    sub newFigureFlag[0], '0'
loop5:  
    in ax,40h 
    inc al
	and	al,07h 
	cmp	al,07h 
	je loop5 
	cmp al, newFigureFlag[0]
	jg loop5
	add newFigureFlag[0], '0'
	ret 
getRandomValue endp 
 
startGame proc near 
	call cls 
	 
	mov	ax,data 
	mov	es,ax 
    mov cx,3 
	mov	bp,offset scoremsg1 ; 
    mov dx,052ah 
    
outputMessage:	
    push cx 
    mov cx,21 
	mov	al,0h 
	
	
	mov	bh,0h 
    mov bl,01011010b 
	mov	ah,13h 
	int	10h  
	
    add bp,21  
	inc	dh 
	pop	cx 
	loop outputMessage 
	  
	mov	bp,offset emptyString 
    mov cx,0024 
    mov dx,0308h 
	mov	bh,0h 
	mov	al,0h 
	mov	bl,00110100b 
	mov	ah,13h 
	int	10h    
    mov dx,1808h 
	int	10h 
 
	mov	cx,20 
    mov dx,0308h 
outputMessage2:	
    mov	si,cx  
	mov	cx,02 
	inc	dh 
	int	10h  
	mov	cx,si 
	loop outputMessage2 
 
	mov	cx,20 
    mov dx,031eh 
outputTable:	
    mov	si,cx 
	mov	cx,02 
	inc	dh 
	int	10h 
	mov	cx,si 
	loop outputTable
	 
	cld 
	mov	di,offset playTable 
	mov	cx,24 
	mov ax,0e007h 
	rep	stosw 
	
	
	mov	di,offset	score 
	mov	al,'0' 
	mov	cx,05h 
	rep	stosb  
	ret 
startGame	endp 
 
code	ends 
	end	start