INCLUDE "hardware.inc"
INCLUDE "game.inc"
	rev_Check_hardware_inc 4.0

DEF BRICK_LEFT EQU $05
DEF BRICK_RIGHT EQU $06
DEF BLANK_TILE EQU $08

SECTION "Header", ROM0[$100]
	jp EntryPoint

	ds $150 - @, 0 ; room for header

SECTION "Entry point", ROM0

EntryPoint:

	WaitForVBlank

	; Turn the LCD off
	xor a
	ld [rLCDC], a

	; Copy the tile date
	ld de, Tiles
	ld hl, $9000
	ld bc, TilesEnd - Tiles
	call MemCopy

	; Copy the tile map
	ld de, Tilemap
	ld hl, $9800
	ld bc, TilemapEnd - Tilemap
	call MemCopy

	; On boot the OAMRAM is filled with random data
	xor a
	ld b, 160
	ld hl, _OAMRAM
ClearOam:
	ld [hl+], a
	dec b
	jp nz, ClearOam

	; Copy the paddle
	ld de, Paddle
	ld hl, $8000
	ld bc, PaddleEnd - Paddle
	call MemCopy

	; Copy the ball
	ld de, Ball
	ld hl, $8020
	ld bc, BallEnd - Ball
	call MemCopy

	; Init paddle sprite
	ld hl, _OAMRAM
	ld a, 128 + 16
	ld [hl+], a
	ld a, 16 + 8
	ld [hl+], a
	ld a, 0
	ld [hl+], a
	ld [hl+], a

	; Init paddle sprite
	ld a, 128 + 16
	ld [hl+], a
	ld a, 24 + 8
	ld [hl+], a
	ld a, 1
	ld [hl+], a
	ld a, 0
	ld [hl+], a

	; Init ball sprite
	ld a, 100 + 16
	ld [hl+], a
	ld a, 32 + 8
	ld [hl+], a
	ld a, 2
	ld [hl+], a
	ld a, 0
	ld [hl+], a

	ld a, 1
	ld [wBallMomentumX], a
	ld a, -1
	ld [wBallMomentumY], a

	xor a
	ld [wFrameCounter], a
	ld [wCurKeys], a
	ld [wNewKeys], a

	; Turn the LCD on
	ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON
	ld [rLCDC], a

	; Init display
	ld a, %11100100
	ld [rBGP], a
	ld a, %11100100
	ld [rOBP0], a

	;Main loop
Main:
	ld a, [rLY]
	cp 144
	jp nc, Main

	WaitForVBlank

	; Move ball
	ld a, [wBallMomentumX]
	ld b, a
	ld a, [_OAMRAM + 9]
	add a, b
	ld [_OAMRAM + 9], a

	ld a, [wBallMomentumY]
	ld b, a
	ld a, [_OAMRAM + 8]
	add a, b
	ld [_OAMRAM + 8], a

BounceOnTop:
	ld a, [_OAMRAM + 8]
	sub a, 16 + 1
	ld c, a
	ld a, [_OAMRAM + 9]
	sub a, 8
	ld b, a
	call GetTileByPixel
	ld a, [hl]
	call IsWallTile
	jp nz, BounceOnRight
	call CheckAndHandleBrick
	ld a, 1
	ld [wBallMomentumY], a

BounceOnRight:
	ld a, [_OAMRAM + 8]
	sub a, 16
	ld c, a
	ld a, [_OAMRAM + 9]
	sub a, 8 - 1
	ld b, a
	call GetTileByPixel
	ld a, [hl]
	call IsWallTile
	jp nz, BounceOnLeft
	call CheckAndHandleBrick
	ld a, -1
	ld [wBallMomentumX], a

BounceOnLeft:
	ld a, [_OAMRAM + 8]
	sub a, 16
	ld c, a
	ld a, [_OAMRAM + 9]
	sub a, 8 + 1
	ld b, a
	call GetTileByPixel
	ld a, [hl]
	call IsWallTile
	jp nz, BounceOnBottom
	call CheckAndHandleBrick
	ld a, 1
	ld [wBallMomentumX], a

BounceOnBottom:
	ld a, [_OAMRAM + 8]
	sub a, 16 - 1
	ld c, a
	ld a, [_OAMRAM + 9]
	sub a, 8
	ld b, a
	call GetTileByPixel
	ld a, [hl]
	call IsWallTile
	jp nz, BounceDone
	call CheckAndHandleBrick
	ld a, -1
	ld [wBallMomentumY], a
BounceDone:

	ld a, [_OAMRAM]
	ld b, a
	ld a, [_OAMRAM + 8]
	add a, 3; Fix Ball dip into paddle
	cp a, b
	jp nz, PaddleBounceDone
	ld a, [_OAMRAM + 9]; Ball X
	ld b, a
	ld a, [_OAMRAM + 1]; Paddle X
	sub a, 8
	cp a, b
	jp nc, PaddleBounceDone
	add a, 8 + 24
	cp a, b
	jp c, PaddleBounceDone

	ld a, -1
	ld [wBallMomentumY], a
PaddleBounceDone:

	call UpdateKeys

CheckLeft:
	ld a, [wCurKeys]
	and a, PADF_LEFT
	jp z, CheckRight
Left:
	; move paddle on pixel to the left
	ld a, [_OAMRAM + 1]
	dec a
	; did we hit the edge?
	cp a, 15
	jp z, Main
	ld [_OAMRAM + 1], a
	add a, 8
	ld [_OAMRAM + 5], a
	jp Main

CheckRight:
	ld a, [wCurKeys]
	and a, PADF_RIGHT
	jp z, Main
Right:
	ld a, [_OAMRAM + 5]
	inc a
	; did we hit the edge?
	cp a, 105
	jp z, Main
	ld [_OAMRAM + 5], a
	sub a, 8
	ld [_OAMRAM + 1], a
	jp Main

; Copy bytes around
; @param de: Source
; @param hl: Target
; @param bc: Length
MemCopy:
	ld a, [de]
	ld [hl+], a
	inc de
	dec bc
	ld a, b
	or a, c
	jp nz, MemCopy
	ret

UpdateKeys:
	; poll the first nibble
	ld a, P1F_GET_BTN
	call .onenibble
	ld b, a

	ld a, P1F_GET_DPAD
	call .onenibble
	swap a
	xor a, b
	ld b, a

	ld a, P1F_GET_NONE
	ldh [rP1], a

	ld a, [wCurKeys]
	xor a, b
	and a, b
	ld [wNewKeys], a
	ld a, b
	ld [wCurKeys], a
	ret

.onenibble
	ldh [rP1], a
	call .knownret
	ldh a, [rP1]
	ldh a, [rP1]
	ldh a, [rP1]
	or a, $F0
.knownret
	ret

; Convert a pixel position to a tilemap address
; hl = $9800 + X + Y * 32
; @param b: X
; @param c: Y
; @return hl: tile address
GetTileByPixel:
	ld a, c
	and a, %11111000
	ld l, a
	ld h, 0
	; hl = position * 8

	add hl, hl ; position * 16
	add hl, hl ; position * 32
	; x position to offset
	ld a, b
	srl a; a / 2
	srl a; a / 4
	srl a; a / 8
	; Add two offsets together
	add a, l
	ld l, a
	adc a, h
	sub a, l
	ld h, a
	; Add offset to tilemap base address
	ld bc, $9800
	add hl, bc
	ret

; @param a: tile ID
; @return z: set if is a wall
IsWallTile:
	cp a, $00
	ret z
	cp a, $01
	ret z
	cp a, $02
	ret z
	cp a, $04
	ret z
	cp a, $05
	ret z
	cp a, $06
	ret z
	cp a, $07
	ret

CheckAndHandleBrick:
	ld a, [hl]
	cp a, BRICK_LEFT
	jr nz, CheckAndHandleBrickRight
	ld [hl], BLANK_TILE
	inc hl
	ld [hl], BLANK_TILE
CheckAndHandleBrickRight:
	cp a, BRICK_RIGHT
	ret nz
	ld [hl], BLANK_TILE
	dec hl
	ld [hl], BLANK_TILE
	ret


SECTION "Tile data", ROM0

Tiles:
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33322222
	dw `33322222
	dw `33322222
	dw `33322211
	dw `33322211
	dw `33333333
	dw `33333333
	dw `33333333
	dw `22222222
	dw `22222222
	dw `22222222
	dw `11111111
	dw `11111111
	dw `33333333
	dw `33333333
	dw `33333333
	dw `22222333
	dw `22222333
	dw `22222333
	dw `11222333
	dw `11222333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33333333
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `33322211
	dw `22222222
	dw `20000000
	dw `20111111
	dw `20111111
	dw `20111111
	dw `20111111
	dw `22222222
	dw `33333333
	dw `22222223
	dw `00000023
	dw `11111123
	dw `11111123
	dw `11111123
	dw `11111123
	dw `22222223
	dw `33333333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `11222333
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `00000000
	dw `11001100
	dw `11111111
	dw `11111111
	dw `21212121
	dw `22222222
	dw `22322232
	dw `23232323
	dw `33333333
	; Paste your logo here:

TilesEnd:

SECTION "Tilemap", ROM0

Tilemap:
	db $00, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $02, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $05, $06, $05, $06, $05, $06, $05, $06, $05, $06, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $0A, $0B, $0C, $0D, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $0E, $0F, $10, $11, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $12, $13, $14, $15, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $08, $07, $03, $16, $17, $18, $19, $03, 0,0,0,0,0,0,0,0,0,0,0,0
	db $04, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $09, $07, $03, $03, $03, $03, $03, $03, 0,0,0,0,0,0,0,0,0,0,0,0
TilemapEnd:

SECTION "Paddle", ROM0

Paddle:
    dw `03333333
    dw `30313111
    dw `33303000
    dw `33313111
    dw `33323222
    dw `03333333
    dw `00000000
    dw `00000000

		dw `33333330
    dw `11111113
    dw `00000003
    dw `11111113
    dw `22222223
    dw `33333330
    dw `00000000
    dw `00000000
PaddleEnd:

SECTION "Ball", ROM0

Ball:
    dw `00033000
    dw `00322300
    dw `03222230
    dw `03222230
    dw `00322300
    dw `00033000
    dw `00000000
    dw `00000000
BallEnd:


SECTION "Counter", WRAM0
wFrameCounter: db

SECTION "Input Variables", WRAM0
wCurKeys: db
wNewKeys: db

SECTION "Ball data", WRAM0
wBallMomentumX: db
wBallMomentumY: db
