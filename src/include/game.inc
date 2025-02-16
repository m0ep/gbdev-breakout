; macro mWaitForVBlank
;
; Loops until entering VBlank period
macro WaitForVBlank
: ld a, [rLY]
  cp a, 144
  jp c, :-
endm
