
	; INESヘッダー
	.inesprg 1 ;   - プログラムにいくつのバンクを使うか。今は１つ。
	.ineschr 1 ;   - グラフィックデータにいくつのバンクを使うか。今は１つ。
	.inesmir 0 ;   - 水平ミラーリング
	.inesmap 0 ;   - マッパー。0番にする。

calc0 = $00
calc1 = $01
calc2 = $02

funcArg0 = $10
funcArg1 = $11
funcArg2 = $12

random = $20

StrAdr_L = $30
StrAdr_H = $31

scene = $90 ;0: title 1: game 2: gameover (top i bit: reload?)

;start game variables (able to be reloaded)
keyBuffer = $A1 ;A,B,Select,Start,Up,Down,Left,Right
counter = $A2
cooltime = $A3
boost = $A4
flag0 = $A5 ;0:isPaused 1:isCollided
life = $A6 ;
scoreH = $A7
scoreL = $A8
sprCount_coin = $A9
sprCount_enemy = $B0 ; default one is not included
sprCount_redCoin = $B1
;game variables end


;macro start]

; ex) JsrAboutKey [KeyAddr] [sub-routine]
jsrKeyEvent .macro
		lda <keyBuffer
		and \1
		beq .else_\@
		jsr \2
.else_\@
		.endm

clearAttrib .macro
		; 属性初期化
		lda #$23
		sta $2006
		lda #$C0
		sta $2006
		ldx #$00    	; Xレジスタクリア
		lda \1			; ４つともパレット0番
.\@
		sta $2007		; $2007に属性の値を読み込む
		; 64回(全キャラクター分)ループする
		inx
		cpx #64
		bne .\@
		.endm
;macro end

	.bank 1      ; バンク１
	.org $FFFA   ; $FFFAから開始

	.dw mainLoop    ; VBlank割り込み
	.dw Start    ; リセット割り込み。起動時とリセットでStartに飛ぶ
	.dw IRQ        ; ハードウェア割り込みとソフトウェア割り込みによって発生

	.bank 0			 ; バンク0

	.org $0300	 ; $0300から開始
Sprite1_Y:     .db  0   ; スプライト#1 Y座標 player
Sprite1_T:     .db  0   ; スプライト#1 ナンバー
Sprite1_S:     .db  0   ; スプライト#1 属性
Sprite1_X:     .db  0   ; スプライト#1 X座標

Sprite2_Y:     .db  0   ; スプライト#2 Y座標 enemy
Sprite2_T:     .db  0   ; スプライト#2 ナンバー
Sprite2_S:     .db  0   ; スプライト#2 属性
Sprite2_X:     .db  0   ; スプライト#2 X座標
	.org $8000	 ; $8000から開始
Start:
	sei
	cld
	ldx #$ff
	txs
	; PPUコントロールレジスタ1初期化
	lda #%00001000	; ここではVBlank割り込み禁止
	sta $2000

waitVSync:
	lda $2002  ; VBlankが発生すると、$2002の7ビット目が1になる
	bpl waitVSync  ; bit7が0の間は、Startラベルの位置に飛んでループして待つ

	lda #%00000110		; 初期化中はスプライトとBGを表示OFFにする
	sta $2001

	ldx #$00    ; Xレジスタクリア

	; VRAMアドレスレジスタの$2006に、パレットのロード先のアドレス$3F00を指定する。
	lda #$3F    ; have $2006 tell
	sta $2006   ; $2007 to start
	lda #$00    ; at $3F00 (pallete).
	sta $2006

loadPal:			; ラベルは、「ラベル名＋:」の形式で記述
	lda tilepal, x ; Aに(ourpal + x)番地のパレットをロードする

	sta $2007 ; $2007にパレットの値を読み込む

	inx ; Xレジスタに値を1加算している

	cpx #32 ; Xを32(10進数。BGとスプライトのパレットの総数)と比較して同じかどうか比較している
	bne loadPal ;	上が等しくない場合は、loadpalラベルの位置にジャンプする
	; Xが32ならパレットロード終了

	jsr loadAttrib_title
	jsr loadBG_common
	jsr loadBG_title
	; ####

	; init ppu reg2
	lda #%00011110	; enable sprite and BG
	sta $2001

	lda #0
initZeroPage:
	sta <$00, x
	inx
	bne initZeroPage

	; init ppu reg2
	lda #%00011110	; enable sprite and BG
	sta $2001

	; init sound reg (tri and noise on)
	lda #0
	sta $4015

	; permit interruption of PPU ctrl register1
	lda #%10001000
	sta $2000

	lda #5
	sta <life

	lda #0
	sta <scene

InfiniteLoop:
	jmp InfiniteLoop

mainLoop:

	; prepare pad IO register
	lda #$01
	sta $4016
	lda #$00
	sta $4016
	sta keyBuffer ;reset key buffer

	jsr loadCtrl ;load key data

	jsrKeyEvent #%00010000, KeyPress_START ;unpause

	lda <flag0
	and #%00000001
	beq .not_paused ;Do nothing if it's paused.
	lda <scene
	beq .not_paused ;Do not pause if the scene is Title
	cmp #2
	beq .not_paused
	rti
.not_paused:

.branchScene
	lda <scene
	and #%01111111
	cmp #1
	beq .br_sceneGame
	cmp #2
	beq .br_sceneGameOver
	jsr sceneTitle
	jmp .branchScene_end
.br_sceneGame
	jsr sceneGame
	jmp .branchScene_end
.br_sceneGameOver
	jsr sceneGameOver
	jmp .branchScene_end
.branchScene_end

;reload?
	lda <scene
	and #%10000000
	beq .reload_end

	lda #%00000110		; 初期化中はスプライトとBGを表示OFFにする
	sta $2001

	lda <scene
	and #%01111111
	cmp #1
	beq .reload_sceneGame
	cmp #2
	beq .reload_sceneGameOver

	jsr loadAttrib_title
	jsr loadBG_common
	jsr loadBG_title

	lda #%00001110 ;doesn't render sprite
	sta $2001

	jmp .reload_end
.reload_sceneGame
	jsr loadAttrib_game
	jsr loadBG_common
	jsr loadBG_game
	jsr initGame
	jsr soundGameStart

	lda #%00011110
	sta $2001
	jmp .reload_end
.reload_sceneGameOver
	jsr loadAttrib_gameover
	jsr loadBG_common
	jsr loadBG_gameover

	lda #%00001110 ;doesn't render sprite
	sta $2001
	jmp .reload_end

.reload_end
	lda <scene
	and #%01111111
	sta <scene

NMIEnd:
	rti
; ### main loop end.

findCoinCollided:

	ldx #0

.loop_find
	lda $0309, x
	cmp #4
	bne .continue

	lda $030A, x
	beq .continue

	lda Sprite1_X
	and #%11111000
	sta <calc0

	lda Sprite1_Y
	and #%11111000
	sta <calc1

	lda $0308, x ;Y
	and #%11111000
	cmp <calc1
	bne .continue

	lda $030B, x ;X
	and #%11111000
	cmp <calc0
	bne .continue

	jmp .found
.continue
	txa
	cmp #20
	bpl .not_found
	inx
	inx
	inx
	inx
	jmp .loop_find

.found
	lda #0
	sta $0308, x
	sta $0309, x
	sta $030A, x
	sta $030B, x ;remove coin
	dec <sprCount_coin
	jsr scoreUp
	jsr soundScoreUp
	jsr deleteEnemies
	rts
.not_found
	rts

findRedCoinCollided:

	ldx #0

.loop_find
	lda $0309, x
	cmp #$4
	bne .continue

	lda $030A, x
	bne .continue

	lda Sprite1_X
	and #%11111000
	sta <calc0

	lda Sprite1_Y
	and #%11111000
	sta <calc1

	lda $0308, x ;Y
	and #%11111000
	cmp <calc1
	bne .continue

	lda $030B, x ;X
	and #%11111000
	cmp <calc0
	bne .continue

	jmp .found
.continue
	txa
	cmp #20
	bpl .not_found
	inx
	inx
	inx
	inx
	jmp .loop_find

.found
	lda #0
	sta $0308, x
	sta $0309, x
	sta $030A, x
	sta $030B, x ;remove coin
	dec <sprCount_redCoin

	inc <life

	jsr scoreUp
	jsr scoreUp
	jsr scoreUp
	jsr soundLifeUp
	rts
.not_found
	rts

findEnemyCollided:

	lda <cooltime
	bne .return

	ldx #0

.loop_find
	lda $0305, x
	cmp #2 ;2 or 3
	bmi .continue
	cmp #4
	bpl .continue

	lda Sprite1_X
	and #%11111000
	sta <calc0

	lda Sprite1_Y
	and #%11111000
	sta <calc1

	lda $0304, x ;Y
	and #%11111000
	cmp <calc1
	bne .continue

	lda $0307, x ;X
	and #%11111000
	cmp <calc0
	bne .continue

	jmp .found
.continue
	txa
	cmp #20
	bpl .return
	inx
	inx
	inx
	inx
	jmp .loop_find

.found

	lda $0305, x
	cmp #2
	beq .not_remove

	lda #0
	sta $0304, x
	sta $0305, x
	sta $0306, x
	sta $0307, x ;remove enemy
	dec <sprCount_enemy

.not_remove

	lda CooltimeDefault
	sta <cooltime
	dec <life
	jsr soundHit

	lda <life
	beq .dead
	jmp .collide_end
.dead
	lda #%10000010
	sta <scene

	jmp .collide_end

.notCollided

	; not collided

.collide_end

.return
	rts

moveEnemy:

	lda <counter
	cmp #30
	bmi .return

	lda <scoreH ;more move
	lsr A
	lsr A
	lsr A
	sta <calc0

	lda $0307, x
	cmp Sprite1_X
	beq .no_lr
	bmi .mv_right
	bpl .mv_left
.mv_left
	dec $0307, x

	lda $0307, x
	sec
	sbc <calc0
	sta $0307, x

	jmp .no_lr
.mv_right
	inc $0307, x

	lda $0307, x
	clc
	adc <calc0
	sta $0307, x

	jmp .no_lr
.no_lr

	lda $0304, x
	cmp Sprite1_Y
	beq .return
	bmi .mv_down
	bpl .mv_up
.mv_up
	dec $0304, x

	lda $0304, x
	sec
	sbc <calc0
	sta $0304, x

	jmp .return
.mv_down
	inc $0304, x

	lda $0304, x
	clc
	adc <calc0
	sta $0304, x

.return
	rts

moveEnemySub:

;y: $0304, x
;x: $0307, x

	lda <sprCount_enemy
	cmp Max_Enemy
	bpl .toPlayer

	jsr getRandom
	and #%00000001
	beq .pattern0

	lda $0304, x
	cmp Sprite2_Y
	beq .return
	bmi .mv_up
	bpl .mv_down

.mv_up
	inc $0304, x
	rts
.mv_down
	dec $0304, x
	rts

.pattern0

	lda $0307, x
	cmp Sprite2_X
	beq .return
	bmi .mv_right
	bpl .mv_left

.mv_right
	inc $0307, x
	rts
.mv_left
	dec $0307, x
	rts

.toPlayer

	jsr getRandom
	and #%00000001
	beq .pattern0_p

	lda $0304, x
	cmp Sprite1_Y
	beq .return
	bmi .mv_up_p
	bpl .mv_down_p

.mv_up_p
	inc $0304, x
	inc $0304, x
	rts
.mv_down_p
	dec $0304, x
	dec $0304, x
	rts

.pattern0_p

	lda $0307, x
	cmp Sprite1_X
	beq .return
	bmi .mv_right_p
	bpl .mv_left_p

.mv_right_p
	inc $0307, x
	inc $0307, x
	rts
.mv_left_p
	dec $0307, x
	dec $0307, x
.return
	rts

deleteEnemies:

	lda #0
	sta <sprCount_enemy

	ldx #0
.loop_find ;find empty sprite
	lda $0305, x
	cmp #3
	bne .continue

	lda #0
	sta $0304, x
	sta $0305, x
	sta $0306, x
	sta $0307, x

.continue
	inx
	inx
	inx
	inx
	txa
	cmp #40
	beq .return
	jmp .loop_find
.return
	rts

; ###   SCENES   ###

sceneTitle:

	jsrKeyEvent #%00010000, .gotoGame
	rts
.gotoGame
	lda #%10000001
	sta <scene
	rts

sceneGameOver:

	jsrKeyEvent #%10000000, .goto_title
	jsrKeyEvent #%01000000, .goto_title
	rts

.goto_title
	;jsr loadAttrib_title

	lda #%10000000
	sta <scene
	rts

sceneGame:

; ### key events
	jsrKeyEvent #%00100000, KeyPress_SELECT ; pause
	jsrKeyEvent #%10000000, KeyPress_A
	jsrKeyEvent #%01000000, KeyPress_B
	jsrKeyEvent #%00001000, KeyPress_UP
	jsrKeyEvent #%00000100, KeyPress_DOWN
	jsrKeyEvent #%00000010, KeyPress_LEFT
	jsrKeyEvent #%00000001, KeyPress_RIGHT
; ###

; ### counter
	inc <counter
	lda <counter
	cmp #60
	bne .after_count
	lda #0
	sta <counter
.after_count
; ###

	jsr findCoinCollided
	jsr findRedCoinCollided

;random

	jsr getRandom

	and #%01111111
	bne .random_coin_end

	jsr addCoin

.random_coin_end

	jsr getRandom
	and #%10111111
	bne .enemy_end
	jsr addEnemy

.enemy_end

	jsr getRandom
	and #%11111011
	bne .random_end
	jsr addRedCoin

.random_end
;random end

; ### render sprites

	lda <counter

	;and #%00011000
	;lsr A
	;lsr A
	;lsr A
	;clc
	;adc #4
	;sta Sprite3_T

	lda <counter
	cmp #30
	bmi .spr_inv_left
	jmp .spr_inv_right
.spr_inv_left
	lda #%00000000
	jmp .spr_inv_end
.spr_inv_right
	lda #%01000000
.spr_inv_end
	sta Sprite1_S
	sta Sprite2_S

	lda <cooltime
	beq .show_player

	lda <counter
	cmp #30
	bmi .show_player

	lda Sprite1_S
	ora #%00100000
	sta Sprite1_S
	jmp .show_player_end

.show_player

	lda Sprite1_S
	and #%11011111
	sta Sprite1_S

.show_player_end

; ###

; ### move sprites
	lda <cooltime
	bne .move_end

	ldx #0
.loop_find ;find empty sprite
	lda $0305, x
	cmp #2
	beq .typeEnemy
	cmp #3
	beq .typeEnemySub

	jmp .continue

.typeEnemy
	jsr moveEnemy
	jmp .continue
.typeEnemySub
	jsr moveEnemySub
	jmp .continue
.continue
	inx
	inx
	inx
	inx
	txa
	cmp #40
	beq .move_end
	jmp .loop_find

.move_end


; ###
	jsr findEnemyCollided

	lda <cooltime
	beq .dont_dec_cool
	dec <cooltime
.dont_dec_cool

.bg_update

	lda #$20
	sta $2006
	lda #$20
	sta $2006

	lda #$00
	sta $2007

	lda #$53 ;li-
	sta $2007
	lda #$54 ;-fe
	sta $2007

	lda <life
	and #$F
	clc
	adc #$20 ;number: $20 ~
	sta $2007

	lda #$00 ;
	sta $2007

	lda #$50 ;sc-
	sta $2007
	lda #$51 ;-or
	sta $2007
	lda #$52 ;-e:
	sta $2007

;score start
	jsr showScore
;score end

	lda #$00
	sta $2007

	lda <flag0
	and #%00000001
	beq .not_show_pause

.show_pause

	lda #$39 ;P
	sta $2007
	lda #$2A ;A
	sta $2007
	lda #$3E ;U
	sta $2007
	lda #$3C ;S
	sta $2007
	lda #$2E ;E
	sta $2007

	jmp .bg_update_end

.not_show_pause

	lda #$00
	sta $2007
	sta $2007
	sta $2007
	sta $2007
	sta $2007

.bg_update_end

	lda $2002
	lda #$00
	sta $2005
	sta $2005

	;sprite start
	lda #$3
	sta $4014
	;sprite end

	rts; SCENE_TITLE END

; ### SCENES END ###

addCoin:
	;$0308 y 9 t A s B x

	lda <sprCount_coin ; won't be more 3 coins.
	cmp #2
	bpl .return

	lda Sprite2_Y ; prevents the coin from being out of screen
	cmp #200
	bpl .return

	lda <cooltime ; cooling x
	bne .return

	ldx #0

.loop_find ;find empty sprite
	lda $0309, x
	beq .found
	inx
	inx
	inx
	inx
	jmp .loop_find

.found

	lda Sprite2_Y
	sta $0308, x
	lda #4
	sta $0309, x
	lda #%00000001
	sta $030A, x
	lda Sprite2_X
	sta $030B, x

	inc <sprCount_coin
.return
	rts

addRedCoin:
	;$0308 y 9 t A s B x

	lda <life
	cmp #4
	bpl .return

	lda <sprCount_redCoin ; won't be more 3 coins.
	cmp #1
	bpl .return

	lda Sprite2_Y ; prevents the coin from being out of screen
	cmp #200
	bpl .return

	lda <cooltime ; cooling x
	bne .return

	ldx #0

.loop_find ;find empty sprite
	lda $0309, x
	beq .found
	inx
	inx
	inx
	inx
	jmp .loop_find

.found

	lda Sprite2_Y
	sta $0308, x
	lda #$4
	sta $0309, x
	lda #%00000000
	sta $030A, x
	lda Sprite2_X
	sta $030B, x

	inc <sprCount_redCoin
.return
	rts

addEnemy:

	lda <sprCount_enemy ; won't be more 4 enemies.
	cmp Max_Enemy
	bpl .return

	lda <cooltime ; cooling x
	bne .return

	ldx #0

.loop_find ;find empty sprite
	lda $0309, x
	beq .found
	inx
	inx
	inx
	inx
	jmp .loop_find

.found

	lda Sprite2_Y
	sta $0308, x
	lda #3
	sta $0309, x
	lda #%00000000
	sta $030A, x
	lda Sprite2_X
	sta $030B, x

	inc <sprCount_enemy
.return
	rts

;keys
loadCtrl:
	ldx #8
	jmp .getInputSub
.getInput
	asl <keyBuffer
.getInputSub
	lda $4016
	and #1
	ora <keyBuffer
	sta <keyBuffer
	dex
	bne .getInput
	rts

KeyPress_SELECT:

	jsr soundPause

	lda <flag0
	ora #%00000001
	sta <flag0

	rts

KeyPress_START:

	lda <flag0
	and #%00000001
	bne .do_restart ;nagaoshi sound renzoku bousi
	rts
.do_restart
	lda <flag0
	and #%11111110
	sta <flag0

	jsr soundRestart

	rts

KeyPress_A:
	rts

KeyPress_B:
	rts

KeyPress_UP:
	dec Sprite1_Y
	rts

KeyPress_DOWN:
	inc Sprite1_Y
	rts

KeyPress_LEFT:
	dec Sprite1_X
	rts

KeyPress_RIGHT:
	inc Sprite1_X
	rts

;sound

soundPause:
	lda $4015
	ora #%00000001
	sta $4015

	lda #%11110001
	sta $4000
	lda #%11010001
	sta $4001
	lda #%10000000
	sta $4002
	lda #%00100000
	sta $4003
	rts

soundRestart:
	lda $4015
	ora #%00000001
	sta $4015

	lda #%11110001
	sta $4000
	lda #%11011001
	sta $4001
	lda #%10000000
	sta $4002
	lda #%00100000
	sta $4003
	rts

soundGameStart:
	lda $4015
	ora #%00000001
	sta $4015

	lda #%10011111
	sta $4000
	lda #%11101001
	sta $4001
	lda #%00000000
	sta $4002
	lda #%00100010
	sta $4003
	rts

soundHit:
	lda $4015
	ora #%00001000 ; noise
	sta $4015

	lda #%11000011 ; [unused]2 [length]1 [decay]1 [decay ratio]4
	sta $400C
	lda $%10000100 ; [rnd type]1 [unused]3 [wave]4
	sta $400E
	lda #%00001011 ; [length]5 [unused]3
	sta $400F
	rts

soundScoreUp:
	lda $4015
	ora #%00000001
	sta $4015

	lda #%10011111
	sta $4000
	lda #%11101001
	sta $4001
	lda #%00000000
	sta $4002
	lda #%00100001
	sta $4003
	rts

soundLifeUp:
	lda $4015
	ora #%00000001
	sta $4015

	lda #%10011111
	sta $4000
	lda #%10101011
	sta $4001
	lda #%00000000
	sta $4002
	lda #%00100011
	sta $4003
	rts

shiftRight:
	lsr A
	dec <funcArg0
	bne shiftRight
	rts

shiftLeft:
	asl A
	dec <funcArg0
	bne shiftLeft
	rts

scoreUp:
	lda <scoreL
	clc
	adc #1
	sta <scoreL
	bcc .scoreUp_end
	clc
	inc <scoreH
.scoreUp_end
	rts


loadAttrib_title:
	clearAttrib #%01010101
	rts

loadAttrib_game:
	clearAttrib #0
	rts

loadAttrib_gameover:
	clearAttrib #%01010101
	rts

loadBG_title: ; + 0x20 de itigyou

	lda $2002

	lda #0
	sta <calc0
	sta <calc2

	lda #$20
	sta <calc1

	ldy #0

.loopY

	tya
	asl A
	asl A
	asl A
	asl A
	sta <calc0
	clc
	adc <calc0
	clc
	adc #$48
	sta <calc2
	bcc .not_carry
	clc
	lda #$21
	sta <calc1
.not_carry

	lda <calc1
	sta $2006

	lda <calc2
	sta $2006

	ldx #0
.loopX

	txa
	clc
	adc #$80
	clc
	adc <calc0
	sta $2007

	inx
	txa
	cmp #$10
	bne .loopX

	iny
	tya
	cmp #$08
	bne .loopY


	lda #$22
	sta $2006
	lda #$6A
	sta $2006

	lda #low(S_Title_Tmp1)
	sta <StrAdr_L
	lda #high(S_Title_Tmp1)
	sta <StrAdr_H
	jsr putStr

	lda #0
	sta $2005
	sta $2005

	rts

loadBG_game:

	rts

loadBG_gameover:

	lda $2002

	lda #$20
	sta $2006
	lda #$20
	sta $2006

	lda #low(S_GameOver_Tmp0)
	sta <StrAdr_L
	lda #high(S_GameOver_Tmp0)
	sta <StrAdr_H
	jsr putStr

	lda #$20
	sta $2006
	lda #$60
	sta $2006

	lda #low(S_GameOver_Tmp1)
	sta <StrAdr_L
	lda #high(S_GameOver_Tmp1)
	sta <StrAdr_H
	jsr putStr

	lda #$20
	sta $2006
	lda #$A0
	sta $2006

	lda #low(S_GameOver_Score)
	sta <StrAdr_L
	lda #high(S_GameOver_Score)
	sta <StrAdr_H
	jsr putStr

	jsr showScore

	lda #0
	sta $2005
	sta $2005

	rts

loadBG_common:

	; $2000のネームテーブルに生成する
	lda #$20
	sta $2006
	lda #$00
	sta $2006

	;960 = 240 * 4

	ldx #240
	ldy #4

	lda #0
.loop
	sta $2007
	dex
	bne .loop
	ldx #240
	dey
	bne .loop
	rts

	rts

putStr:
	; 文字列表示
	ldy #0
.putStrSub
	lda [StrAdr_L],y	; 間接アドレッシング
	cmp #'@'			; '@'か？
	beq .putStrEnd		; '@'ならエンドコードなので終了

	cmp #' '
	beq .putSpace
	cmp #':'
	beq .putColon

	cmp #$41
	bmi .notAlphabet
	cmp #$60
	bpl .notAlphabet

	clc
	sbc #$16
	sta $2007			; 1文字出力
	jmp .putCharEnd

.notAlphabet

	cmp #$30
	bmi .notNumber
	cmp #$39
	bpl .notNumber

	clc
	sbc #$0F
	sta $2007			; 1文字出力
	jmp .putCharEnd

.notNumber
	;no char
	jmp .putCharEnd
.putSpace
	clc
	lda #$00
	sta $2007
	jmp .putCharEnd
.putColon
	clc
	lda #$49
	sta $2007
	jmp .putCharEnd
.putCharEnd

	iny
	jmp .putStrSub
.putStrEnd
	rts

; ###

initGame:

	ldx #0
.loop
	lda #0
	sta <$A0, x
	inx
	txa
	cmp #20 ;A0 ~ BF
	bne .loop

	lda #5
	sta <life

	; １番目のスプライト座標初期化
	lda X_Pos_Init
	sta Sprite1_X
	lda Y_Pos_Init
	sta Sprite1_Y

	lda #200
	sta Sprite2_X
	lda #200
	sta Sprite2_Y

	lda #1
	sta Sprite1_T
	lda #2
	sta Sprite2_T

	ldx #0
.init_sprites
	lda #0
	sta $0308, x
	inx
	txa
	cmp #248
	bne .init_sprites

	rts

showScore:
	lda #4
	sta <funcArg0

	lda <scoreH
	jsr shiftRight
	clc
	adc #$20
	sta $2007

	lda <scoreH
	and #$0F
	clc
	adc #$20
	sta $2007

	lda #4
	sta <funcArg0
	lda <scoreL
	jsr shiftRight

	clc
	adc #$20
	sta $2007

	lda <scoreL
	and #$0F
	clc
	adc #$20
	sta $2007
	rts

getRandom:
	lda <random
	adc Sprite1_X
	adc Sprite1_Y
	adc Sprite2_X
	adc Sprite2_Y
	adc keyBuffer
	sta <random
	lda <random
	rts

IRQ:
	rti

; ###############

	; 初期データ
X_Pos_Init   .db 20
Y_Pos_Init   .db 40

Max_Enemy   .db 3

CooltimeDefault .db 180

tilepal: .incbin "main.dat"

S_Title_Tmp1:
	.db "PRESS [START]@"
S_GameOver_Tmp0:
	.db "GAME OVER!@"
S_GameOver_Tmp1:
	.db "PRESS A OR B TO GO TO TITLE@"
S_GameOver_Score:
	.db "FINAL SCORE:@"

	.bank 2       ; バンク２
	.org $0000    ; $0000から開始

	.incbin "main.bkg"
	.incbin "main.spr"
