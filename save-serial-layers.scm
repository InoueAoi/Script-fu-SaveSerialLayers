; ©Carotery 2025
; 開発日記
; 2025/5/20
;	3日くらいかけていろいろ実装しました。
;	・レイヤーグループの中のレイヤーも保存する機能
;	・上記実装の上で、レイヤーを下から上へ連番保存する機能
;	・不可視のレイヤーの保存をスキップする機能
(define (save-serial-layers
	image				; 画像情報の取得
	layers				; レイヤー情報をあれこれするときに必要な引数
	; 下記はポップアップウィンドウからの取得情報
	file-place			; ファイルの保存先の変数（フォルダ指定）
	file-name			; ファイルの保存名（テキスト）
	digits				; 連番の桁数（数値）
	first-number		; 連番の最初の数字（数値）
	rev					; 通常上から下に出力するんですけど（多分）逆に出力するときのためのもの（スイッチ）
	file-ext			; ファイルの拡張子の変数（リスト） リストの中の型は数値
	quality 			; jpeg の品質の変数（スライダー）
	transparent 		; PNGの透過を出力するか（スイッチ）
	into-folder			; フォルダ内のレイヤーも連番保存するかの変数（スイッチ）
	without-invisible	; 不可視レイヤーを連番保存しないようにする変数（スイッチ）
	run-on-windows		; Windows環境で実行する場合、ディレクトリの仕切りが変わるのでそれ（スイッチ）
	)

	; ファイルを保存する関数
	(define (save-files image file)
		(cond
			((= file-ext 0) ; xcf ファイルの保存
				(gimp-file-save 1 image (string-append file ".xcf") "")
			)
			((= file-ext 1) ; jpeg ファイルの保存
				(file-jpeg-export 1 image (string-append file ".jpeg") ""
					quality 1 TRUE TRUE FALSE "sub-sampling-1x1"
					TRUE 0 "fixed"  FALSE FALSE FALSE FALSE TRUE FALSE)
			)
			((= file-ext 2) ; png ファイルの保存
				(file-png-export 1 image (string-append file ".png") ""
					FALSE 9 TRUE FALSE TRUE TRUE transparent FALSE "auto" FALSE FALSE FALSE FALSE TRUE)
			)
			((= file-ext 3) ; bmp ファイルの保存
				(file-bmp-export 1 image (string-append file ".bmp") "" FALSE TRUE "rgb-565")
			)
		)
	)
	; レイヤーを一つずつ取り出す関数
	(define (save-layers width height type layers number)
		(let 
			(
				; レイヤーをコピーするための画像の変数
				(new-image 0)
				; レイヤー周りの変数
				(current-layer 0)
				(copied-layer 0)
				; ファイルの連番と、パスの変数
				(file-number "")
				(file "")
				; ループを制御する用の変数
                (count 0)
			)
			(while (> (vector-length layers) count)
				
                (if (= TRUE rev)
					; rev が真の場合、下からレイヤー情報を引き出す
					(set! current-layer (vector-ref layers (- (vector-length layers) count 1)))
					(set! current-layer (vector-ref layers count))
				)
                (cond
					; 取得したレイヤーが不可視状態かつ、without-invisible が真の場合、保存を飛ばす
					((and (= TRUE without-invisible) (= 0 (car (gimp-item-get-visible current-layer))))
						()
					)
					; 取得したレイヤーがレイヤーグループだった場合、レイヤーグループに潜る
					((and (not (= 0 (car (gimp-item-is-group current-layer)))) (eq? TRUE into-folder))
						(set! current-layer (car (gimp-item-get-children current-layer)))
						; フォルダ内のレイヤーがない場合、飛ばす
						(if (null? current-layer)
							()
							(begin
								; フォルダの子レイヤーを保存する
								(save-layers width height type current-layer number)
								; 連番をレイヤーの数だけ増やす
								(set! number (+ number (vector-length current-layer)))
							)
						)
					)
					; 取得したレイヤーが普通のレイヤーの場合、ファイルに書き出す
                    (
						; 保存用画像の生成
						(set! new-image (car (gimp-image-new width height type)))
						; 現在のレイヤーを、保存用画像にコピー
                        (set! copied-layer (car (gimp-layer-new-from-drawable current-layer new-image)))
						; コピーしたレイヤーを画像に挿入
						(gimp-image-insert-layer new-image copied-layer 0 0)
						; 連番の数の変数の型を数値から文字列に変える
						(set! file-number (number->string number))
						; ゼロの追加
						(while (> digits (string-length file-number))
							(set! file-number (string-append "0" file-number))
						)
						; ファイルのパスの設定
						(set! file (string-append file-place file-name file-number))
						; ファイルの保存
						(save-files new-image file)
						; 保存用画像の破棄
						(gimp-image-delete new-image)
						; 連番を一つ進める
						(set! number (+ number 1))
                    )
                )
				; ループ変数を一つ進める
                (set! count (+ 1 count))
            )
		)
	)
	(let 
		(
			; 現在の画像の幅、高さ、カラーモードを格納する変数
			(image-width 0)
			(image-height 0)
			(image-type 0)
			; 現在の画像のレイヤー全てを格納する変数
			(all-layers 0)
		)
		; 画像の幅、高さ、カラーモードを格納
		(set! image-width (car (gimp-image-get-width image)))
		(set! image-height (car (gimp-image-get-height image)))
		(set! image-type (car (gimp-image-get-base-type image)))
		; 全てのレイヤーを格納
		(set! all-layers (car (gimp-image-get-layers image)))
		; Jpeg　品質の変数を加工
		(set! quality (* 0.1 quality))
		; run-on-windows が真の場合（windows環境で実行している場合）、パスの区切りの記号を変える
		(if (eq? TRUE run-on-windows)
			(set! file-place (string-append file-place "¥"))
			(set! file-place (string-append file-place "/"))
		)
		; レイヤー連番保存実行
		(save-layers image-width image-height image-type all-layers first-number)
	)
)

(script-fu-register-filter
	"save-serial-layers"
	"Save Serial Layers"
	"レイヤーごとに連番でエクスポートします"
	"Carotery"
	"Carotery"
	"May 20, 2025"
	"RGB* INDEXED* GRAY*"
	SF-ONE-DRAWABLE
	SF-DIRNAME		"File Directory"	""
	SF-STRING		"File Name"			"Images"
	SF-ADJUSTMENT	"Digits"			'(1 1 8 1 1 0 1)
	SF-ADJUSTMENT	"First Number"		'(1 0 99999999 1 1 0 1)
	SF-TOGGLE		"Numbering from Bottom to Top"  FALSE
	SF-OPTION		"File Type"			'("xcf" "jpeg" "png" "bmp")
	SF-ADJUSTMENT	"Jpeg Quality"		'(1 0 10 1 1 0 0)
	SF-TOGGLE 		"PNG Transparent"	FALSE
	SF-TOGGLE		"Get into Folders"	FALSE
	SF-TOGGLE		"Save without Invisible Layers"	FALSE
	SF-TOGGLE		"Run on Windows"	FALSE
)

(script-fu-menu-register
	"save-serial-layers"
	"<Image>/Filters"
)