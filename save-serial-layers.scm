; ©Carotery 2025
; 開発日記
; 2025/5/12
;	なんとかまず習作をひとつ作りました。
;	選択範囲のコピー、頑張るぞ。
; 2025/5/14
;	とりあえずファイルを連番で保存できるようにはしました。
; 2025/5/16
;	結構完成してきたかも
;	あとはポップアップウィンドウから情報を取得するようにするのと、
;	エラーメッセージを出すだけですね。
;	なんか、SF-STRING のパラメータが数値になってしまって困ります。
;	と思っていたらなんとかなったっぽいです。ラッキー。
;	それとは別で、最初の引数からlayers を抜くと、string-length でエラーが出る。
;	こういうときは放っておくに限りますね。いやはや。
;	なんとか完成したと思います。
(define (save-serial-layers image layers file-place file-name digits first-number rev file-type-num quality transparent run-on-windows)
	; image 画像情報の取得

	; 下記はポップアップウィンドウからの取得情報
	; filePlace 	ファイルの保存先の変数（フォルダ指定）
	; fileName		ファイルの保存名（テキスト）
	; digits		連番の桁数（数値）
	; firstNumber	連番の最初の数字（数値）
	; rev			通常上から下に出力するんですけど（多分）逆に出力するときのためのもの（スイッチ）
	; fileTypeNum 	ファイルの拡張子の変数（リスト） リストの中の型は数値
	; quality 		jpeg の品質の変数（スライダー）
	; transparent 	PNGの透過を出力するか（スイッチ）
	; runOnWindows	Windows環境で実行する場合、ディレクトリの仕切りが変わるのでそれ（スイッチ）

	; ファイルを保存する関数
	(define (saveFiles type image name quality transparent)
		(cond
			((= type 0) ; xcf ファイルの保存
				(gimp-file-save 1 image name "")
			)
			((= type 1) ; jpeg ファイルの保存
				(file-jpeg-export 1 image name ""
					1 TRUE TRUE FALSE quality "sub-sampling-1x1"
					TRUE 0 "fixed"  FALSE FALSE FALSE FALSE TRUE FALSE)
			)
			((= type 2) ; png ファイルの保存
				(file-png-export 1 image name ""
					FALSE 9 TRUE FALSE TRUE TRUE transparent FALSE "auto" FALSE FALSE FALSE FALSE TRUE)
			)
			((= type 3) ; bmp ファイルの保存
				(file-bmp-export 1 image name "" FALSE TRUE "rgb-565")
			)
		)
	)

	(let
		(
			; 画像周りの変数を定義
			(image-width 0)
			(image-height 0)
			(image-type 0)
			; 保存するための画像の変数
			(new-image 0)

			; レイヤー周りの変数
			(all-layers 0)
			(current-layer 0)
			(copied-layer 0)

			; ファイル名と拡張子の変数
			(file-type "")
			;(fileName "")
			(saved-file-name "")
			; ファイルの連番の開始番号
			;(firstNumber 0)
			; 連番の桁数と、それに応じたゼロを挿入するための変数
			(file-number "")
			;(digits 0)
			; PNG の透過を出力するかの変数
			;(transparent FALSE)
			
			; while文の制御のための変数
			(count 0)
		)
		; 画像の幅・高さ・カラーモードを代入
		(set! image-width (car (gimp-image-get-width image)))
		(set! image-height (car (gimp-image-get-height image)))
		(set! image-type (car (gimp-image-get-base-type image)))

		; レイヤーの情報を代入
		(set! all-layers (car (gimp-image-get-layers image)))

		;ディレクトリが選択されていない時、エラーを返す
		(if (string=? file-place "") (error "Please Select File Directory"))

		; ファイルの保存先を代入
		(if (= run-on-windows TRUE)
			(set! file-place (string-append file-place "¥"))
			(set! file-place (string-append file-place "/"))
		)
		
		; ファイルの名前を代入
		;(set! fileName "test")
		;(set! fileName (number->string fileName))
		; ファイルの拡張子を取得して、文字列に変更
		(set! file-type #("xcf" "jpeg" "png" "bmp"))

		; JPEGの場合、品質を代入
		(set! quality (* quality 0.1))

		;連番の桁数を代入
		;(set! digits 1)

		(while (> (vector-length all-layers) count)
			; 連番の生成
			(if (= rev TRUE)
				; リバースが真の場合、数字を逆転させる
				(set! file-number (number->string (+ first-number (- (vector-length all-layers) count 1))))
				; リバースが偽の場合、数字を順に増やす
				(set! file-number (number->string (+ first-number count)))
			)
			; ゼロの追加
			(while (> digits (string-length file-number))
				(set! file-number (string-append "0" file-number))
			)
			; ファイル名の作成
			(set! saved-file-name
				(string-append file-place file-name file-number "." (vector-ref file-type file-type-num)))
			; 画像の作成
			(set! new-image (car (gimp-image-new image-width image-height image-type)))
			; レイヤー情報の取得
			(set! current-layer (vector-ref all-layers count))
			; レイヤーのコピー
			(set! copied-layer (car (gimp-layer-new-from-drawable current-layer new-image)))
			; レイヤーの挿入
			(gimp-image-insert-layer new-image copied-layer 0 0)

			; ファイルの保存
			(saveFiles file-type-num new-image saved-file-name quality transparent)

			; 終わりの手続き
			(gimp-image-delete new-image)
			(set! file-number "")
			(set! count (+ 1 count))
		)
		
		(gimp-message "Succeed.")
	)
)

(script-fu-register-filter
	"save-serial-layers"
	"Save Serial Layers"
	"レイヤーごとに連番でエクスポートします"
	"Carotery"
	"Carotery"
	"May 16, 2025"
	"RGB* INDEXED* GRAY*"
	SF-ONE-DRAWABLE
	SF-DIRNAME		"File Directory"	""
	SF-STRING		"File Name"			"Images"
	SF-ADJUSTMENT	"Digits"			'(1 1 8 1 1 0 1)
	SF-ADJUSTMENT	"First Number"		'(1 0 99999999 1 1 0 1)
	SF-TOGGLE		"Numbering Botton to Top"  FALSE
	SF-OPTION		"File Type"			'("xcf" "jpeg" "png" "bmp")
	SF-ADJUSTMENT	"Jpeg Quality"		'(9 0 10 1 1 0 0)
	SF-TOGGLE 		"PNG Transparent"	FALSE
	SF-TOGGLE		"Run on Windows"	FALSE
)

(script-fu-menu-register
	"save-serial-layers"
	"<Image>/Filters"
)