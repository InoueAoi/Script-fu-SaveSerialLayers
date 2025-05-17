**This page is written in Japanese. Please translate in your language.**

# Script-fu-SaveSerialLayers
## これって何？
画像編集ソフトGIMPにて、レイヤーをひとつずつ番号を振って保存するスクリプトのりーどみーです。  
GIMPを使っている方は、とりあえず使ってみてください。
## 使い方
1. GIMPをインストールする
2. 任意のフォルダにこのリポジトリの`save-serial-layers.scm`を保存する
3. GIMPの環境設定の項目の、`フォルダー`の下の`スクリプト`を選択し、先ほどのファイルを保存したフォルダをスクリプト読み取り元に設定する
4. GIMPを再起動する
5. メニューの`フィルター`の`Development`の下に`Save Serial Layers`があると思うので、探して押す
## パラメータの説明
上から説明していきます
| 項目 | 説明 |
----|----
| File Directory | ファイルの保存先のフォルダ<br>**※必ず指定してください** |
| File Name | 保存するファイルの名前<br>名前の後に連番がつきます |
| Digits | 連番の桁数<br>1の場合`1,2,3 ~ 9,10` 2の場合`01,02,03 ~ 09,10` |
| First Number | 連番の一番小さい数<br>この数字から数え始めます |
| Numbering from Bottom to Top | オフの場合、上から下の順に保存していきます<br>オンの場合、下から上の順で保存していきます |
| File Type | 保存するファイルの種類<br>`xcf` `Jpeg` `PNG` `BMP`から選べます |
| Jpeg Quality | Jpegで保存する場合の、画像の品質<br>最小1、最大10 |
| PNG Transparent | PNGで保存する場合、透過を反映させるか<br>オンの場合透過させます |
| Run on Windows | **PC環境がWindowsの場合、オンにしてください** |
## 使用条件・禁止事項
| 項目 | 許諾 |
----|----
スクリプトの私的改変 | 許可
第三者による販売・再配布 | 禁止
## このスクリプトに関する記事
準備中
