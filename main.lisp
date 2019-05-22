;;TimerA

;;initialize
(ql:quickload :ltk)
(ql:quickload :trivial-shell)
(ql:quickload :harmony-simple)
(harmony-simple:initialize)

;;パス関係
(defvar *music-directory* #P"~/for-lisp/timer/music-source/")
(defun choose-music (music-name &key (music-type "mp3"))
  (make-pathname :name music-name
                 :type music-type
                 :defaults *music-directory*))



(defun force-exit (sig) (eq sig 'no));;for safty
;;現在時刻のリスト取得
(defun get-time ()
  (multiple-value-list (get-decoded-time)))
;;時間設定関数定義
(defun set-alarmtime
    (hour minute
        &key (now (get-time))
          (sec 10)
          (date (nth 3 now))
          (month (nth 4 now))
          (year (nth 5 now)))
  (let ((n (encode-universal-time sec minute hour date month year)))
    (if (<= n (get-universal-time)) (+ n 86400) n)))

;;音量制御
;;PC自体の音量
(defun get-volume ()
  (parse-integer
   (trivial-shell:shell-command
    "osascript -e \"(get volume settings)'s output volume\"")))
(defun set-volume (volume)
  (if (or (< volume 0) (< 100 volume))
      nil
      (trivial-shell:shell-command
       (format nil
               "osascript -e \"set volume output volume ~A\""
               volume))))
;;全体の音量を下げて、アラーム自体の音量をあげる実装もあり
(defmacro with-alarm-volume (alarm-volume &body body)
  `(let ((default-volume (get-volume)))
     (set-volume ,alarm-volume)
     (progn ,@body)
     (set-volume default-volume)))
(defun play-alarm (volume music-name)
  (with-alarm-volume volume
    (let
        ((music-tmp
           (harmony-simple:play (choose-music music-name) :music)))
      (do () (harmony-simple:ended-p music-tmp)
              nil)
        (sleep 1)))))

;;曜日判定
;;;曜日繰返し判定用リスト(月　火　水...)
(defun wday-loop-by
  (&key (now-wday (nth 6 (get-time)))
     (wday-lst '(t t t t t nil nil)))
  ;;ループしない場合を除外
  (if (every #'null wday-lst)
      (return-from wday-lst 0 ;;or nil
  (setq wday-lst (subseq (append wday-lst wday-lst) (1+ now-wday))
  (do ((i 1 (+ 1 i)) (lst wday-lst (cdr lst)))
      ((car lst) i)))
(defvar *test-signal* 1)
;;指定時刻にアラーム←時刻判定+対応する関数呼び出し
(defun timer-loop (hour min &key (music-name "seirei-no-uta"))
  (do ((alarmtime (set-alarmtime hour min) alarmtime))
      ((or (<= alarmtime (get-universal-time))
           (force-exit 'exit))
       (play-alarm 30 music-name))
    (sleep 1)
    ))
#|メイン関数総括試作
(defun time-keeper ()
        (init-alarmtime)
        (create window)
        (timer-loop)
        ())
|#

#|開発用メモ
実装を考えている機能
 GUI,任意の音楽を再生可能,スヌーズ機能,曜日別繰返機能
 複数のアラーム設定,アラームのオンオフ可能,音量制御,照明制御
 スケジューラーとしての機能(時間到達時に呼び出す関数を切り替える)
 タイマー等も付与可能？
 コマンドを打たないと止められないようなモードの搭載
今後の課題
 アラームの停止
 defstructでalarmtimeの型を作り利用する？
|#
