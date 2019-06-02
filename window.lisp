;;window systemA
(load "~/for-lisp/ltk/ltk")
;;;;;;;;;;wdayが初期化されない不具合
;;;GUI→main,time(by1sec)→main,main-read-if-do,stream経由？file経由
;;aconf-init
(defstruct alarm-conf (unitime 9999999999)
           wday0 wday1 wday2 wday3 wday4 wday5 wday6
           (hour 0) (min 0) month date wday-or-date
           (music "seirei-no-uta") (volume 0))
(defvar *alarm-conf-list* '())
(defun name-aconf ()
  (intern (concatenate 'string "ACONF"
                       (princ-to-string (get-universal-time)))))
(defun alarm-conf-init ()
  (let ((aconf (name-aconf)))
    (setf *alarm-conf-list*
          (append *alarm-conf-list*
                  (list
                   (eval (list 'defparameter aconf
                               '(make-alarm-conf)))
                   )))
    (eval aconf)))
;;for-setf-aconf
(defun setter (slotname aconf n)
  (case slotname
    (min (setf (alarm-conf-min aconf) (parse-integer n)))
    (hour (setf (alarm-conf-hour aconf) (parse-integer n)))
    (wday (let ((m (if (zerop (cadr n)) nil t)))
            (case (car n)
              (0 (setf (alarm-conf-wday0 aconf) m))
              (1 (setf (alarm-conf-wday1 aconf) m))
              (2 (setf (alarm-conf-wday2 aconf) m))
              (3 (setf (alarm-conf-wday3 aconf) m))
              (4 (setf (alarm-conf-wday4 aconf) m))
              (5 (setf (alarm-conf-wday5 aconf) m))
              (6 (setf (alarm-conf-wday6 aconf) m)))))
    (vol (setf (alarm-conf-volume aconf) (round n)))
    ))
(defun calunitime (aconf)
  (
;;wins-separated
(defun win-time (conf parent)
  (let* ((f (make-instance 'ltk:frame :master parent))
         (bhour (make-instance 'ltk:spinbox
                              :master f :wrap t :width 2
                              :from 0 :to 24 :increment 1 :text 0
                              :command (lambda (var)
                                         (setter 'hour conf var))
                              ))
         (bmin (make-instance 'ltk:spinbox
                              :master f :wrap t :width 2
                              :from 0 :to 59 :increment 1 :text 0
                              :command (lambda (var)
                                         (setter 'min conf var))
                              ))
         (lab (make-instance 'ltk:label
                               :master f
                               :text "時間"))
         (lab1 (make-instance 'ltk:label
                                :master f
                                :text " : "))
         )
    (ltk:pack f)
    (ltk:pack (list lab bhour lab1 bmin) :side :left)
    ))
(defun win-select-dw (parent use-wday)
  (let* ((fselect-dw (make-instance 'ltk:frame
                                    :master parent))
         (rdatep (make-instance 'ltk:radio-button
                                :master fselect-dw
                                :variable "d-or-wday"
                                :value 0 :text "日付を使用" ))
         (rwdayp (make-instance 'ltk:radio-button
                                :master fselect-dw
                                :variable "d-or-wday"
                                :value 1 :text "曜日を使用"))
         )
    (ltk:pack fselect-dw :side :left)
    (ltk:pack (list rdatep rwdayp))
    (setf (ltk:value rwdayp) (if use-wday 1 0))
    ))
(defun win-date (conf parent state)
  (let* ((fdate (make-instance 'ltk:frame :master parent))
         (emonth (make-instance 'ltk:entry
                                :master fdate :text "month"
                                :state state))
         (edate (make-instance 'ltk:entry
                               :master fdate :text "date"
                               :state state))
         )
    (ltk:pack fdate)
    (ltk:pack (list emonth edate) :side :left)
    ))
(defun win-wday (conf parent state)
  (let* ((fwdays (make-instance 'ltk:frame
                                :master parent))
         (rwday1 (make-instance 'ltk:check-button
                                :master fwdays :text "月"
                                :state state
                                :command (lambda (var)
                                           (setter 'wday conf
                                                   (list 0 var)))
                                ))
         (rwday2 (make-instance 'ltk:check-button
                                :master fwdays :text "火"
                                :state state
                                :command (lambda (var)
                                           (setter 'wday conf
                                                   (list 1 var)))
                                ))
         (rwday3 (make-instance 'ltk:check-button
                                :master fwdays :text "水"
                                :state state
                                :command (lambda (var)
                                           (setter 'wday conf
                                                   (list 2 var)))
                                ))
         (rwday4 (make-instance 'ltk:check-button
                                :master fwdays :text "木"
                                :state state
                                :command (lambda (var)
                                           (setter 'wday conf
                                                   (list 3 var)))
                                ))
         (rwday5 (make-instance 'ltk:check-button
                                :master fwdays :text "金"
                                :state state
                                :command (lambda (var)
                                           (setter 'wday conf
                                                   (list 4 var)))
                                ))
         (rwday6 (make-instance 'ltk:check-button
                                :master fwdays :text "土"
                                :state state
                                :command (lambda (var)
                                           (setter 'wday conf
                                                   (list 5 var)))
                                ))
         (rwday7 (make-instance 'ltk:check-button
                                :master fwdays :text "日"
                                :state state
                                :command (lambda (var)
                                           (setter 'wday conf
                                                   (list 6 var)))
                                ))
         )
    (ltk:pack fwdays)
    (ltk:pack
     (list rwday1 rwday2 rwday3 rwday4 rwday5 rwday6 rwday7)
     :side :left)
    ))
(defun win-volume (conf parent)
  (let* ((fvolume (make-instance 'ltk:frame :master parent))
         (vvlume (make-instance 'ltk:scale
                                :master fvolume
                                :from 0 :to 100 :length 150
                                :command (lambda (var)
                                           (setter 'vol conf var))
                                ))
         (labvol (make-instance 'ltk:label
                                :master fvolume :text "音量"))
         )
    (ltk:pack fvolume)
    (ltk:pack (list labvol vvlume) :side :left)
    ))
(defun win-buttons (conf parent)
  (let* ((fcalc (make-instance 'ltk:frame :master parent))
         (bunivt (make-instance 'ltk:button
                                :master fcalc :text "保存"
                                :command (+ 1 2)
                                ))
         (bshow (make-instance 'ltk:button
                               :master fcalc :text "show"
                               :command (write conf)))
         )
    (ltk:pack fcalc)
    (ltk:pack (list bunivt bshow) :side :left)
    ))
;;;;;frame:add width option
;;;(conf (eval (car (reverse (alarm-conf-init)))))
;;window-of-alarmsettings
(defun win-settigs
    (parent &key (conf (alarm-conf-init))
       (use-wday t))
  (let* ((fsetting (make-instance 'ltk:frame :master parent))
         (fdw (make-instance 'ltk:frame :master fsetting))
         (csetting (make-instance 'ltk:canvas :master fdw))
         )
    (ltk:pack fsetting)
    (win-time conf fsetting)
    (ltk:pack fdw)
    (win-select-dw fdw use-wday)
    (ltk:pack csetting :side :left)
    (win-date conf csetting (if use-wday 'disable 'normal))
    (win-wday conf csetting (if use-wday 'normal 'disable))
    (win-volume conf fsetting)
    (win-buttons conf fsetting)
    ))
;;win-select-alarm-config
(defun win-alarm-list (parent)
  (let* ((clist (make-instance 'ltk:canvas :master parent))
         (bnewconf (make-instance 'ltk:button
                                  :master parent :text "+"
                                  :command (+ 1 2)
                                  ))
         (conf-frames (make-instance 'ltk:frame :master clist))
         )
    (ltk:pack clist)
    (ltk:pack conf-frames)
    (ltk:pack bnewconf :side :bottom)
    ))
;;generete window
(defun window1 ()
  (ltk:with-ltk ()
    (let* ((winleft (make-instance 'ltk:frame))
           (winright (make-instance 'ltk:frame))
           (cright (make-instance 'ltk:canvas :master winright))
           )
      (ltk:wm-title ltk:*tk* "Alarm")
      (ltk:pack winleft :side :left)
      (win-alarm-list winleft)
      (ltk:pack winright :side :left)
      (ltk:pack cright)
      (win-settigs cright)
      )))
(setf *alarm-conf-list* nil)


#|
(defun window1 ()
  (ltk:with-ltk ()
    (let* ((wdaystate 'normal)
           (conf (eval (car (reverse (alarm-conf-init)))))
           (win (make-instance 'ltk:frame))
           (winright (make-instance 'ltk:frame :master win))
           (f (make-instance 'ltk:frame :master winright))
           (b (make-instance 'ltk:button
                            :master f
                            :text "show"
                            :command (lambda () ;;for-test
                                       (format t "~a:~a"
                                               (alarm-conf-hour conf)
                                               (alarm-conf-min conf)))
                            ))
           (bhour (make-instance 'ltk:spinbox
                              :master f :wrap t :width 2
                              :from 0 :to 24 :increment 1 :text 12
                              :command (lambda (var)
                                         (setter 'hour conf var))
                              ))
           (bmin (make-instance 'ltk:spinbox
                              :master f :wrap t :width 2
                              :from 0 :to 59 :increment 1 :text 18
                              :command (lambda (var)
                                         (setter 'min conf var))
                              ))
           (lab (make-instance 'ltk:label
                               :master f
                               :text "時間"))
           (lab1 (make-instance 'ltk:label
                                :master f
                                :text " : "))
           (winleft (make-instance 'ltk:frame
                              :master win
                              :width 5 :height 50));;いらないかも
           (c1 (make-instance 'ltk:canvas
                              :master winleft))
           (fdate (make-instance 'ltk:frame :master winright))
           (rdatep (make-instance 'ltk:radio-button
                                  :master fdate :variable "d-or-wday"
                                  :value 0 :text "日付を使用" ))
           (emonth (make-instance 'ltk:entry
                                  :master fdate :text "month"))
           (edate (make-instance 'ltk:entry
                                 :master fdate :text "date"))
           (fwdays (make-instance 'ltk:frame
                                  :master winright))
           (rwdayp (make-instance 'ltk:radio-button
                                  :master fwdays :variable "d-or-wday"
                                  :value 1 :text "曜日を使用"))
           (rwday1 (make-instance 'ltk:check-button
                                  :master fwdays :text "月"
                                  :state wdaystate
                                  :command (lambda (var)
                                             (setter 'wday conf
                                                     (list 0 var)))
                                  ))
           (rwday2 (make-instance 'ltk:check-button
                                  :master fwdays :text "火"
                                  :state wdaystate
                                  :command (lambda (var)
                                             (setter 'wday conf
                                                     (list 1 var)))
                                  ))
           (rwday3 (make-instance 'ltk:check-button
                                  :master fwdays :text "水"
                                  :command (lambda (var)
                                             (setter 'wday conf
                                                     (list 2 var)))
                                  ))
           (rwday4 (make-instance 'ltk:check-button
                                  :master fwdays :text "木"
                                  :command (lambda (var)
                                             (setter 'wday conf
                                                     (list 3 var)))
                                  ))
           (rwday5 (make-instance 'ltk:check-button
                                  :master fwdays :text "金"
                                  :command (lambda (var)
                                             (setter 'wday conf
                                                     (list 4 var)))
                                  ))
           (rwday6 (make-instance 'ltk:check-button
                                  :master fwdays :text "土"
                                  :command (lambda (var)
                                             (setter 'wday conf
                                                     (list 5 var)))
                                  ))
           (rwday7 (make-instance 'ltk:check-button
                                  :master fwdays :text "日"
                                  :command (lambda (var)
                                             (setter 'wday conf
                                                     (list 6 var)))
                                  ))
           (fvolume (make-instance 'ltk:frame :master winright))
           (vvlume (make-instance 'ltk:scale
                                  :master fvolume
                                  :from 0 :to 100 :length 150
                                  :command (lambda (var)
                                             (setter 'vol conf var))
                                  ))
           (fcalc (make-instance 'ltk:frame :master winright))
           (bunivt (make-instance 'ltk:button
                                  :master fcalc :text "保存"
                                  :command (+ 1 2)
                                  ))
           (bclear (make-instance 'ltk:button
                                  :master c1 :text "clear"
                                  :command (lambda () (+ 1 1))
                                  ))
           )
      (ltk:wm-title ltk:*tk* "Alarm")
      (ltk:pack win)
      (ltk:pack winleft :side :left)
      (ltk:pack c1)
      (ltk:pack bclear)
      (ltk:pack winright :side :left)
      (ltk:pack f)
      (ltk:pack
       (list lab bhour lab1 bmin b)
       :side :left)
      (ltk:pack fdate)
      (ltk:pack
       (list rdatep emonth edate)
       :side :left)
      (setf (ltk:value rdatep) 0)
      (ltk:pack fwdays)
      (setf wdaystate 'disable)
      (ltk:pack
       (list rwdayp rwday1 rwday2 rwday3 rwday4 rwday5 rwday6 rwday7)
       :side :left)
      (setf wdaystate 'disable)
      (ltk:pack fvolume)
      (ltk:pack vvlume :side :left)
      (ltk:pack fcalc)
      (ltk:pack bunivt :side :right)
      (testframe)
      )))
|#
