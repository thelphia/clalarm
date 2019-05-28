;;window system
(load "~/for-lisp/ltk/ltk")

;;aconf-init
(defstruct alarm-conf (unitime 9999999999)
           (wday '(nil nil nil nil nil nil nil))
           hour min month date wday-or-date music volume)
(defvar *alarm-conf-list* '())
(defun name-aconf ()
  (intern (concatenate 'string "ACONF"
                       (princ-to-string (get-universal-time)))))
(defmacro alarm-conf-init ()
  (let ((aconf-time (name-aconf)))
    `(setf *alarm-conf-list*
          (reverse
           (cons (defparameter ,aconf-time (make-alarm-conf))
                 (reverse *alarm-conf-list*))))
    ))
;;for-setf-aconf
(defun setter (slotname aconf n)
  (case slotname
    (min (setf (alarm-conf-min aconf) (parse-integer n)))
    (hour (setf (alarm-conf-hour aconf) (parse-integer n)))
    (wday (setf (nth (car n) (alarm-conf-wday aconf))
                (if (zerop (cadr n)) nil t)))
    (vol (setf (alarm-conf-volume aconf) (round n)))
    ))
;;wins-separated
(defun win-time (conf parent)
  (let* ((f (make-instance 'ltk:frame :master parent))
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
         )
    (ltk:pack f)
    (ltk:pack (list lab bhour lab1 bmin) :side :left)
    ))
(defun win-date (conf parent)
  (let* ((fdate (make-instance 'ltk:frame :master parent))
         (rdatep (make-instance 'ltk:radio-button
                                :master fdate :variable "d-or-wday"
                                :value 0 :text "日付を使用" ))
         (emonth (make-instance 'ltk:entry
                                :master fdate :text "month"))
         (edate (make-instance 'ltk:entry
                               :master fdate :text "date"))
         )
    (ltk:pack fdate)
    (ltk:pack (list rdatep emonth edate) :side :left)
    ))
(defun win-wday (conf parent state)
  (let* ((wdaystate state)
         (fwdays (make-instance 'ltk:frame
                                :master parent))
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
         )
    (ltk:pack fwdays)
    (ltk:pack
     (list rwdayp rwday1 rwday2 rwday3 rwday4 rwday5 rwday6 rwday7)
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
;;window-of-alarmsettings
(defun win-settigs
    (&key (conf (eval (car (reverse (alarm-conf-init))))))
  (let* ((fsetting (make-instance 'ltk:frame))
         (csetting (make-instance 'ltk:canvas :master fsetting))
         )
    (win-time conf fsetting)
    (win-date conf csetting)
    (win-wday conf csetting 'normal)
    (win-volume conf fsetting)
    (win-buttons conf fsetting)
    ))
;;generete window
(defun window1 ()
  (ltk:with-ltk ()
    (let* ()
      (ltk:wm-title ltk:*tk* "Alarm")
      (win-settigs)
      (testframe)
      )))
(setf *alarm-conf-list* nil)


(defun testframe ()
  (let* ((ftest (make-instance 'ltk:frame))
        (entest (make-instance 'ltk:entry
                               :master ftest :text "testform"))
        )
    (ltk:pack ftest)
    (ltk:pack entest)
    ))
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
