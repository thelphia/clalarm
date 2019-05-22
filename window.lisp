;;window system
(load "~/for-lisp/ltk/ltk")

(defstruct alarm-conf (unitime 9999999999)
           (wday '(nil nil nil nil nil nil nil))
           hour min month date wday-or-date music)
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

(defun setter (slotname aconf n)
  (case slotname
    (min (setf (alarm-conf-min aconf) (parse-integer n)))
    (hour (setf (alarm-conf-hour aconf) (parse-integer n)))
    (wday (setf (nth (car n) (alarm-conf-wday aconf))
                 (if (zerop (cadr n)) nil t)))))

(defun window1 ()
  (ltk:with-ltk ()
    (let* ((conf (eval (car (reverse (alarm-conf-init)))))
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
           (b2 (make-instance 'ltk:spinbox
                              :master f :wrap t :width 2
                              :from 0 :to 24 :increment 1
                              :command (lambda (var)
                                         (setf
                                          (alarm-conf-hour conf) var))
                              ))
           (b3 (make-instance 'ltk:spinbox
                              :master f :wrap t :width 2
                              :from 0 :to 59 :increment 1
                              :command (lambda (var)
                                         (setf
                                          (alarm-conf-min conf) var))
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
           (f3 (make-instance 'ltk:frame
                              :master winright))
           (rwday1 (make-instance 'ltk:check-button
                                  :master f3
                                  :text "月"
                                  :command (lambda (var)
                                             (setf
                                              (nth 0
                                               (alarm-conf-wday conf))
                                              var))
                                  ))
           (rwday2 (make-instance 'ltk:check-button
                                  :master f3 :text "火"
                                  :command (lambda (var)
                                             (setf
                                              (nth 1
                                               (alarm-conf-wday conf))
                                              var))
                                  ))
           (rwday3 (make-instance 'ltk:check-button
                                  :master f3 :text "水"
                                  :command (lambda (var)
                                             (setf
                                              (nth 2
                                               (alarm-conf-wday conf))
                                              var))
                                  ))
           (rwday4 (make-instance 'ltk:check-button
                                  :master f3 :text "木"
                                  :command (lambda (var)
                                             (setf
                                              (nth 3
                                               (alarm-conf-wday conf))
                                              var))
                                  ))
           (rwday5 (make-instance 'ltk:check-button
                                  :master f3 :text "金"
                                  :command (lambda (var)
                                             (setf
                                              (nth 4
                                               (alarm-conf-wday conf))
                                              var))
                                  ))
           (rwday6 (make-instance 'ltk:check-button
                                  :master f3 :text "土"
                                  :command (lambda (var)
                                             (setf
                                              (nth 5
                                               (alarm-conf-wday conf))
                                              var))
                                  ))
           (rwday7 (make-instance 'ltk:check-button
                                  :master f3 :text "日"
                                  :command (lambda (var)
                                             (setf
                                              (nth 6
                                               (alarm-conf-wday conf))
                                              var))
                                  ))
           )
      (ltk:wm-title ltk:*tk* "Alarm")
      (ltk:pack win)
      (ltk:pack winleft :side :left)
      (ltk:pack c1)
      (ltk:pack winright :side :left)
      (ltk:pack f)
      (ltk:pack lab :side :left)
      (ltk:pack b2 :side :left)
      (ltk:pack lab1 :side :left)
      (ltk:pack b3 :side :left)
      (ltk:pack b :side :left)
      (ltk:pack f3)
      (ltk:pack rwday1 :side :left)
      (ltk:pack rwday2 :side :left)
      (ltk:pack rwday3 :side :left)
      (ltk:pack rwday4 :side :left)
      (ltk:pack rwday5 :side :left)
      (ltk:pack rwday6 :side :left)
      (ltk:pack rwday7 :side :left)
      )))
(setf *alarm-conf-list* nil)








