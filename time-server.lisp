;;未定義-time-scheduled send-time-signal case分岐
;;;(defparameter time-scheduled (unitime alarm-p light-p))
(defun read-no-hang (stream)
  (let ((str-st (make-string-output-stream)))
    (do ((chr (read-char-no-hang stream) (read-char-no-hang stream)))
        ((null chr) (get-output-stream-string str-st))
      (write-char chr str-st))))


(defun with-timeservser ()
  (with-open-socket (tserver :local-host "localhost"
                             :local-port 5001
                             :connect :passive
                             :reuse-address t)
    (loop
      (if (<= time-scheduled (get-universal-time))
          (send-time-signal))
      (if (setf st (accept-connection tserver))
          (progn
            (loop (if (listen st)
                      (case (read-no-hang st)
                        ())
                      )
                  (sleep 1))
            (close st)))
      (sleep 1)
      )))
