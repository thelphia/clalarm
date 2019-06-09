(ql:quickload :trivial-shell)
(trivial-shell:shell-command
 "echo \"m 17 w  w 17 0 m 18 r  pud 18 u\" > /dev/pigpio")
(defun light-max ()
    (trivial-shell:shell-command
     "python3 ~/irrp.py -p -g17 -f ~/codes light:on"))
(defun light-off ()
    (trivial-shell:shell-command
     "python3 ~/irrp.py -p -g17 -f ~/codes light:off"))
(defun light-mid ()
    (trivial-shell:shell-command
     "python3 ~/irrp.py -p -g17 -f ~/codes light:middle"))
(defun light-time-control (lightstate time)
  (if (< time (get-universal-time))
      (return-from light-time-control nil))
  (do () ((<= time (get-universal-time))
          (case lightstate
            (max (light-max))
            (off (light-off))
            (mid (light-mid))))
    (sleep 1)))
(defun light-schedule ()
  (light-time-control 'max
                      (encode-universal-time 0 30 (- 29 9) 9 6 2019)))
(defun read-no-hang (stream)
  (let ((str-st (make-string-output-stream)))
    (do ((chr (read-char-no-hang stream) (read-char-no-hang stream)))
        ((null chr) (get-output-stream-string str-st))
      (write-char chr str-st))))
(defun read-socket-loop (st)
  (loop
     (if (listen st)
         (case (intern (read-no-hang st))
           (lightmax (light-max) (format t "max~%"))
           (lightmid (light-mid) (format t "mid~%"))
           (lightoff (light-off) (format t "off~%"))
           (close (return-from read-socket-loop nil))))
     (sleep 1)))
(defun open-light-server ()
  (with-open-socket (frsoc :local-port 7815
                           :local-host "hayamatokine.local"
                           :connect :passive
                           :reuse-address t)
    (format t "connecting")
    (defparameter st (accept-connection frsoc))
    (format t "connect seccess~%")
    (read-socket-loop st)
    (close st)))      