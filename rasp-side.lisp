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
                      (encode-universal-time 0 49 (- 22 9) 8 6 2019)))


