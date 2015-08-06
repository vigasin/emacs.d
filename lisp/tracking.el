;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(defun time-to-minutes (string)
  (let ((hm (split-string string ":")))
    (+ (* 60 (string-to-int (car hm)))
    (string-to-int (cadr hm)))))

(defun minutes-to-time (minutes)
  (concat
   (number-to-string (/ minutes 60))
   ":"
   (number-to-string (% minutes 60))
   ))

(defun interval-to-minutes (string)
  (let* ((split (split-string string "-"))
         (from (time-to-minutes (car split)))
         (to (time-to-minutes (cadr split))))
    (if (< from to)
        (- to from)
      (- (+ to (* 24 60)) from))))

(defun sum-interval (interval)
  (let (x, y) (split-string interval "-")))

(defun sum-day (str)
  (apply '+ (mapcar 'interval-to-minutes (split-string str ","))))

(interval-to-minutes "22:15 - 00:15")
(time-to-minutes "10:15")
(sum-day "10:15 - 14:25, 15:55 - 00:10")
(minutes-to-time 745)
