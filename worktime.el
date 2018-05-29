(defun my-extract-minutes-of-hm-string(hm-string)
  "returns the minutes of a string like 9:42 -> 42 (and 0 if there are no minutes)"
  (let (
        (minutes (nth 1 (split-string hm-string ":")))
        )
    (if (eq minutes 'nil)
        0
      (string-to-number minutes)
      )
    )
  )

(defun my-extract-hours-of-hm-string(hm-string)
  "returns the hours of a string like 9:42 -> 9"
  (string-to-number
   (car
    (split-string hm-string ":")
    )
   )
  )

(defun my-hm-string-to-minutes(hm-string)
  "returns the minutes of a string like 2:42 -> 162"
  (let (
        ;; minutes is the second element after splitting with ":"
        (minutes (my-extract-minutes-of-hm-string hm-string))
        (hours (my-extract-hours-of-hm-string hm-string))
        )
    (+ minutes (* hours 60))
    )
  )

(defun minutes-to-time-str (minutes-total)
  (let (
        (hours (/ minutes-total 60))
        (minutes (mod minutes-total 60))
        )

    (format "%d:%d" hours minutes)
    )
  )

(defun remaining-work-time ()
  (interactive)

  (let ((current-line (thing-at-point 'line t)))
    (if
        (string-match "\\([0-9]+:[0-9]+\\) - \\([0-9]+:[0-9]+\\), \\([0-9]+:[0-9]+\\)" current-line)

        (let*
            (
             (start1-str (match-string 1 current-line))
             (end1-str (match-string 2 current-line))
             (start2-str (match-string 3 current-line))
             (start1 (my-hm-string-to-minutes start1-str))
             (end1 (my-hm-string-to-minutes end1-str))
             (start2 (my-hm-string-to-minutes start2-str))
             (period1 (- end1 start1))
             (remainder1 (- 480 period1))
             (home (+ start2 remainder1))
             )

          (insert (minutes-to-time-str home))
          )
      )))
