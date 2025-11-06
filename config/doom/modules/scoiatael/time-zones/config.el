;;; config.el ---                                  -*- lexical-binding: t; -*-

(use-package! time-zones
  :config
  (setq time-zones--city-list
        '(((country . "Netherlands") (state . "North Holland") (city . "Amsterdam") (timezone . "Europe/Amsterdam") (latitude . "52.37403000") (longitude . "4.88969000"))
          ((country . "Poland") (state . "Mazovia") (city . "Warsaw") (timezone . "Europe/Warsaw") (latitude . "52.22977000") (longitude . "21.01178000"))
          ((country . "United Arab Emirates") (state . "Dubai") (city . "Dubai") (timezone . "Asia/Dubai") (latitude . "25.06570000") (longitude . "55.17128000"))
          ((country . "Japan") (state . "Ōsaka") (city . "Osaka") (timezone . "Asia/Tokyo") (latitude . "34.69374000") (longitude . "135.50218000"))
          ((country . "Taiwan") (state . "Taichung") (city . "Taichung") (timezone . "Asia/Taipei") (latitude . "24.14690000") (longitude . "120.68390000"))
          ((country . "Vietnam") (state . "Nghệ An") (city . "Vinh") (timezone . "Asia/Ho_Chi_Minh") (latitude . "18.67337000") (longitude . "105.69232000"))
          ((country . "Canada") (state . "Alberta") (city . "Edmonton") (timezone . "America/Toronto") (latitude . "53.55014000") (longitude . "-113.46871000"))
          ((country) (state) (city) (timezone . "UTC") (latitude . "0") (longitude . "0") (flag . "🕒"))))
  (setq time-zones--home-city
        '((country . "Poland") (state . "Mazovia") (city . "Warsaw") (timezone . "Europe/Warsaw") (latitude . "52.22977000") (longitude . "21.01178000")))
  (map! :leader :n "T" #'time-zones)
  (map! :map time-zones-mode-map
        :n "+" #'time-zones-add-city
        :n "D" #'time-zones-delete-city-at-point
        :n "h" #'time-zones-mark-home-at-point
        :n "r" #'time-zones-refresh
        :n "g" #'time-zones-refresh
        :n "f" #'time-zones-time-forward
        :n "b" #'time-zones-time-backward
        :n "F" #'time-zones-time-forward-hour
        :n "B" #'time-zones-time-backward-hour
        :n "j" #'time-zones-jump-to-date
        :n "(" #'time-zones-toggle-showing-details
        :n "?" #'time-zones-toggle-showing-help
        :n "n" #'next-line
        :n "p" #'previous-line
        :n "q" #'kill-buffer-and-window))
