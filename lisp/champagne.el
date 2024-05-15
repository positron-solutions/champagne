;;; champagne.el --- Graphical countdowns  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Positron Solutions

;; Author:  Psionic K <73710933+psionic-k@users.noreply.github.com>
;; Keywords: games
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1") (posframe "1.4.2"))
;; Homepage: http://github.com/positron-solutions/champagne

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Now you can count down important events with friends & family from within
;; Emacs!  Subscriptions?  Yuck!  Stop paying $10 a month for "turn-key"
;; solutions to animate your countdowns.  They don't even have integration hooks
;; or customizations!

;;; Usage:

;; For a quick manual countdown, M-x champagne will just ask you for a number of
;; seconds.

;; For a basic countdown, starting right now:
;; (champagne 5)
;;
;; Count down to some point in the future:
;; (champagne nil "Wed Jan 23 00:00:00 2025")
;; (champagne nil "12:00am") ; will use tomorrow if necessary
;; (champagne nil "12:00") ; ⚠️ read as military time, tomorrow if necessary
;; (champagne nil 60) ; one minute from now
;; (champagne nil "2 hours 10 minutes") ; two hours and 10 minutes from now
;; (champagne nil (25945 48350 688785 440000)) ; you like Emacs time lists
;;
;; Count down and call a function at the beginning of the countdown
;; (champagne nil nil #'parrot-start-animation)
;;
;; Count down and call a function at the end of the countdown
;; (champagne nil nil nil #'snow)
;;
;; Count down with all behaviors
;; (champagne 60 "Wed Jan 23 00:00:00 2025" #'parrot-start-animation #'snow)

;;; Code:

(require 'posframe)
(require 'timer)

;; macros only
(eval-when-compile (require 'cl-lib))

(defgroup champagne nil "Count down." :prefix 'champagne :group 'champagne)

(defcustom champagne-default-seconds 10
  "Default seconds to count down."
  :group 'champagne
  :type 'integer)

(defcustom champagne-frame-time 0.02
  "How much time for each frame during animation."
  :group 'champagne
  :type 'float)

(defcustom champagne-buffer-name " *champagne*"
  "Buffer where countdown is rendered."
  :group 'champagne
  :type 'string)

(defcustom champagne-alpha 100
  "Opacity of foreground and background from 0-100."
  :group 'champagne
  :type 'natnum)

(defcustom champagne-alpha-background 100
  "Opacity of just the background."
  :group 'champagne
  :type 'natnum)

(defun champagne--read-N+ ()
  "Read a positive integer."
  (save-match-data
    (let ((str (read-from-minibuffer
                "Countdown duration (integer seconds): "
                (number-to-string champagne-default-seconds) nil nil nil)))
      (if (string-match-p "\\`[1-9][0-9]*\\'" str)
          (string-to-number str)
        (user-error "Please enter a positive integer")))))

(defun champagne--start (goal-time start-fun end-fun digits)
  "Count down to GOAL-TIME.
Call START-FUN and then start the animation, which will call
END-FUN when it's done.  DIGITS is determined from duration when
calling `champagne'."
  (when start-fun (funcall start-fun))
  (let ((animation-timer (timer-create)))
    (timer-set-function
     animation-timer
     #'champagne--animate (list goal-time end-fun animation-timer digits))
    (timer-set-time animation-timer (current-time) champagne-frame-time)
    (timer-activate animation-timer)))

(defun champagne--animate (goal-time end-fun animation-timer digits)
  "Animate a countdown until GOAL-TIME.
Call END-FUN when countdown finishes.  ANIMATION-TIMER is
canceled when GOAL-TIME is reached.  DIGITS is used to make
display behavior consistent."
  (if (time-less-p goal-time (current-time))
      (progn (cancel-timer animation-timer)
             (posframe-delete champagne-buffer-name)
             (when end-fun (funcall end-fun)))
    (let* ((diff (time-convert (time-subtract goal-time (current-time)) 'list))
           (landscape (> (frame-pixel-width) (frame-pixel-height)))
           (diff-seconds (+ 1(nth 1 diff)))
           (digits-remaining (length (number-to-string diff-seconds)))
           (diff-micros (nth 2 diff))
           (fraction (/ diff-micros 1000000.0))
           (buffer (get-buffer-create champagne-buffer-name))
           ;; You can customize the animation easing function and counter scale if
           ;; you add support here.
           (height-max-frac 0.8)
           (height-min-frac 0.1)
           (eased-frac (expt fraction 0.6))
           (height-cur-frac (+ (* eased-frac (- height-max-frac height-min-frac))
                               height-min-frac))
           (height-margin-frac (- height-max-frac height-cur-frac))
           ;; Note, fudge factor 0.6 is because letters are taller than they are wide.
           ;; TODO enable clipping the lines to avoid reflow nonsense.
           (font-scale  (* (float (if landscape (frame-height) (* 0.6 (frame-width))))
                           height-cur-frac)))
      (set-buffer buffer)
      (face-remap-set-base 'default :height font-scale)

      (unless (string=
               (buffer-substring-no-properties
                (max (- (point-max) digits-remaining) 1)
                (point-max))
               (number-to-string diff-seconds))
        (erase-buffer)
        ;; Make any leading spaces half width so that shorter numbers are
        ;; slightly shifted left.
        (let* ((padding (propertize (make-string (max 0 (- digits digits-remaining)) ?\40)
                                    'display '(space-width 0.5))))
          (insert (format " %s%d" padding diff-seconds))))

      ;; Customize the color in animation if you want full party mode
      ;; (put-text-property 2 (point-max) 'face '(:foreground "#ff00ff"))

      ;; Note to any hackers, anything you do that affects the child frame size
      ;; in animation will slow your display down too much for smooth visuals.
      ;; This includes borders, child borders, and refreshing the posframe.

      ;; expand the space character we appended at the front as the current
      ;; fraction goes down.  Horizontally, this scales by digits.
      (put-text-property
       1 2
       'display `(space-width
                  ,(* digits (/ height-margin-frac height-cur-frac 2.0))))

      ;; add a subscript translation that scales up as current fraction goes
      ;; down.
      (put-text-property
       (- (point-max) digits-remaining) (point-max)
       'display `(raise ,(- (/ height-margin-frac height-cur-frac 2.0))))

      ;; posframe show is run only after contents are available so that the
      ;; handler will do most of the work.
      (unless (posframe--find-existing-posframe buffer)
        (posframe-show
         buffer
         ;; Border will use child-frame-border color by default
         ;; :border-width 10
         ;; :border-color (face-attribute 'internal-border :background)

         ;; Use of timeout or refresh calls leads to considerable slowness
         ;; probably because it causes child frame resize.

         ;; alpha-background seems pgtk-exclusive
         ;; alpha is inconsistent for different window managers (and Emacs versions (¬_¬))
         ;; https://www.reddit.com/r/emacs/comments/v72tu6/new_emacs_frame_parameter_for_transparency/
         :override-parameters `((alpha . ,champagne-alpha)
                                (alpha-background . ,champagne-alpha-background)
                                (no-other-frame . t)
                                (no-accept-focus . t)
                                (no-other-frame . t))
         :poshandler 'posframe-poshandler-frame-center)))))

(declare-function diary-entry-time "diary-lib")
(defun champagne--future-diary-time (time-string)
  "Use `diary-time' to interpret TIME-STRING.
Always return a future time because countdowns to the past are
degenerate.  Returns nil if TIME-STRING is invalid according to
`diary-entry-time', which understands `timer-duration-words'."
  (when-let* ((now (decode-time))
              (hhmm (and (require 'diary-lib nil t)
                         (diary-entry-time time-string)))
              (hhmm (unless (< hhmm 0) hhmm)))
    (setf (decoded-time-minute now) (% hhmm 100))
    (setf (decoded-time-hour now) (/ hhmm 100))
    (let ((time (time-convert (encode-time now) 'list)))
      (if (time-less-p (current-time) time)
          time
        (time-add time (time-convert (days-to-time 1) 'list))))))

(defun champagne--string-to-time (time)
  "Make a best effort to convert TIME to something useful."
  (cond ((string-empty-p time) (current-time))
        ((timer-duration time) (time-add (current-time)
                                         (seconds-to-time
                                          (timer-duration time))))
        ((and (require 'diary-lib nil t)
              (> (diary-entry-time time) 0))
         (champagne--future-diary-time time))
        ((and (require 'parse-time nil t)
              ;; first value is nil in a lot of degenerate cases
              (car (parse-time-string time)))
         (time-convert
          (encode-time (parse-time-string time))
          'list))))

(defun champagne--read-time ()
  "Read a TIME.
Always returns an Emacs style time list, like `current-time'."
  (let ((time (read-string "Goal time.  Enter a duration from now or future \
time: ")))
    (unless (string-empty-p time)
      (if-let ((time (champagne--string-to-time time)))
          time
        ;; TODO could have sworn one of the options will format mm:ss to a time
        ;; today.
        (user-error "Goal time couldn't be decided
Valid formats - hh:mm
              - hh:mm:ss
              - 2 hours 35 min
              - 11:23pm
              - Wed Jan 1 00:00:00 2025")))))

;; TODO provide a transient interface
;;;###autoload
(defun champagne (&optional duration goal-time start-fun end-fun)
  "Count down.
DURATION is the number of seconds that will be counted down,
reaching GOAL-TIME.  If GOAL-TIME is not in the future far enough to allow the
entire countdown to finish, it is considered degenerate and the current time -
DURATION is used instead.

GOAL-TIME supports a number of formats:

- nil: start immediately
- number: seconds from now
- string: valid arguments to `timer-duration' or `timer-relative-time' or as a
  fallback, `parse-time-string'.  Examples of valid values:
  hh:mm
  hh:mm:ss
  2 hours 35 minutes
  2 hour 35 min
  11:23pm
  Wed Jan 1 00:00:00 2025
See `timer-duration-words' for details of using `timer-duration' style.  See
`diary-entry-time' for more valid 11:23PM style times.

- cons: used as a raw value like calling `current-time'

START-FUN will be called when the countdown begins.  END-FUN will
be called with the countdown finishes."
  (interactive (list (champagne--read-N+)
                     (champagne--read-time)))
  (let* ((digits (length (number-to-string duration)))
         (duration (time-convert (or duration champagne-default-seconds) nil))
         (goal-time (cond ((numberp goal-time)
                           (timer-relative-time (current-time) goal-time))
                          ((stringp goal-time)
                           (champagne--string-to-time goal-time))
                          ((listp goal-time) goal-time)
                          (t (time-add (current-time) duration))))
         (goal-time (if (time-less-p (time-subtract goal-time
                                                    duration)
                                     (current-time))
                        (time-add (current-time) duration)
                      goal-time))
         (start-time (time-subtract goal-time duration)))
    (run-at-time start-time nil #'champagne--start
                 goal-time start-fun end-fun digits)))

(provide 'champagne)
;;; champagne.el ends here
