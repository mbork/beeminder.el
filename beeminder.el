;;; beeminder.el --- Emacs client for Beeminder

;; Copyright (C) 2015 Marcin 'mbork' Borkowski

;; Author: Marcin Borkowski <mbork@mbork.pl>
;; Keywords: calendar
;; Package-Requires: ((request "0.2.0"))

;; This file is NOT part of GNU Emacs.

;; beeminder.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; beeminder.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with beeminder.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; beeminder.el is an Emacs client for the Beeminder service.
;; Beeminder (taglined "a reminder with a sting") is
;; a motivation/quantified self tool based on behavioral economics
;; principles.

(require 'json)
(require 'request)
(require 'cl-lib)
(require 'ewoc)
(require 'seq)

;;; Code:


;; Settings

(defgroup beeminder nil
  "An Emacs client for Beeminder."
  :group 'applications)

(defcustom beeminder-username ""
  "User name for the Beeminder account."
  :type 'string
  :group 'beeminder)

(defcustom beeminder-auth-token ""
  "Authentication token for Beeminder.
You can retrieve it from the URL
`https://www.beeminder.com/api/v1/auth_token.json'."
  :type 'string
  :group 'beeminder)

(defcustom beeminder-api-url "https://www.beeminder.com/api/v1/users/"
  "The URL for making API calls."
  :type 'string
  :group 'beeminder)

(defvar beeminder-goals nil
  "The list of sexps representing goals.
Updated by `beeminder-get-goals'.")

(defcustom beeminder-default-timeout 4
  "Default timeout for HTTP requests sent to beeminder, in seconds."
  :type 'number
  :group 'beeminder)


;; Beeminder mode

(define-derived-mode beeminder-mode special-mode "Beeminder"
  "A major mode for a buffer with Beeminder goal list.")

(defun next-goal (count)
  "Move COUNT goals forward in the Beeminder buffer."
  (interactive "p")
  (ewoc-goto-next beeminder-goals-ewoc
		  (if (beeminder-before-first-goal-p)
		      (1- count)
		    count)))

(defun previous-goal (count)
  "Move COUNT goals back in the Beeminder buffer.
If on the first goal, move to (point-min)."
  ;; If point is before the place `previous-goal' would move it, move
  ;; to (point-min).  This jumps to the beginning from any place
  ;; before the first node, but won't work when point is on Nth goal
  ;; and `count' is greater than N.  This doesn't seem a big deal, so
  ;; let's just hope nobody notices that.
  (interactive "p")
  (when (<= (point)
	    (progn (ewoc-goto-prev beeminder-goals-ewoc count)
		   (point)))
    (goto-char (point-min))))

(define-key beeminder-mode-map (kbd "n") #'next-goal)
(define-key beeminder-mode-map (kbd "p") #'previous-goal)


;; API interface

(defun beeminder-create-api-url (string)
  "Prepend Beeminder site address and the username to STRING.
STRING should begin with a slash."
  (concat beeminder-api-url beeminder-username string))

(defun beeminder-request-get (request &optional timeout)
  "Send a GET REQUEST to beeminder.com, with TIMEOUT.
Add the necessary details (including the username and the auth
token)."
  (request-response-data
   (request (concat (beeminder-create-api-url request)
		    (if (string-match "\\?" request) "&" "?") ; this is hackish...
		    "auth_token="
		    beeminder-auth-token)
	    :parser #'json-read
	    :sync t
	    :timeout (or timeout beeminder-default-timeout))))

(defun beeminder-request-post (request data &optional timeout)
  "Send a POST REQUEST with given DATA and TIMEOUT to beeminder.com.
Add the username and the auth token."
  (request-response-data
   (request (beeminder-create-api-url request)
	    :type "POST"
	    :data (concat
		   (format "auth_token=%s&" beeminder-auth-token)
		   data)
	    :parser #'json-read
	    :sync t
	    :timeout (or timeout beeminder-default-timeout))))

(defun beeminder-request-delete (request &optional timeout)
  "Send a DELETE request to beeminder.com, with TIMEOUT.
Add the necessary details (username and the auth token)."
  (request-response-data
   (request (concat (beeminder-create-api-url request)
		    (if (string-match "\\?" request) "&" "?") ; this is hackish...
		    "auth_token="
		    beeminder-auth-token)
	    :type "DELETE"
	    :parser #'json-read
	    :sync t
	    :timeout (or timeout beeminder-default-timeout))))

(defun beeminder-request-put (request data &optional timeout)
  "Send a PUT request to beeminder.com, with TIMEOUT.
Add the necessary details (username and the auth token)."
  (request-response-data
   (request (beeminder-create-api-url request)
	    :type "PUT"
	    :data (concat
		   (format "auth_token=%s&" beeminder-auth-token)
		   data)
	    :parser #'json-read
	    :sync t
	    :timeout (or timeout beeminder-default-timeout))))


;; API calls (currently synchronous only)

(defun last-goal-midnight (goal-deadline now)
  "Return the last \"midnight\" for GOAL-DEADLINE, counting from NOW.
GOAL-DEADLINE is an offset from real midnight in seconds, NOW is
a time value."
  (let* ((now-decoded (decode-time now))
	 (last-real-midnight (encode-time 0 0 0
					  (cadddr now-decoded)
					  (nth 4 now-decoded)
					  (nth 5 now-decoded)
					  (nth 8 now-decoded)))
	 (last-midnight (+ goal-deadline
			   (if (< goal-deadline 0) (* 24 60 60) 0)
			   (time-to-seconds last-real-midnight))))
    (if (> last-midnight
	   (time-to-seconds now))
	(- last-midnight (* 24 60 60))
      last-midnight)))

(defun last-user-midnight (now)
  "Return the last \"midnight\" counting from NOW, as Unix timestamp.
Take `beeminder-when-the-day-ends' into consideration."
  (let* ((now-decoded (decode-time now))
	 (last-real-midnight (encode-time 0 0 0
					  (cadddr now-decoded)
					  (nth 4 now-decoded)
					  (nth 5 now-decoded)
					  (nth 8 now-decoded)))
	 (last-midnight (+ (time-to-seconds last-real-midnight)
			   beeminder-when-the-day-ends)))
    (if (> last-midnight (time-to-seconds now))
	(- last-midnight (* 24 60 60))
      last-midnight)))

(defcustom beeminder-history-length 7
  "Number of days from which to load datapoints.")

(defun beeminder-get-goals ()
  "Get all the user's Beeminder goals.
Return a vector of sexps, each describing one goal."
  (let* ((goals (append (beeminder-request-get "/goals.json") nil)) ; goal data
	 (datapoints-data (cdr (assoc 'goals ; datapoints data
				      (beeminder-request-get
				       (format ".json?diff_since=%d"
					       (- (time-to-seconds
						   (beeminder-current-time))
						  (* beeminder-history-length 24 60 60)))))))
	 (datapoints			; datapoints alone
	  (mapcar
	   (lambda (goal)
	     (cons (cdr (assoc 'slug goal))
		   (append (cdr (assoc 'datapoints goal)) nil)))
	   datapoints-data))
	 (deadlines			; deadlines alone
	  (mapcar
	   (lambda (goal)
	     (cons (cdr (assoc 'slug goal))
		   (cdr (assoc 'deadline goal))))
	   goals))
	 (today-values
	  (mapcar
	   (lambda (goal)
	     (let ((last-midnight (if beeminder-use-goal-midnight-today-values
				      (last-goal-midnight (cdr (assoc (car goal) deadlines))
							  (beeminder-current-time))
				    (last-user-midnight (beeminder-current-time)))))
	       (cons (car goal)
		     (cl-reduce #'+
				(mapcar (lambda (datapoint)
					  (if (> (cdr (assoc 'timestamp datapoint))
						 last-midnight)
					      (cdr (assoc 'value datapoint))
					    0))
					(cdr goal))))))
	   datapoints)))
    (setq beeminder-goals (mapcar
			   (lambda (goal)
			     (let ((slug-str (cdr (assoc 'slug goal))))
			       (cons (cons 'datapoints
					   (cdr (assoc slug-str datapoints)))
				     (cons
				      (cons 'donetoday
					    (cdr (assoc
						  (cdr (assoc 'slug goal))
						  today-values)))
				      goal))))
			   goals))
    (mapc (lambda (goal)
	    (let ((slug (intern (cdr (assoc 'slug goal)))))
	      (if (and (cdr (assoc slug beeminder-dirty-alist))
		       (/= (cdr (assoc 'curval goal))
			   (cdr (assoc slug beeminder-dirty-alist))))
		  (setq beeminder-dirty-alist (assq-delete-all slug beeminder-dirty-alist)))))
	  goals)))

(defun beeminder-refresh-goal (slug-str)
  "Refresh autodata and graph of the goal named SLUG-STR."
  (beeminder-request-get (concat "/goals/" slug-str "/refresh_graph.json")))


;; Submitting datapoints

(defun beeminder-read-string (prompt &optional initial-input history default-value inherit-input-method)
  "Replacement for `read-string', showing the default."
  (read-string (if default-value
		   (format "%s (default %s): "
			   (if (string-match "^\\(.+\\)\\(: \\)$" prompt)
			       (match-string 1 prompt)
			       prompt)
			   default-value)
		 prompt)
	       initial-input history default-value inherit-input-method))

(defun beeminder-before-first-goal-p ()
  "Return t if the point is before the first goal."
  (< (point)
     (ewoc-location (ewoc-nth beeminder-goals-ewoc 0))))

(defun beeminder-get-slug (goal)
  "Return the slug of GOAL."
  (intern (cdr (assoc 'slug goal))))

(defun beeminder-slug-to-goal (slug)
  "Return the goal corresponding to SLUG."
  (cl-find slug beeminder-goals :key #'beeminder-get-slug))

(defun beeminder-slug-to-gnode (slug)
  "Return the goal node corresponding to SLUG."
  (let ((gnode (ewoc-nth beeminder-goals-ewoc 0)))
    (while (and gnode
		(not
		 (eq slug (beeminder-get-slug (ewoc-data gnode)))))
      (setq gnode (ewoc-next beeminder-goals-ewoc gnode)))
    gnode))

(defvar beeminder-minibuffer-history nil
  "History of goal slug-strs entered through minibuffer.")

(defun beeminder-read-slug (&optional default)
  "Return a slug read from minibuffer or DEFAULT.
DEFAULT should be a symbol."
  (intern (completing-read (if default
			       (format "Goal slug (default %s): " default)
			     "Goal slug: ")
			   (mapcar (lambda (goal)
				     (symbol-name (beeminder-get-slug goal)))
				   beeminder-goals)
			   nil
			   t
			   nil
			   'beeminder-minibuffer-history
			   (if default (symbol-name default)))))

(defun current-or-read-goal ()
  "Return the goal the point is on.
If the point is before the first goal or in a buffer whose mode
is not `beeminder-mode', use `beeminder-read-slug' to ask for the
goal slug and return that goal instead."
  (if (or (not (eq major-mode 'beeminder-mode))
	  (beeminder-before-first-goal-p))
      (let ((default (aif (ewoc-nth beeminder-goals-ewoc 0)
			 (beeminder-get-slug (ewoc-data it)))))
	(beeminder-slug-to-goal
	 (beeminder-read-slug default)))
    (ewoc-data (ewoc-locate beeminder-goals-ewoc))))

(defun beeminder-current-time-hmsz-string (&optional timestamp)
  "Return TIMESTAMP (Unix time) as a string.
Use current time by default.  Format is hh:mm:ss tz."
  (let ((decoded-time (decode-time (or (seconds-to-time timestamp)
				       (beeminder-current-time)))))
    (format "%02d:%02d:%02d %s"
	    (caddr decoded-time)
	    (cadr decoded-time)
	    (car decoded-time)
	    (cadr (current-time-zone (apply #'encode-time decoded-time))))))

(defvar beeminder-dirty-alist '()
  "Alist of slugs and \"curval\" values of changed goals.
A goal is put here by `beeminder-submit-datapoint' and cleared
from the list by `beeminder-get-goals' (if the retrieved data are
actually updated, which can take from a few seconds to even a few
minutes).")

(defgroup beeminder-faces nil
  "Faces used be the Beeminder client."
  :group 'beeminder)

(defface beeminder-dirty '((t :slant italic :foreground "grey50"))
  "Face for displaying \"dirty\" goals, i.e., goals for which
a datapoint was submitted but had ot yet been reloaded."
  :group 'beeminder-faces)

(defun ask-for-timestamp (&optional default)
  "Ask the user for the timestamp, and return it as Unix time.
If `org-read-date' is present, use that; if not, fall back to
`safe-date-to-time' and augment the result with current time."
  (time-to-seconds
   (if (fboundp 'org-read-date)
       (org-read-date t t nil nil default)
     (let (time)
       (while
	   (progn (setq time (safe-date-to-time (beeminder-read-string "Date+time: ")))
		  (not
		   (y-or-n-p
		    (format-time-string "Time entered: %c. Confirm? " time)))))
       time))))

(defun beeminder-default-comment (&optional timestamp)
  "Generate the default comment for the given TIMESTAMP."
  (concat
   "via Emacs at "
   (beeminder-current-time-hmsz-string timestamp)))

(defun beeminder-ask-for-comment (slug-str amount &optional default-comment)
  "Ask the user for the comment for the goal named SLUG-STR.
Include AMOUNT in the question, and default to DEFAULT-COMMENT."
  (beeminder-read-string
   (format "Comment for amount %d for goal %s: " amount slug-str)
   nil nil default-comment))

(defcustom beeminder-ask-for-comment t
  "Non-nil means ask for comment when a goal is submitted.
This also serves as a confirmation that the user actually wants
to submit data to Beeminder (especially that the question
includes the goal slug and amount), so disabling of this option
is discouraged.")

(defun beeminder-make-goal-dirty (slug)
  "Make the goal with SLUG dirty."
  (setf (alist-get slug beeminder-dirty-alist)
	(cdr (assoc 'curval (beeminder-slug-to-goal slug))))
  (aif (beeminder-slug-to-gnode slug)
      (ewoc-invalidate beeminder-goals-ewoc it)))

(defun beeminder-clear-dirty-goals ()
  "Clear all dirty goals manually.
This may be needed in rare circumstances, namely when
successfully submitting a datapoint of 0."
  (interactive)
  (setq beeminder-dirty-alist ())
  (save-current-goal
    (ewoc-refresh beeminder-goals-ewoc)))

(defun beeminder-submit-datapoint (slug-str amount &optional comment timestamp print-message)
  "Submit a datapoint to Beeminder goal SLUG-STR with AMOUNT.
Additional data are COMMENT and TIMESTAMP (as Unix time).  If
COMMENT is nil, then ask the user for the comment.  If TIMESTAMP
is nil, assume now.  If PRINT-MESSAGE is non-nil, print suitable
messages in the echo area.

If called interactively, ask for SLUG-STR (with completion) unless the
point is on a goal node.  Then, ask for AMOUNT unless the user
provided a numeric argument, in which case take the argument.  Then,
ask for COMMENT, proposing a reasonable default, unless the option
`beeminder-ask-for-comment' is nil.  If called with a prefix argument
of \\[universal-argument], ask also for TIMESTAMP.  If called with
a prefix argument of `-', use previous day as the TIMESTAMP."
  (interactive
   (let* ((slug-str (cdr (assoc 'slug (current-or-read-goal))))
	  (yesterdayp (eq current-prefix-arg '-))
	  (amount (if (numberp current-prefix-arg)
		      current-prefix-arg
		    (string-to-number (beeminder-read-string
				       (format "Datapoint value for %s%s: "
					       slug-str
					       (if yesterdayp " (yesterday)" ""))
				       nil nil "1"))))
	  (current-timestamp (time-to-seconds (beeminder-current-time))))
     (list slug-str
	   amount
	   (unless beeminder-ask-for-comment
	     (beeminder-default-comment current-timestamp))
	   (or (when yesterdayp
		 (- current-timestamp (* 24 60 60)))
	       (when (consp current-prefix-arg)
		 (ask-for-timestamp))
	       current-timestamp)
	   t)))
  (if print-message (message (format "Submitting datapoint of %d for goal %s..." amount slug-str)))
  (let ((timestamp (or timestamp (time-to-seconds (beeminder-current-time)))))
    (unless (beeminder-request-post (format "/goals/%s/datapoints.json" slug-str)
				    (format "value=%f&comment=%s&timestamp=%d"
					    amount
					    (or comment (beeminder-ask-for-comment
							 slug-str
							 amount
							 (beeminder-default-comment timestamp)))
					    timestamp))
      (sit-for beeminder-default-timeout)
      (error "Submitting failed, check your internet connection")))
  (if print-message (message (format "Submitting datapoint of %d for goal %s...done" amount slug-str)))
  (let* ((slug (intern slug-str))
	 (goal (beeminder-slug-to-goal slug)))
    (cl-incf (alist-get 'donetoday goal) amount)
    (beeminder-make-goal-dirty slug)))

(define-key beeminder-mode-map (kbd "RET") #'beeminder-submit-datapoint)


;; Sorting EWOC

(defun true (&rest args)
  "Always return t (irrespective of ARGS)."
  t)

(defun ewoc-sort (ewoc pred)
  "Sort EWOC, comparing its nodes using PRED.
Since the author of EWOC didn't really care for sorting, and
neither do I, we just first collect the nodes into a list, sort
it using Elisp's sort, and then recreate the EWOC."
  (let ((ewoc-list (ewoc-collect ewoc #'true)))
    (ewoc-filter ewoc #'ignore)
    (mapcar (lambda (node) (ewoc-enter-last ewoc node))
	    (sort ewoc-list pred))))


;; Displaying goals

(defvar beeminder-human-time-use-weekday t
  "Non-nil means that `beeminder-human-time' uses weekday names.
Otherwise, use number of days from today.")

(defvar beeminder-tomorrow-code "tom"
  "The abbreviation for \"tomorrow\".")

(defcustom beeminder-when-the-day-ends (* 6 60 60)
  "Number of seconds from midnight when the day is assumed to end.
Times up to this time will be considered to belong to the
previous day.  Note: this should be positive, or weird things
might happen."
  :type 'integer
  :group 'beeminder)

(defun beeminder-plural-ending (number)
  "Return \"s\" if NUMBER not equal to one, and \"\" otherwise."
  (if (= number 1) "" "s"))

(defun beeminder-time-to-days (time)
  "Compute the number of days from 0001-12-31 BC until TIME.
Take into consideration `beeminder-when-the-day-ends'."
  (time-to-days (time-add time (- beeminder-when-the-day-ends))))

(defun beeminder-human-time (time)
  "Convert (future) TIME to a human-friendly format.
- For today, the time.
- For tomorrow, the string `beeminder-tomorrow-code' (by default) and
  the time.
- For times within a week, abbreviation of the weekday or a plus and
  a number of days (depending on `beeminder-human-time-use-weekday')
  and the time.
- For later times, iso date without time.
Midnight is treated as belonging to the previous day, not the following one."
  (let ((delta (- (beeminder-time-to-days time)
		  (beeminder-time-to-days (beeminder-current-time)))))
    (cond ((zerop delta) (format-time-string "     %R" time))
	  ((= 1 delta) (concat " " beeminder-tomorrow-code
			       (format-time-string " %R" time)))
	  ((<= delta 7) (concat (if beeminder-human-time-use-weekday
				    (format-time-string " %a"
							(time-add time
								  (- beeminder-when-the-day-ends)))
				  (format "  +%d" delta))
				" "
				(format-time-string "%R" time)))
	  (t (format-time-string "%Y-%m-%d" (time-add time
						      (- beeminder-when-the-day-ends)))))))

(defconst beeminder-lanes-to-faces-alist
  '((-2 . beeminder-red) (-1 . beeminder-yellow) (1 . beeminder-blue) (2 . beeminder-green))
  "Alist mapping the (normalized) value of lane to goal colors.")

(defun beeminder-normalize-lane (lane-number)
  "Normalize LANE-NUMBER into the interval -2 .. 2.
This means to return 2 for LANE-NUMBER greater than 2 and -2 for
LANE-NUMBER less than -2."
  (min (max lane-number -2) 2))

(defun beeminder-display-string-field (goal field &optional width invisible)
  "Return GOAL's FIELD (which should be a symbol) as a string.
Optionally use length WIDTH (padded from the right with spaces).
Make it invisible if INVISIBLE is non-nil."
  (let ((text (format (if width
			  (format "%%-%d.%ds" width width)
			"%s")
		      (cdr (assoc field goal)))))
    (if invisible
	(propertize text 'invisible invisible)
      text)))

(defun beeminder-display-losedate-human (goal)
  "Return the losedate field of GOAL in human-friendly format."
  (beeminder-human-time (seconds-to-time (1+ (cdr (assoc 'losedate goal))))))

(defun beeminder-get-rate (goal)
  "Return the rate of GOAL."
  (elt (cdr (assoc 'mathishard goal)) 2))

(defun beeminder-display-rate (goal)
  "Return the rate of the GOAL (with units), as a string."
  (let ((rate (beeminder-get-rate goal)))
    (format (concat
	     (number-to-human-string rate 4)
	     "/%s")
	    (cdr (assoc 'runits goal)))))

(defun beeminder-display-pledge (goal)
  "Return the pledge of the GOAL, as a string."
  (format "$%.2f" (cdr (assoc 'pledge goal))))

(defcustom beeminder-goal-pp-format
  '((beeminder-display-string-field slug 12)
    " "
    beeminder-display-losedate-human
    " "
    (beeminder-display-string-field limsum 16)
    " "
    beeminder-display-rate
    " "
    beeminder-display-pledge
    " "
    (beeminder-display-string-field title))
  "The format for displaying a Beeminder goal.
It is a list whose elements are either strings, printed verbatim,
either functions, which are then called with one argument (the
goal), or lists, in which case the car of the list is a function
and the cdr the list of arguments it should get after the goal."
  :type 'sexp
  :group 'beeminder)

(defun beeminder-goal-face (goal)
  "Return the face for displaying GOAL."
  (if (alist-get (intern (cdr (assoc 'slug goal)))
		 beeminder-dirty-alist)
      'beeminder-dirty
    (cdr (assoc (* (cdr (assoc 'yaw goal))
		   (beeminder-normalize-lane (cdr (assoc 'lane goal))))
		beeminder-lanes-to-faces-alist))))

(defun beeminder-goal-representation (goal)
  "The string representation of GOAL, with the face applied."
  (propertize (mapconcat (lambda (field-specifier)
			   (cond
			    ((functionp field-specifier) (funcall field-specifier goal))
			    ((consp field-specifier) (apply (car field-specifier)
							    goal
							    (cdr field-specifier)))
			    ((stringp field-specifier) field-specifier)))
		      beeminder-goal-pp-format "")
	      'face
	      (beeminder-goal-face goal)))

(defun beeminder-display-first-goal ()
  "Display the first goal in the echo area.
Normally, this should be one of the goals with the nearest
deadline.  Caution: if more than one goal has the same deadline,
it is not obvious which one is returned as the first from the
server!  You might want to bind this function globally so that
you don't need to enter the Beeminder mode to see the nearest
deadline."
  (interactive)
  (let* ((goal (car beeminder-goals))
	 (minutes (/ (- (cdr (assoc 'losedate goal))
			(time-to-seconds (beeminder-current-time)))
		     60)))
    (message "%s (%d minute%s left, %s to do)"
	     (replace-regexp-in-string
	      " \\{2,\\}"
	      "  "
	      (beeminder-goal-representation goal))
	     minutes
	     (beeminder-plural-ending minutes)
	     (let ((limsum (cdr (assoc 'limsum goal))))
	       (string-match "[[:digit:]]+\\(\\.[[:digit:]]*\\)?" limsum)
	       (match-string 0 limsum)))))


;; Faces for goals

(defface beeminder-green '((t :foreground "#080"))
  "Face for displaying Beeminder goals in green."
  :group 'beeminder-faces)

(defface beeminder-blue '((t :foreground "#008"))
  "Face for displaying Beeminder goals in blue."
  :group 'beeminder-faces)

(defface beeminder-yellow '((t :foreground "#880"))
  "Face for displaying Beeminder goals in green."
  :group 'beeminder-faces)

(defface beeminder-red '((t :foreground "#800"))
  "Face for displaying Beeminder goals in red."
  :group 'beeminder-faces)


;; Beeminder EWOC

(defvar beeminder-goals-ewoc nil)

(defvar beeminder-short-header nil
  "If t, the default header is (extremely) shortened.")

(defun beeminder-toggle-short-header (&optional arg)
  "Toggle shortening the header for Beeminder goal list.
If ARG is positive, shorten the header; otherwise, do not."
  (interactive "P")
  (setq beeminder-short-header
	(if (null arg)
	    (not beeminder-short-header)
	  (> (prefix-numeric-value arg) 0)))
  (save-current-goal
    (ewoc-set-hf beeminder-goals-ewoc (beeminder-ewoc-header) "")
    (ewoc-refresh beeminder-goals-ewoc)))

(define-key beeminder-mode-map (kbd "=") #'beeminder-toggle-short-header)

(defun beeminder-print-filter (filter)
  "Return a printed representation of FILTER.
It should be an element of `beeminder-current-filters'."
  (let ((filter-pp (nth (if beeminder-short-header 4 3)
			(assoc (car filter) beeminder-filters))))
    (cond
     ((stringp filter-pp) (format filter-pp (cdr filter)))
     ((functionp filter-pp) (funcall filter-pp (cdr filter)))
     (t error "Wrong format of `beeminder-filters' for filter %s" (car filter)))))

(defun beeminder-ewoc-header ()
  "Generate header for the Beeminder EWOC."
  (concat (format "Beeminder goals for user %s"
		  beeminder-username)
	  (propertize (concat (format (if beeminder-short-header
					  "  sort:%s"
					"\nsorting criterion: %s")
				      (caddr beeminder-current-sorting-setting))
			      (format (if beeminder-short-header
					  "  fil:%s"
					(format "\nfilter%s: %%s\n"
						(beeminder-plural-ending
						 (length beeminder-current-filters))))
				      (if beeminder-current-filters
					  (mapconcat #'beeminder-print-filter
						     beeminder-current-filters
						     ", ")
					"none")))
		      'face 'shadow)))

(defun beeminder-create-goals-ewoc ()
  "Return a newly created EWOC for Beeminder goals."
  (ewoc-create (lambda (goal) (insert (beeminder-goal-representation goal)))
	       (beeminder-ewoc-header)""))

(defun beeminder-recreate-ewoc ()
  "Recreate Beeminder EWOC from the goal list.
In particular, reapply filtering and sorting settings.  Note: since
only the last sorting criterion is remembered, and sorting is stable,
this might actually change the ordering of goals, which may have been
sorted by another criterion previously."
  (ewoc-filter beeminder-goals-ewoc #'ignore)
  (mapcar (lambda (goal)
	    (ewoc-enter-last beeminder-goals-ewoc goal))
	  beeminder-goals)
  (beeminder-apply-filters)
  (apply #'beeminder-sort-by-field beeminder-current-sorting-setting)
  (ewoc-set-hf beeminder-goals-ewoc (beeminder-ewoc-header) "")
  (ewoc-refresh beeminder-goals-ewoc)
  (with-current-buffer (ewoc-buffer beeminder-goals-ewoc)
    (goto-char (point-min))))

(defun beeminder-list-goals ()
  "Switch to a buffer containing the list of Beeminder goals."
  (interactive)
  (switch-to-buffer "*Beeminder goals*")
  (buffer-disable-undo)
  (unless beeminder-goals
    (message "Beeminder goals downloading...")
    (beeminder-get-goals)
    (message "Beeminder goals downloading...done"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq beeminder-goals-ewoc (beeminder-create-goals-ewoc))
    (beeminder-recreate-ewoc))
  (beeminder-mode)
  (setq truncate-lines t))


;; Current time function

(defalias 'beeminder-current-time 'current-time
  "An alias for current-time, useful for testing/debugging.")


;; Sorting

(defcustom beeminder-default-sorting-setting (list 'losedate #'< "losedate")
  "Default sorting setting for Beeminder goals.
This is a list whose first element is the field according to
which the sorting should be done, then the predicate, and then
the printed representation of this sorting method (as a string)."
  :type 'sexp
  :group 'beeminder)

(defvar beeminder-current-sorting-setting beeminder-default-sorting-setting)

(defmacro save-current-goal (&rest body)
  "Evaluate BODY and bring the point back to the current goal."
  (declare (indent 0) (debug t))
  `(let ((current-goal-slug
	  (if (beeminder-before-first-goal-p)
	      nil
	    (beeminder-get-slug (ewoc-data (ewoc-locate beeminder-goals-ewoc))))))
     ,@body
     (if (not current-goal-slug)
	 (goto-char (point-min))
       (ewoc-goto-node beeminder-goals-ewoc (ewoc-nth beeminder-goals-ewoc 0))
       (let ((current-node (ewoc-nth beeminder-goals-ewoc 0)))
	 (while (and current-node
		     (not (eq (beeminder-get-slug (ewoc-data current-node))
			      current-goal-slug)))
	   (ewoc-goto-next beeminder-goals-ewoc 1)
	   (setq current-node (ewoc-next beeminder-goals-ewoc current-node)))
	 (unless current-node (goto-char (point-min)))))))

(defun beeminder-sort-by-field (field predicate info)
  "Sort entries in `beeminder-goals-ewoc' by FIELD, using PREDICATE.
INFO is the printed representation of the sorting criterion."
  (save-current-goal
    (ewoc-sort beeminder-goals-ewoc (lambda (x y)
				      (funcall predicate
					       (cdr (assoc field x))
					       (cdr (assoc field y)))))
    (ewoc-refresh beeminder-goals-ewoc)
    (setq beeminder-current-sorting-setting (list field predicate info))
    (ewoc-set-hf beeminder-goals-ewoc (beeminder-ewoc-header) "")))

(defun beeminder-sort-by-losedate ()
  "Sort entries in `beeminder-goals' by losedate."
  (interactive)
  (beeminder-sort-by-field 'losedate #'< "losedate"))

(defun beeminder-seconds-to-from-midnight (time)
  "Convert TIME to seconds from midnight.
If after 6:00, convert to seconds to midnight (with a minus
sign).  The magic time constant 6:00 is the result of Beeminder's way
of dealing with the \"midnight\" setting."
  (let* ((decoded-time (decode-time time))
	 (seconds (+ (car decoded-time)
		     (* 60 (cadr decoded-time))
		     (* 3600 (caddr decoded-time)))))
    (if (> seconds (* 6 60 60))
	(- seconds (* 24 60 60))
      seconds)))

(defun beeminder-earlier-midnight (sec1 sec2 time)
  "Compare SEC1 and SEC2, taking into account the TIME.
All three parameters are expressed as seconds from midnight, like
the result of calling `beeminder-seconds-to-from-midnight'.  If
SEC1 < SEC2 < TIME, return t.  If TIME < SEC1 < SEC2, return t.
If SEC2 < TIME < SEC1, return t.  In all other cases, return nil.
This function is useful for sorting goals by their \"midnight\"
setting, with the goals which are after their \"midnight\" at the
end."
  (or (< sec1 sec2 time)
      (< time sec1 sec2)
      (< sec2 time sec1)))

(defun beeminder-sort-by-midnight ()
  "Sort entries in `beeminder-goals' by their midnight, taking current time into consideration."
  (interactive)
  (beeminder-sort-by-field
   'deadline
   (lambda (x y)
     (beeminder-earlier-midnight
      x y (beeminder-seconds-to-from-midnight
	   (beeminder-current-time))))
    "midnight"))

(define-key beeminder-mode-map "l" #'beeminder-sort-by-losedate)
(define-key beeminder-mode-map "m" #'beeminder-sort-by-midnight)


;; refreshing view and reloading goals
(defun beeminder-refresh-goals-list ()
  "Refresh the goals list."
  (interactive)
  (save-current-goal
    (beeminder-recreate-ewoc)))

(defun beeminder-reload-goals-list ()
  "Reload the goals from the server."
  (interactive)
  (message "Beeminder goals reloading...")
  (beeminder-get-goals)
  (message "Beeminder goals reloading...done")
  (beeminder-refresh-goals-list))

(define-key beeminder-mode-map (kbd "C-l") #'beeminder-refresh-goals-list)
(define-key beeminder-mode-map "g" #'beeminder-reload-goals-list)


;; Filtering goals

(defvar beeminder-current-filters '()
  "Alist of filters currently in effect.")

(defcustom beeminder-saved-filters '()
  "A remembered set of filters for fast retrieval.")

(defun beeminder-save-filters ()
  "Save the current filters."
  (interactive)
  (setq beeminder-saved-filters
	beeminder-current-filters)
  (message "Current filter setting saved."))

(defun beeminder-retrieve-filters ()
  "Retrieve saved filters."
  (interactive)
  (setq beeminder-current-filters
	beeminder-saved-filters)
  (beeminder-refresh-goals-list)
  (message "Filter settings retrieved."))

(defun beeminder-clear-filters ()
  "Clear all filters.
If there are no saved filters, first save the current filters."
  (interactive)
  (unless beeminder-saved-filters
    (beeminder-save-filters))
  (setq beeminder-current-filters '())
  (beeminder-refresh-goals-list))

(define-key beeminder-mode-map "c" #'beeminder-clear-filters)

(define-prefix-command 'beeminder-filter-map)
(define-key beeminder-mode-map "f" #'beeminder-filter-map)
(define-key beeminder-filter-map "c" #'beeminder-clear-filters)
(define-key beeminder-filter-map "s" #'beeminder-save-filters)
(define-key beeminder-filter-map "f" #'beeminder-retrieve-filters)
(define-key beeminder-filter-map "r" #'beeminder-retrieve-filters)

(defcustom beeminder-default-filter-days 3
  "Defalt number of days used for filtering by losedate.
If the user doesn't specify the number of days for filtering, all
goals with more than this amount of days left to losedate will be
filtered out."
  :type 'integer
  :group 'beeminder)

(defcustom beeminder-use-goal-midnight-today-values nil
  "If non-nil, compute today's values using the goal's midnight.
If nil, use the global midnight defined by
`beeminder-when-the-day-ends'."
  :type 'boolean
  :group 'beeminder)

(defcustom beeminder-default-filter-donetoday 100
  "Default percentage of donetoday used for filtering."
  :type 'integer
  :group 'beeminder)

(defcustom beeminder-show-dirty-donetoday t
  "If non-nil, show dirty goals even if they would be normally
  filtered out by the \"donetoday\" filter."
  :type 'boolean
  :group 'beeminder)

(defun beeminder-days-p (goal days)
  "Return nil if time to derailment of GOAL > DAYS."
  (<= (- (beeminder-time-to-days (cdr (assoc 'losedate goal)))
	 (beeminder-time-to-days (beeminder-current-time)))
      days))

(defun beeminder-donetoday-p (goal percentage)
  "Return nil if donetoday for GOAL >= PERCENTAGE * day's amount.
Take the variable `beeminder-show-dirty-donetoday' into account."
  (if (and beeminder-show-dirty-donetoday
	   (alist-get (beeminder-get-slug goal) beeminder-dirty-alist))
      t
    (let* ((rate (beeminder-get-rate goal))
	   (daily-rate (/ rate
			  (cl-case (intern (cdr (assoc 'runits goal)))
			    (y 365)
			    (m (/ 365.0 12))
			    (w 7)
			    (d 1)
			    (h (/ 1 24.0))))))
      (when (> rate 0)
	(< (* 100 (cdr (assoc 'donetoday goal)))
	   (* percentage daily-rate))))))

(defun beeminder-not-killed-p (goal kill-list)
  "Return nil if GOAL is in the KILL-LIST."
  (not (member (beeminder-get-slug goal) kill-list)))

(defvar beeminder-filters `((losedate ,#'beeminder-days-p
				  ,beeminder-default-filter-days
				  "days to derailment (%d)"
				  "d2d(%d)")
			    (donetoday ,#'beeminder-donetoday-p
				       ,beeminder-default-filter-donetoday
				       "done today (%d%%)"
				       "dt(%d%%)")
			    (killed ,#'beeminder-not-killed-p
				    '()
				    (lambda (kill-list)
				      (format "%d goal%s killed"
					      (length kill-list)
					      (beeminder-plural-ending (length kill-list))))
				    (lambda (kill-list)
				      (format "%d gk" (length kill-list)))))

  "List of possible filters.  Each element is a list, consisting of:
- symbol, denoting the filter,
- predicate (with two arguments - the goal and the parameter),
- default value for the parameter,
- formatting function (with one argument - the parameter) or a format
  string (with one placeholder)
- formatting function or a format string for the shortened version of
  header.")

(defun beeminder-kill-goal (gnode)
  "Delete GNODE from `beeminder-goals-ewoc'."
  (interactive (list (beeminder-slug-to-gnode (intern (cdr (assoc 'slug (current-or-read-goal)))))))
  (if gnode
      (let ((inhibit-read-only t)
	    (next-goal (or (ewoc-next beeminder-goals-ewoc gnode)
			   (ewoc-prev beeminder-goals-ewoc gnode))))
	(ewoc-delete beeminder-goals-ewoc gnode)
	(ewoc-refresh beeminder-goals-ewoc)
	(push (beeminder-get-slug (ewoc-data gnode))
	      (alist-get 'killed beeminder-current-filters))
	(ewoc-set-hf beeminder-goals-ewoc (beeminder-ewoc-header) "")
	(if next-goal
	    (ewoc-goto-node beeminder-goals-ewoc next-goal)
	  (goto-char (point-min)))
	(message "Goal %s killed (hidden from view)." (cdr (assoc 'slug (ewoc-data gnode)))))
    (message "Goal already killed.")))

(define-key beeminder-mode-map (kbd "C-k") #'beeminder-kill-goal)
(define-key beeminder-filter-map "k" #'beeminder-kill-goal)

(defun beeminder-show-kills ()
  "Show all killed goals."
  (interactive)
  (message "Killed goals: %s."
	   (aif (alist-get 'killed beeminder-current-filters)
	       (mapconcat #'symbol-name it ", ")
	     "none")))

(defun beeminder-clear-kills ()
  "Unkill all killed goals."
  (interactive)
  (setq beeminder-current-filters
	(assq-delete-all 'killed beeminder-current-filters))
  (beeminder-recreate-ewoc))

(defun beeminder-clear-or-show-kills (arg)
  "Unkill all killed goals if ARG is nil.
With prefix argument, show the list of killed goals."
  (interactive "P")
  (if arg
      (beeminder-show-kills)
    (beeminder-clear-kills)))

(define-key beeminder-mode-map (kbd "C-y") #'beeminder-clear-or-show-kills)
(define-key beeminder-filter-map "y" #'beeminder-clear-or-show-kills)

(defun beeminder-apply-filter (filter)
  "Apply FILTER (a dotted pair of symbol and parameter).
This means deleting some goals from `beeminder-goals-ewoc'."
  (save-current-goal
    (ewoc-filter beeminder-goals-ewoc
		 (lambda (goal)
		   (funcall (cadr (assoc (car filter) beeminder-filters))
			    goal (cdr filter))))))

(defun beeminder-apply-filters ()
  "Apply filters from `beeminder-current-filters' in sequence."
  (mapc #'beeminder-apply-filter beeminder-current-filters))

(defun beeminder-enable-filter (filter parameter)
  "Enable FILTER (symbol) with PARAMETER (number).
Disable FILTER if PARAMETER is nil."
  (setf (alist-get filter beeminder-current-filters)
	parameter)
  (setq beeminder-current-filters (rassq-delete-all nil beeminder-current-filters))
  (save-current-goal (beeminder-recreate-ewoc)))

(defun beeminder-filter-parameter (raw-prefix default)
  "Return filter parameter based on RAW-PREFIX and DEFAULT."
  (cond ((eq raw-prefix '-) nil)
	((null raw-prefix) default)
	(t (prefix-numeric-value raw-prefix))))

(defun beeminder-filter-by-losedate (&optional days)
  "Filter out goals with time to losedate greater than DAYS."
  (interactive "P")
  (beeminder-enable-filter 'losedate
			   (beeminder-filter-parameter days
						       beeminder-default-filter-days)))

(defun beeminder-filter-by-donetoday (&optional percentage)
  "Filter out goals with donetoday greater than PERCENTAGE."
  (interactive "P")
  (beeminder-enable-filter 'donetoday
			   (beeminder-filter-parameter percentage
						       beeminder-default-filter-donetoday)))

(define-key beeminder-mode-map (kbd "d") #'beeminder-filter-by-losedate)
(define-key beeminder-filter-map (kbd "d") #'beeminder-filter-by-losedate)
(define-key beeminder-mode-map (kbd "t") #'beeminder-filter-by-donetoday)
(define-key beeminder-filter-map (kbd "t") #'beeminder-filter-by-donetoday)


;; Displaying goal details

(defcustom beeminder-goal-template-fields-alist
  '((backburnerp . (if (string= (cdr (assoc 'burner goal)) "backburner") "(backburner)" ""))
    (username . beeminder-username)
    (dirtyp . (if (assoc (beeminder-get-slug goal) beeminder-dirty-alist)
		  (propertize " (goal dirty!)" 'face 'beeminder-dirty) ""))
    (target . (highlight-subtly (number-to-string (elt (cdr (assoc 'mathishard goal)) 1))))
    (goaldate . (highlight-subtly (format-time-string "%x" (seconds-to-time (elt (cdr (assoc 'mathishard goal)) 0)))))
    (rate . (highlight-subtly (number-to-human-string (beeminder-get-rate goal))))
    (runit . (highlight-subtly
	      (cl-case (intern (cdr (assoc 'runits goal)))
		(d "day")
		(w "week")
		(m "month")
		(h "hour")
		(y "year"))))
    (curval . (highlight-subtly (number-to-human-string (cdr (assoc 'curval goal)))))
    (autodatap . (aif (cdr (assoc 'autodata goal))
		     (concat ", autodata source: "
			     (highlight-subtly it))
		   ""))
    (goaltype . (highlight-subtly (beeminder-display-goal-type (cdr (assoc 'goal_type goal)))))
    (losedate . (highlight-subtly (trim-leading-whitespace
				   (beeminder-display-losedate-human goal))))
    (pledge . (highlight-subtly (beeminder-display-pledge goal)))
    (midnight . (highlight-subtly (beeminder-display-midnight-setting (cdr (assoc 'deadline goal)))))
    (donetoday . (highlight-subtly (number-to-human-string (cdr (assoc 'donetoday goal)))))
    (datapoints . (propertize (beeminder-format-datapoints goal) 'face 'shadow)))
  "Alist of symbols and corresponding pieces of code to evaluate
and insert the result in the goal details info.")

(defun beeminder-display-time-field (alist field)
  "Return ALIST's (unix-time) FIELD formatted."
  (format-time-string "%x %X" (time-to-seconds (cdr (assoc field alist)))))

(defcustom beeminder-datapoint-format
  '((beeminder-display-string-field id 25 t)
    (beeminder-display-time-field timestamp)
    "  "
    (beeminder-display-string-field value 4)
    "  "
    (beeminder-display-string-field comment))
  "The format for displaying a goal's datapoint.
The format is identical to that of `beeminder-goal-pp-format'.")

(defun beeminder-datapoint-representation (datapoint)
  "The string representation of DATAPOINT."
  ;; TODO: factor out common code of this and `beeminder-goal-pp-format'.
  (mapconcat (lambda (field-specifier)
	       (cond
		((functionp field-specifier) (funcall field-specifier datapoint))
		((consp field-specifier) (apply (car field-specifier)
						datapoint
						(cdr field-specifier)))
		((stringp field-specifier) field-specifier)))
	     beeminder-datapoint-format ""))

(defun beeminder-format-datapoints (goal)
  "Return the printed representation of GOAL's datapoints."
  (mapconcat #'beeminder-datapoint-representation
	     (reverse (cdr (assoc 'datapoints goal)))
	     "\n"))

(defun beeminder-insert-goal-template-with-expansion (template goal)
  "Insert TEMPLATE with information about GOAL.
If a substring of the form \"#SYMBOL\" is found, and SYMBOL is
a key in the `beeminder-goal-template-fields-alist' variable,
\"#SYMBOL\" is replaced with result of evaluating the associated
value.  If there is no such entry, the SYMBOL is looked up in the
alist representing the current goal, and \"#SYMBOL\" is replaced
with its printed representation (using `format''s \"%s\"
specifier).  In the latter case, the result is colorized with the
`subtle-highlight-face'; in the former case, code in
`beeminder-goal-template-fields-alist' should take care of
colorization if needed.

If a substring of the form \"#SEXP\" is found, and SEXP is not
a symbol, \"#SEXP\" is replaced with the result of evaluating SEXP.
Within SEXP, the variable `goal' is bound to the alist holding the
current's goal properties.

Should someone want to insert a literal \"#\" character, the form
\"#(identity \"#\")\" can be used.

Warning: this function uses `eval', so evil code in TEMPLATE or
`beeminder-goal-template-fields-alist' can do real harm!"
  (save-excursion (insert template))
  (while (search-forward "#" nil t)
    (let ((begin (1- (point)))
	  (sexp (read (current-buffer))))
      (delete-region begin (point))
      (insert (format "%s"
		      (cond ((symbolp sexp)
			     (aif (assoc sexp beeminder-goal-template-fields-alist)
				 (eval (cdr it))
			       (highlight-subtly (cdr (assoc sexp goal)))))
			    (t (eval sexp))))))))

(define-derived-mode beeminder-goal-mode special-mode "Beeminder goal"
  "A major mode for a buffer displaying details of a Beeminder goal,
in particular the history of datapoints.")

(define-key beeminder-goal-mode-map (kbd "TAB") #'quit-window)

(defun beeminder-next-datapoint (count)
  "Move forward COUNT datapoints."
  (interactive "p")
  (forward-line 1)
  (re-search-forward "^[0-9a-f]\\{24\\}" nil t count)
  (beginning-of-line))

(defun beeminder-previous-datapoint (count)
  "Move forward COUNT datapoints."
  (interactive "p")
  (beginning-of-line)
  (re-search-backward "^[0-9a-f]\\{24\\}" nil t count))

(define-key beeminder-goal-mode-map (kbd "n") #'beeminder-next-datapoint)
(define-key beeminder-goal-mode-map (kbd "p") #'beeminder-previous-datapoint)

(defface subtle-highlight '((t :foreground "#006600"))
  "Face for subtly highlighting things.")

(defun highlight-subtly (string)
  "Make STRING stand out, but only a little."
  (propertize string 'face 'subtle-highlight))

(defun number-to-human-string (number &optional width)
  "Convert NUMBER to a human-friendly form, at least WIDTH characters.
If NUMBER is greater than 10, use one decimal place.  Otherwise,
use two.  Trim any non-significant trailing zeros."
  (let ((str (replace-regexp-in-string
	      "\\.$" "" (replace-regexp-in-string
			 "0+$" ""
			 (format (cond
				  ((> number 10) "%.1f")
				  (t "%.2f"))
				 number)))))
    (if width (format (format "%%%ds" width) str) str)))

(defun beeminder-display-midnight-setting (seconds)
  "Convert SECONDS to or from midnight to a time string."
  (when (< seconds 0) (setq seconds (+ seconds (* 24 60 60))))
  (let* ((hours (/ seconds 60 60))
	 (minutes (/ (- seconds (* hours 60 60)) 60)))
    (format "%d:%02d" hours minutes)))

(defun beeminder-display-goal-type (goaltype)
  "Convert GOALTYPE to a printed representation."
  (cl-case (intern goaltype)
    (hustler "do more")
    (biker "odometer")
    (fatloser "weight loss")
    (gainer "gain weight")
    (inboxer "inbox fewer")
    (drinker "do less")
    (custom "custom")))

(defcustom beeminder-goal-template
  "Details for Beeminder goal #slug for user #username#backburnerp
#title#dirtyp
goal target: #target on #goaldate at rate #rate per #runit (currently at #curval, done today: #donetoday)
goal type: #goaltype#autodatap
safe until #losedate (current pledge: #pledge, left to do: #limsum, midnight setting: #midnight)

Recent datapoints (#(symbol-value 'beeminder-history-length) days):
#datapoints
"
  "The default template for displaying goal details.
See the docstring of the function
`beeminder-insert-goal-template-with-expansion' for the list of
available keywords.")

(defun beeminder-refresh-goal-details ()
  "Refresh goal details, assuming that the respective buffer
exists and is set up properly."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (beeminder-insert-goal-template-with-expansion
     beeminder-goal-template
     beeminder-detailed-goal))
  (goto-char (point-min)))

(defun beeminder-display-goal-details (goal)
  "Display details about GOAL in a temporary buffer."
  (interactive (list (current-or-read-goal)))
  (pop-to-buffer "*Beeminder goal details*")
  (beeminder-goal-mode)
  (setq-local beeminder-detailed-goal goal)
  (beeminder-refresh-goal-details))

(define-key beeminder-mode-map (kbd "TAB") #'beeminder-display-goal-details)

(defcustom beeminder-confirm-datapoint-deletion #'y-or-n-p
  "How to ask for confirmation of datapoint deletion.
If nil, don't ask."
  :type '(choice (const :tag "Ask with yes-or-no-p" yes-or-no-p)
		 (const :tag "Ask with y-or-n-p" y-or-n-p)
		 (const :tag "Don't ask at all" nil)
		 (funtion :tag "Predicate function"))
  :group 'beeminder)

(defun beeminder-get-datapoint-id ()
  "Return the id of the datapoint at point."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[0-9a-f]\\{24\\}")
	(match-string-no-properties 0)
      (error "Not at a datapoint"))))

(defun beeminder-delete-datapoint (id)
  "Delete datapoint with id ID.
If called interactively, take the id from the beginning of the
line."
  (interactive (list (beeminder-get-datapoint-id)))
  (if (and beeminder-confirm-datapoint-deletion
	   (funcall beeminder-confirm-datapoint-deletion "Are you sure you want to delete this datapoint?"))
      (cond ((beeminder-request-delete
	      (concat "/goals/" (cdr (assoc 'slug beeminder-detailed-goal)) "/datapoints/" id ".json"))
	     (let ((datapoints (cdr (assoc 'datapoints beeminder-detailed-goal))))
	       (cl-decf (alist-get 'donetoday beeminder-detailed-goal)
			(cdr (assoc 'value (cl-find id datapoints
						    :key (lambda (dp)
							   (cdr (assoc 'id dp)))
						    :test #'string=))))
	       (cl-delete id datapoints :key (lambda (dp) (cdr (assoc 'id dp))) :test #'string=)
	       (beeminder-make-goal-dirty (beeminder-get-slug beeminder-detailed-goal))
	       (beeminder-refresh-goal-details)
	       (message "Datapoint succesfully deleted.")))
	    (t
	     (sit-for beeminder-default-timeout)
	     (error "I had a problem deleting the datapoint.")))))

(define-key beeminder-goal-mode-map (kbd "d") #'beeminder-delete-datapoint)

(defun beeminder-get-datapoint (id datapoints)
  "Return datapoint with id ID from DATAPOINTS."
  (cl-find id datapoints
	   :key (lambda (dp)
		  (cdr (assoc 'id dp)))
	   :test #'string=))

(defun beeminder-edit-datapoint (id)
  "Edit datapoint with id ID.
If called interactively, take the id from the beginning of the
line."
  (interactive (list (beeminder-get-datapoint-id)))
  (let* ((datapoints (reverse (cdr (assoc 'datapoints beeminder-detailed-goal))))
	 (datapoint (aif (beeminder-get-datapoint id datapoints)
			it
		      (error "%s" "Invalid datapoint id -- beeminder-get-datapoint")))
	 (timestamp (ask-for-timestamp (cdr (assoc 'timestamp datapoint))))
	 (value (string-to-number
		 (let ((default-value (cdr (assoc 'value datapoint))))
		   (read-string (format "Value (default %s): " default-value)
				nil nil (number-to-string default-value)))))
	 (comment (let ((comment-history
			 (mapcar (lambda (dp)
				   (cdr (assoc 'comment dp)))
				 datapoints)))
		    (read-string "Comment: "
				 (cdr (assoc 'comment datapoint))
				 (cons 'comment-history
				       (1+ (cl-position
					    id
					    datapoints
					    :key (lambda (dp)
						   (cdr (assoc 'id dp)))
					    :test #'string=)))
				 nil t))))
    (message "Submitting new datapoint...")
    (if (beeminder-request-put (format "/goals/%s/datapoints/%s.json"
				       (beeminder-get-slug beeminder-detailed-goal)
				       id)
			       (format "value=%f&comment=%s&timestamp=%d"
				       value comment timestamp))
	(message "Submitting new datapoint...done")
      (sit-for beeminder-default-timeout)
      (error "Datapoint submitting failed, check your internet connection"))))

(define-key beeminder-goal-mode-map (kbd "e") #'beeminder-edit-datapoint)


;; Org-mode integration

(defcustom beeminder-org-inherit-beeminder-properties nil
  "Make beeminder.el use property inheritance.")

(defun beeminder-org-submit-on-done (state-change)
  "Submit a datapoint when marking an item as DONE.
This function should be placed in `org-trigger-hook'.  It looks
up the following properties of the headline: the \"beeminder\"
property (which should be set to \"done\", the \"slug\"
property (which should be set to the slug of the goal), the
\"amount\" property (defaults to 1), the \"ask-comment\"
property (asks for the comment if it is present)."
  (let ((position (plist-get state-change :position)))
    (if (and (string= (downcase (or (org-entry-get position
						   "beeminder"
						   beeminder-org-inherit-beeminder-properties)
				    ""))
		      "done")
	     (eq (plist-get state-change :type)
		 'todo-state-change)
	     (member (plist-get state-change :to)
		     org-done-keywords)
	     (org-entry-get position "slug" beeminder-org-inherit-beeminder-properties))
	(let* ((slug-str (org-entry-get position
					"slug"
					beeminder-org-inherit-beeminder-properties))
	       (amount (aif (org-entry-get position
					   "amount"
					   beeminder-org-inherit-beeminder-properties)
			   (string-to-number it)
			 1))
	       (comment (unless (org-entry-get position
					       "ask-comment"
					       beeminder-org-inherit-beeminder-properties)
			  (concat "via Org-mode at " (beeminder-current-time-hmsz-string)))))
	  (beeminder-submit-datapoint slug-str amount comment nil t)))))

(defun beeminder-org-submit-clock-at-point ()
  "Submit the data from the clock item at point to Beeminder.
This is mainly useful if submitting on clocking out (see
`beeminder-org-submit-on-clock-out' failed for some reason, so
that the user may want to submit clock items later."
  (interactive)
  (let ((duration (org-element-property :duration (org-element-at-point))))
    (when (string-match "\\([[:digit:]]+\\):\\([[:digit:]]\\{2\\}\\)" duration)
      (let ((minutes (+ (* 60 (string-to-number (match-string 1 duration)))
			(string-to-number (match-string 2 duration))))
	    (slug-str (org-entry-get (point)
				     "slug"
				     beeminder-org-inherit-beeminder-properties))
	    (comment (unless (org-entry-get (point)
					    "ask-comment"
					    beeminder-org-inherit-beeminder-properties)
		       (concat "via Org-mode at " (beeminder-current-time-hmsz-string))))
	    (multiplier (cl-case (intern (or (org-entry-get (point)
							    "unit"
							    beeminder-org-inherit-beeminder-properties)
					     ""))
			  ((hour hours)
			   (/ 1 60.0))
			  ((hail-Mary hail-Marys)
			   3)
					; 1 hail-Mary ≈ 20 seconds
			  (t 1))))
	(save-excursion
	  (beeminder-submit-datapoint slug-str (* minutes multiplier)
				      comment nil t))))))

(defun beeminder-org-submit-on-clock-out ()
  "Submit the time clocked for this item.
This function should be placed in `org-clock-out-hook'.  It looks
up the following properties of the headline: the \"beeminder\"
property (which should be set to \"clock\", the \"slug\"
property (which should be set to the slug of the goal), the
\"unit\" property (which may be \"minutes\", which is the
default, or \"hours\", etc.\"), the \"ask-comment\"
property (asks for the comment if it is present)."
  (when (and (string= (downcase (or (org-entry-get (point)
						   "beeminder"
						   beeminder-org-inherit-beeminder-properties)
				    ""))
		      "clock")
	     (not (string-match " LINE REMOVED$" (or (current-message) ""))) ; this is really hackish
	     (org-entry-get
	      (point) "slug" beeminder-org-inherit-beeminder-properties))
    (beeminder-org-submit-clock-at-point)))

(defun add-or-remove-hook (arg hook function &optional local message)
  "Call `add-hook' if ARG is positive, `remove-hook' otherwise.
Print MESSAGE and \"on\" or \"off\" if non-nil."
  (funcall (if (> arg 0) #'add-hook #'remove-hook) hook function local)
  (if message (message "%s %s" message (if (> arg 0) "on" "off"))))

(defun beeminder-org-done-submitting (arg)
  "Turn submitting on marking as DONE on.
With negative argument ARG, turn it off."
  (interactive "p")
  (add-or-remove-hook arg
		      'org-trigger-hook
		      #'beeminder-org-submit-on-done
		      nil
		      "Submitting goals on DONE"))

(defun beeminder-org-clocking-out-submitting (arg)
  "Turn submitting on clocking out on.
With negative argument ARG, turn it off."
  (interactive "p")
  (add-or-remove-hook arg
		      'org-clock-out-hook
		      #'beeminder-org-submit-on-clock-out
		      nil
		      "Submitting goals on clocking out"))


(provide 'beeminder)


;;; beeminder.el ends here
