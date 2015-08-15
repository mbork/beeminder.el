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
  "The vector of sexps representing goals.
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
	    :data data
	    :parser #'json-read
	    :sync t
	    :timeout (or timeout beeminder-default-timeout))))


;; API calls (currently synchronous only)

(defun last-midnight (goal-deadline now)
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

(defun beeminder-get-goals ()
  "Get all the user's Beeminder goals.
Return a vector of sexps, each describing one goal."
  (let ((goals (beeminder-request-get "/goals.json"))
	(today-values
	 (mapcar
	  (lambda (goal)
	    (let ((last-midnight (last-midnight (cdr (assoc 'deadline goal))
						(beeminder-current-time))))
	      (cons (cdr (assoc 'slug goal))
		    (cl-reduce #'+
			       (mapcar (lambda (datapoint)
					 (if (> (cdr (assoc 'timestamp datapoint))
						last-midnight)
					     (cdr (assoc 'value datapoint))
					   0))
				       (cdr (assoc 'datapoints goal)))))))
	  (cdr (assoc 'goals (beeminder-request-get
			     (format ".json?diff_since=%d"
				     (- (time-to-seconds
					 (current-time))
					(* 24 60 60)))))))))
    (setq beeminder-goals (mapcar
			   (lambda (goal) (cons
					   (cons 'donetoday
						 (cdr (assoc
						  (cdr (assoc 'slug goal))
						  today-values)))
					   goal))
			   goals))
    (mapc (lambda (goal)
	    (let ((slugsym (intern (cdr (assoc 'slug goal)))))
	      (if (and (cdr (assoc slugsym beeminder-dirty-alist))
		       (/= (cdr (assoc 'curval goal))
			   (cdr (assoc slugsym beeminder-dirty-alist))))
		  (setq beeminder-dirty-alist (assq-delete-all slugsym beeminder-dirty-alist)))))
	  goals)))

(defun beeminder-refresh-goal (slug)
  "Refresh autodata and graph of the goal named SLUG."
  (beeminder-request-get (concat "/goals/" slug "/refresh_graph.json")))


;; Submitting datapoints

(defun beeminder-before-first-goal-p ()
  "Return t if the point is before the first goal."
  (< (point)
     (ewoc-location (ewoc-nth beeminder-goals-ewoc 0))))

(defun beeminder-get-slug (goal)
  "Return the slug of GOAL."
  (cdr (assoc 'slug goal)))

(defun beeminder-slug-to-goal (slug)
  "Return the goal node corresponding to SLUG."
  (let ((goal-node (ewoc-nth beeminder-goals-ewoc 0)))
    (while (and goal-node
		(not
		 (string= slug (cdr (assoc 'slug (ewoc-data goal-node))))))
      (setq goal-node (ewoc-next beeminder-goals-ewoc goal-node)))
    goal-node))

(defvar beeminder-minibuffer-history nil
  "History of goal slugs entered through minibuffer.")

(defun current-or-read-goal ()
  "Return the goal node the point is on.
If the point is before the first goal, use `completing-read' to
ask for the goal slug and return that goal node instead."
  (if (beeminder-before-first-goal-p)
      (beeminder-slug-to-goal
       (completing-read "Goal slug: "
			(mapcar #'beeminder-get-slug (ewoc-collect beeminder-goals-ewoc #'true))
			nil
			t
			nil
			'beeminder-minibuffer-history
			(beeminder-get-slug (ewoc-data (ewoc-nth beeminder-goals-ewoc 0)))))
    (ewoc-locate beeminder-goals-ewoc)))

(defun current-time-hmsz-string (&optional timestamp)
  "Return TIMESTAMP (Unix time) as a string.
Use current time by default.  Format is hh:mm:ss tz."
  (let ((decoded-time (decode-time (or (seconds-to-time timestamp)
				       (current-time)))))
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

(defun beeminder-submit-datapoint (slug amount &optional comment timestamp print-message)
  "Submit a datapoint to Beeminder goal SLUG with AMOUNT.
Additional data are COMMENT and TIMESTAMP (as Unix time).  If
PRINT-MESSAGE is non-nil, print suitable messages in the echo
area."
  (interactive
   (let* ((slug (cdr (assoc 'slug (ewoc-data (current-or-read-goal)))))
	  (yesterdayp (eq current-prefix-arg '-))
	  (amount (if (numberp current-prefix-arg)
		      current-prefix-arg
		    (string-to-number (read-string
				       (format "Datapoint value for %s%s: "
					       slug
					       (if yesterdayp " (yesterday)" ""))
				       nil nil "1"))))
	  (current-timestamp (time-to-seconds (current-time)))
	  (default-comment (concat
			    "via Emacs at "
			    (current-time-hmsz-string current-timestamp))))
     (list slug
	   amount
	   (read-string
	    (format "Comment for amount %d for goal %s: " amount slug)
	    nil nil default-comment)
	   (or (if yesterdayp
		   (- current-timestamp (* 24 60 60)))
	       (if (consp current-prefix-arg)
		   (ask-for-timestamp))
	       current-timestamp)
	   t)))
  (if print-message (message (format "Submitting datapoint of %d for goal %s..." amount slug)))
  (unless (beeminder-request-post (format "/goals/%s/datapoints.json" slug)
				  (concat (format "auth_token=%s&value=%f&comment=%s&timestamp=%d"
						  beeminder-auth-token
						  amount
						  (or comment default-comment)
						  (or timestamp
						      current-timestamp))))
    (sit-for beeminder-default-timeout)
    (error "Submitting failed, check your internet connection"))
  (if print-message (message (format "Submitting datapoint of %d for goal %s...Done." amount slug)))
  (setf (alist-get (intern slug)
		   beeminder-dirty-alist)
	(cdr (assoc 'curval (ewoc-data (beeminder-slug-to-goal slug)))))
  (beeminder-recreate-ewoc))

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
previous day."
  :type 'integer
  :group 'beeminder)

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

(defun beeminder-display-string-field (goal field &optional width)
  "Display GOAL's FIELD (which should be a symbol) as a string.
Optionally use length WIDTH (padded from the right with spaces)."
  (format (if width
	      (format "%%-%d.%ds" width width)
	    "%s")
	  (cdr (assoc field goal))))

(defun beeminder-display-losedate-human (goal)
  "Return the losedate field of GOAL in human-friendly format."
  (beeminder-human-time (seconds-to-time (1+ (cdr (assoc 'losedate goal))))))

(defun beeminder-display-rate (goal)
  "Return the rate of the GOAL (with units), as a string."
  (let ((rate (cdr (assoc 'rate goal))))
    (format (concat
	     (if (>= rate 1)
		 "%4d"
	       "%4.2f")
	     "/%s")
	    (cdr (assoc 'rate goal))
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
  (message (beeminder-goal-representation (car beeminder-goals))))


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
						(if (and (car beeminder-current-filters)
							 (not (cdr beeminder-current-filters)))
						    "" "s")))
				      (if beeminder-current-filters
					  (mapconcat #'beeminder-print-filter
						     beeminder-current-filters
						     ", ")
					"none")))
		      'face 'shadow)))

(defun beeminder-create-ewoc ()
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
  (goto-char (point-min)))

(defun beeminder-list-goals ()
  "Switch to a buffer containing the list of Beeminder goals."
  (interactive)
  (switch-to-buffer "*Beeminder goals*")
  (buffer-disable-undo)
  (unless beeminder-goals (beeminder-get-goals))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq beeminder-goals-ewoc (beeminder-create-ewoc))
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
  `(let ((current-goal-slug (cdr (assoc 'slug (ewoc-data (ewoc-locate beeminder-goals-ewoc))))))
     ,@body
     (ewoc-goto-node beeminder-goals-ewoc (ewoc-nth beeminder-goals-ewoc 0))
     (let ((current-node (ewoc-nth beeminder-goals-ewoc 0)))
       (while (and current-node
		   (not (string= (cdr (assoc 'slug (ewoc-data current-node)))
				 current-goal-slug)))
	 (ewoc-goto-next beeminder-goals-ewoc 1)
	 (setq current-node (ewoc-next beeminder-goals-ewoc current-node)))
       (unless current-node (goto-char (point-min))))))

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


;; Reloading goals
(defun beeminder-reload-goals-list ()
  "Reload the goals from the server."
  (interactive)
  (save-current-goal
   (message "Beeminder goals downloading...")
   (beeminder-get-goals)
   (message "Beeminder goals downloading...Done.")
   (beeminder-recreate-ewoc)))

(define-key beeminder-mode-map "g" #'beeminder-reload-goals-list)


;; Filtering goals

(defvar beeminder-current-filters '()
  "Alist of filters currently in effect.")

(defun beeminder-clear-filters ()
  "Clear all filters."
  (interactive)
  (setq beeminder-current-filters '())
  (beeminder-recreate-ewoc))

(define-key beeminder-mode-map "c" #'beeminder-clear-filters)

(defcustom beeminder-default-filter-days 3
  "Defalt number of days used for filtering by losedate.
If the user doesn't specify the number of days for filtering, all
goals with more than this amount of days left to losedate will be
filtered out."
  :type 'integer
  :group 'beeminder)

(defcustom beeminder-default-filter-donetoday 100
  "Default percentage of donetoday used for filtering."
  :type 'integer
  :group 'beeminder)

(defun beeminder-days-p (goal days)
  "Return nil if time to derailment of GOAL > DAYS."
  (<= (- (beeminder-time-to-days (cdr (assoc 'losedate goal)))
	 (beeminder-time-to-days (beeminder-current-time)))
      days))

(defun beeminder-donetoday-p (goal percentage)
  "Return nil if donetoday for GOAL > PERCENTAGE * day's amount."
  (< (/ (* 100 (cdr (assoc 'donetoday goal)))
	(/ (cdr (assoc 'rate goal))
	   (cl-case (intern (cdr (assoc 'runits goal)))
	     (y 365)
	     (m (/ 365 12))
	     (w 7)
	     (d 1)
	     (h (/ 1 24)))))
     percentage))

(defun beeminder-not-killed-p (goal kill-list)
  "Return nil if GOAL is in the KILL-LIST."
  (not (member (cdr (assoc 'slug goal)) kill-list)))

(defvar beeminder-filters `((days ,#'beeminder-days-p
				  ,beeminder-default-filter-days
				  "days to derailment (%d)"
				  "d2d(%d)"
				  "d")
			    (donetoday ,#'beeminder-donetoday-p
				       ,beeminder-default-filter-donetoday
				       "done today (%d%%)"
				       "dt(%d%%)"
				       "t")
			    (killed ,#'beeminder-not-killed-p
				    '()
				    (lambda (kill-list) (format "%d goal%s killed"
								(length kill-list)
								(if (= 1 (length kill-list))
								    "" "s")))
				    (lambda (kill-list) (format "%d gk" (length kill-list)))
				    ""))

  "List of possible filters.  Each element is a list, consisting of:
- symbol, denoting the filter,
- predicate (with two arguments - the goal and the parameter),
- default value for the parameter,
- formatting function (with one argument - the parameter) or a format
  string (with one placeholder)
- formatting function or a format string for the shortened version of header
- key for enabling the filter (as a string, passed to `kbd').")

(defun beeminder-kill-goal (goal-node)
  "Delete GOAL-NODE from `beeminder-goals-ewoc'."
  (interactive (list (current-or-read-goal)))
  (let ((inhibit-read-only t)
	(next-goal (or (ewoc-next beeminder-goals-ewoc goal-node)
		       (ewoc-prev beeminder-goals-ewoc goal-node))))
    (ewoc-delete beeminder-goals-ewoc goal-node)
    (ewoc-refresh beeminder-goals-ewoc)
    (push (cdr (assoc 'slug (ewoc-data goal-node)))
	  (alist-get 'killed beeminder-current-filters))
    (ewoc-set-hf beeminder-goals-ewoc (beeminder-ewoc-header) "")
    (if next-goal
	(ewoc-goto-node beeminder-goals-ewoc next-goal)
      (goto-char (point-min)))))

(define-key beeminder-mode-map (kbd "C-k") #'beeminder-kill-goal)

(defun beeminder-clear-kills ()
  "Unkill all killed goals."
  (interactive)
  (setq beeminder-current-filters
	(assq-delete-all 'killed beeminder-current-filters))
  (beeminder-recreate-ewoc))

(define-key beeminder-mode-map (kbd "C-w") #'beeminder-clear-kills)

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

;; Rethink how this command is invoked - the `last-command-event'
;; trick is fishy.  Also, PARAMETER should probably be optional.
(defun beeminder-filter-command (parameter)
  "Enable the selected filter with PARAMETER.
The filter is selected from `beeminder-filters' using
`last-command-event', i.e., the key which invoked this command.
PARAMETER, when given, overrides the default."
  (interactive "P")
  (let ((filter beeminder-filters))
    (while (and filter (not (string= (char-to-string last-command-event) (nth 5 (car filter)))))
      (setq filter (cdr filter)))
    (if (not filter)
	(error "This shouldn't happen -- beeminder-filter-command)")
      (setq filter (car filter))
      (setf (alist-get (car filter) beeminder-current-filters)
	    (cond
	     ((eq parameter '-)
	      nil)
	     ((null parameter)
	      (caddr filter))
	     (t (prefix-numeric-value parameter))))
      (setq beeminder-current-filters (rassq-delete-all nil beeminder-current-filters))
      (beeminder-recreate-ewoc))))

(define-key beeminder-mode-map (kbd "d") #'beeminder-filter-command)
(define-key beeminder-mode-map (kbd "t") #'beeminder-filter-command)

(provide 'beeminder)

;;; beeminder.el ends here
