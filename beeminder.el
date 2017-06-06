;;; beeminder.el --- Emacs client for Beeminder     -*- lexical-binding: t; -*-

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
(if (>= emacs-major-version 24)
    (progn
      (require 'cl-lib)
      (require 'anaphora)
      (defalias 'increasingp '<))
  (require 'cl)
  (defalias 'cl-reduce 'reduce)
  (defalias 'cl-find 'find)
  (defalias 'cl-incf 'incf)
  (defalias 'cl-decf 'decf)
  (defalias 'cl-case 'case)
  (defalias 'cl-delete 'delete*)
  (defmacro setq-local (var val)
    "This is taken from subr.el."
    `(set (make-local-variable ',var) ,val))
  (defmacro aif (cond then &rest else)
    `(let ((it ,cond))
       (if it ,then ,@else)))
  (defmacro awhen (cond &rest body)
    `(aif ,cond
	 (progn ,@body)))
  (defun increasingp (&rest args)
    "Return t if ARGS are in increasing order."
    (if (cdr args)
	(and (< (car args) (cadr args))
	     (apply #' increasingp (cdr args)))
      t)))
(require 'ewoc)

;;; Code:


;; Utilities
(defun trim-leading-whitespace (string)
  "Trim tabs and spaces from the beginning of STRING."
  (when (string-match "^[ \t]*" string)
    (replace-match "" nil nil string)))

(defun beeminder-alist-get (key alist)
  "Return the value associated to KEY in ALIST.
This function is needed for Emacsen older than v25."
  (cdr (assoc key alist)))

(defun beeminder-set-alist-value (key alist value)
  "Set the value corresponding to KEY in ALIST to VALUE.
Note: ALIST should be a symbol.  This is morally equivalent to
`(setf (alist-get key (symbol-value alist)) value)',
but works in older Emacsen."
  (let ((pair (assoc key (symbol-value alist))))
    (if pair
	(setcdr pair value)
      (set alist (acons key value (symbol-value alist))))))

(defun beeminder-inc-alist-value (key alist increment)
  "Increment the value corresponding to KEY in ALIST by INCREMENT.
Throw an error if KEY is not in ALIST."
  (let ((pair (assoc key alist)))
    (if pair
		(incf (cdr pair) increment)
	  (error "Nothing to increment"))))

(defun beeminder-to-list (sequence)
  "Turn SEQUENCE into a list."
  (append sequence nil))


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

(defcustom beeminder-default-timeout 30
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

(defun beeminder-request-get (request &optional params success-fun error-fun timeout)
  "Send a GET REQUEST to beeminder.com, with TIMEOUT.
Add the necessary details (including the username and the auth
token)."
  (request (beeminder-create-api-url request)
	   :parser #'json-read
	   :params (append params (list (cons "auth_token" beeminder-auth-token)))
	   :success success-fun
	   :error error-fun
	   :timeout (or timeout beeminder-default-timeout)))

(defun beeminder-request-post (request data success-fun error-fun &optional timeout)
  "Send a POST REQUEST with given DATA and TIMEOUT to beeminder.com.
Add the username and the auth token."
  (request (beeminder-create-api-url request)
	   :type "POST"
	   :data (append data
			 (list (cons "auth_token" beeminder-auth-token)))
	   :parser #'json-read
	   :success success-fun
	   :error error-fun
	   :timeout (or timeout beeminder-default-timeout)))

(defun beeminder-request-delete (request success-fun error-fun &optional timeout)
  "Send a DELETE request to beeminder.com, with TIMEOUT.
Add the necessary details (username and the auth token)."
  (request-response-data
   (request (concat (beeminder-create-api-url request))
	    :params (list (cons "auth_token" beeminder-auth-token))
	    :type "DELETE"
	    :parser #'json-read
	    :success success-fun
	    :error error-fun
	    :timeout (or timeout beeminder-default-timeout))))

(defun beeminder-request-put (request data success-fun error-fun &optional timeout)
  "Send a PUT request to beeminder.com, with TIMEOUT.
Add the necessary details (username and the auth token)."
  (request-response-data
   (request (beeminder-create-api-url request)
	    :type "PUT"
	    :data (append data
			  (list (cons "auth_token" beeminder-auth-token)))
	    :parser #'json-read
	    :success success-fun
	    :error error-fun
	    :timeout (or timeout beeminder-default-timeout))))


;; API calls

(defun last-goal-midnight (goal-deadline now) ; TODO: maybe refactor using beeminder-determine-date!
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

(defun last-user-midnight (now) ; TODO: maybe refactor using beeminder-determine-date!
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

(defun beeminder-sum-today-value (datapoints start-time stop-time)
  "Sum the value for DATAPOINTS between START-TIME and STOP-TIME."
  (cl-reduce #'+
	     (mapcar (lambda (datapoint)
		       (if (< start-time
			      (cdr (assoc 'timestamp datapoint))
			      stop-time)
			   (cdr (assoc 'value datapoint))
			 0))
		     datapoints)))

(defun beeminder-get-goals ()
  "Get all the user's Beeminder goals and put them in the
`beeminder-goals' variable."
  (beeminder-log "fetching goals...")
  (beeminder-request-get
   "/goals.json"
   ()
   (cl-function (lambda (&key data &allow-other-keys)
		  (beeminder-log "fetching goals.......")
		  (let ((goals (beeminder-to-list data))
			(now (beeminder-current-time)))
		    (beeminder-request-get
		     ".json"
		     (list
		      (cons "diff_since"
			    (number-to-string
			     (- (last-user-midnight now)
				(* beeminder-history-length 24 60 60)))))
		     (cl-function (lambda (&key data &allow-other-keys)
				    (let* ((datapoints ; extract datapoints alone
					    (mapcar
					     (lambda (goal) ; extract datapoints from a goal from data from API
					       (cons (beeminder-alist-get 'slug goal)
						     (beeminder-to-list (beeminder-alist-get 'datapoints goal))))
					     (cdr (assoc 'goals data))))
					   (deadlines ; deadlines alone
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
											    now)
								      (last-user-midnight now))))
						 (cons (car goal)
						       (beeminder-sum-today-value (cdr goal) last-midnight (time-to-seconds now)))))
					     datapoints)))
				      (cl-flet ((beeminder-join-goal-data (goal)
									  "Join GOAL data from various sources."
									  (let ((slug-str (cdr (assoc 'slug goal))))
									    (append (list (cons 'datapoints
												(cdr (assoc slug-str datapoints)))
											  (cons 'donetoday
												(cdr (assoc
												      (cdr (assoc 'slug goal))
												      today-values)))
											  (cons 'history-length beeminder-history-length))
										    goal))))
					(setq beeminder-goals (mapcar #'beeminder-join-goal-data goals))
					(mapc #'beeminder-clean-goal goals)
					(beeminder-log "fetching goals...done")
					(setq beeminder-reloading-in-progress nil)))))
		     #'beeminder-report-fetching-error))))
   #'beeminder-report-fetching-error))

(defun beeminder-clean-goal (goal)
  "Remove GOAL from `beeminder-dirty-alist' if needed.
The heuristics for that is simple: if the current curval is
different than the one recorded in that list, remove it."
  (let ((slug (intern (cdr (assoc 'slug goal)))))
	(if (and (cdr (assoc slug beeminder-dirty-alist))
			 (/= (cdr (assoc 'curval goal))
				 (cdr (assoc slug beeminder-dirty-alist))))
		(setq beeminder-dirty-alist (assq-delete-all slug beeminder-dirty-alist)))))

(cl-defun beeminder-report-fetching-error (&key error-thrown &allow-other-keys)
  "Report ERROR-THROWN when fetching goals."
  (beeminder-log (format "fetching goals...error: %s" error-thrown))
  (setq beeminder-reloading-in-progress nil))

(defun beeminder-refresh-goal (slug-str)
  "Refresh autodata and graph of the goal named SLUG-STR.
Please do not use unless really necessary, since it creates
a considerable server load."
  (interactive (list (cdr (assoc 'slug (current-or-read-goal)))))
  (beeminder-request-get (concat "/goals/" slug-str "/refresh_graph.json")
			 nil
			 (cl-function (lambda (&rest _)
					(beeminder-log
					 (format
					  "goal %s refreshed"
					  slug-str))))
			 (cl-function (lambda (&rest _)
					(beeminder-log
					 (format
					  "goal %s could not be refreshed"
					  slug-str))))))

(define-key beeminder-mode-map (kbd "G") #'beeminder-refresh-goal)


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
  "Return t if the point is before the first goal or if there is
no first goal."
  (aif (ewoc-nth beeminder-goals-ewoc 0)
      (< (point)
	 (ewoc-location it))
    t))

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

(defcustom beeminder-time-format "%FT%T%z"
  "Default time format for Beeminder comments.")

(defun beeminder-current-time-string (&optional timestamp)
  "Return TIMESTAMP (Unix time) as a string.
Use current time by default.  Format is hh:mm:ss tz."
  (format-time-string beeminder-time-format
		      (or timestamp (beeminder-current-time))))

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
       (org-read-date t t nil nil (beeminder-safe-time default))
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
   (beeminder-current-time-string timestamp)))

(defun beeminder-ask-for-comment (slug-str amount &optional default-comment)
  "Ask the user for the comment for the goal named SLUG-STR.
Include AMOUNT in the question, and default to DEFAULT-COMMENT."
  (beeminder-read-string
   (format "Comment for amount %s for goal %s: " (number-to-human-string amount) slug-str)
   nil nil default-comment))

(defcustom beeminder-ask-for-comment t
  "Non-nil means ask for comment when a goal is submitted.
This also serves as a confirmation that the user actually wants
to submit data to Beeminder (especially that the question
includes the goal slug and amount), so disabling of this option
is discouraged.")

(defun beeminder-make-goal-dirty (slug)
  "Make the goal with SLUG dirty."
  (beeminder-set-alist-value slug
			     'beeminder-dirty-alist
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

(defun beeminder-submit-datapoint (slug-str value &optional comment timestamp id)
  "Submit a datapoint to Beeminder goal SLUG-STR with AMOUNT.
Additional data are COMMENT and TIMESTAMP (as Unix time).  If
COMMENT is nil, then ask the user for the comment.  If TIMESTAMP
is nil, assume now.  If PRINT-MESSAGE is non-nil, print suitable
messages in the echo area.  If ID is non-nil, use it as requestid.

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
	  (value (if (numberp current-prefix-arg)
		     current-prefix-arg
		   (string-to-number (beeminder-read-string
				      (format "Datapoint value for %s%s: "
					      slug-str
					      (if yesterdayp " (yesterday)" ""))
				      nil nil "1"))))
	  (current-timestamp (time-to-seconds (beeminder-current-time))))
     (list slug-str
	   value
	   (unless beeminder-ask-for-comment
	     (beeminder-default-comment current-timestamp))
	   (or (when yesterdayp
		 (- current-timestamp (* 24 60 60)))
	       (when (consp current-prefix-arg)
		 (ask-for-timestamp))
	       current-timestamp))))
  (let ((timestamp (or timestamp (time-to-seconds (beeminder-current-time)))))
    (beeminder-log (format "submitting datapoint of %s for goal %s..."
			   (number-to-human-string value)
			   slug-str))
    (beeminder-request-post (format "/goals/%s/datapoints.json" slug-str)
			    (list
			     (cons "value" (format "%f" value))
			     (cons "comment" (or comment (beeminder-ask-for-comment
							  slug-str
							  value
							  (beeminder-default-comment timestamp))))
			     (if id (cons "requestid" id))
			     (cons "timestamp" (format "%s" timestamp)))
			    (cl-function (lambda (&rest _)
					   (beeminder-log (format "submitting datapoint of %s for goal %s...done"
								  (number-to-human-string value)
								  slug-str))
					   (let* ((slug (intern slug-str))
						  (goal (beeminder-slug-to-goal slug)))
					     (when goal
					       (beeminder-inc-alist-value 'donetoday goal value)
					       (beeminder-make-goal-dirty slug)))))
			    (cl-function (lambda (&rest _)
					   (beeminder-log
					    (format "submitting datapoint of %s for goal %s...failed"
						    (number-to-human-string value)
						    slug-str)
					    :error))))))

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


;; Logging
(define-derived-mode beeminder-log-mode special-mode "Beeminder log"
  "A major mode for logging beeminder.el actions.")

(defface beeminder-error '((t :foreground "#800" :weight bold))
	 "Face for displaying error notifications.")

(defface beeminder-warning '((t :foreground "#880" :weight bold))
	 "Face for displaying warning notifications.")

(defcustom beeminder-notification-expire-time 8
  "After that many seconds less important notifications expire.
TODO: not yet implemented."
  :type 'integer
  :group 'beeminder)

(defvar beeminder-notification-expiration-timer nil
  "Timer used to clear notifications after
`beeminder-notification-expire-time'.")

(defun beeminder-log (message &optional level)
  "Put MESSAGE into the log and possibly into the notification
area.  LEVEL can be `:error', `:warning', `:logonly' or `:nolog'.
Messages without any of these levels expire after
`beeminder-notification-expire-time' seconds."
  (let ((level-string (cl-case level
			(:error " error")
			(:warning " warning")
			(t ""))))
    (unless (eq level :nolog)
      (save-excursion
	(setq message (subst-char-in-string ?\n ?\s message t))
	(set-buffer (get-buffer-create "*Beeminder log*"))
	(beeminder-log-mode)
	(goto-char (point-max))
	(let ((inhibit-read-only t))
	  (insert (current-time-string) level-string ":    " message "\n"))))
    (unless (eq level :logonly)
      (message "Beeminder%s: %s"
	       level-string
	       message)
      (setq beeminder-notification
	    (cond ((eq level :error)
		   (propertize message 'face 'beeminder-error))
		  ((eq level :warning)
		   (propertize message 'face 'beeminder-warning))
		  (t message))))
    (awhen beeminder-notification-expiration-timer (cancel-timer it))
    (unless (memq level '(:error :warning))
      (setq beeminder-notification-expiration-timer
	    (run-at-time beeminder-notification-expire-time nil #'beeminder-clear-notification)))
    (when beeminder-goals-ewoc (beeminder-refresh-goals-list))))

(defun beeminder-clear-notification ()
  "Clear the notification."
  (interactive)
  (setq beeminder-notification nil)
  (beeminder-refresh-goals-list))

(define-key beeminder-mode-map (kbd "C") #'beeminder-clear-notification)

(defvar beeminder-notification nil
  "A message that should appear right below the header in
Beeminder mode.")

(defun beeminder-pop-log ()
  "Pop the Beeminder log buffer."
  (interactive)
  (pop-to-buffer "*Beeminder log*"))

(define-key beeminder-mode-map (kbd "L") #'beeminder-pop-log)


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

(defun beeminder-safe-time (time)
  "Convert TIME to Emacs time format if it is a number."
  (if (numberp time)
      (seconds-to-time time)
    time))

(defun beeminder-time-to-days (time)
  "Compute the number of days from 0001-12-31 BC until TIME.
Take into consideration `beeminder-when-the-day-ends'."
  (time-to-days
   (time-add (beeminder-safe-time time)
	     (seconds-to-time (- beeminder-when-the-day-ends)))))

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
	  ((<= delta 7)
	   (concat (if beeminder-human-time-use-weekday
		       (format-time-string " %a"
					   (time-add time
						     (seconds-to-time (- beeminder-when-the-day-ends))))
		     (format "  +%d" delta))
		   " "
		   (format-time-string "%R" time)))
	  (t (format-time-string "%Y-%m-%d"
				 (time-add time
					   (seconds-to-time (- beeminder-when-the-day-ends))))))))

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
  (if (beeminder-alist-get (intern (cdr (assoc 'slug goal)))
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
    (beeminder-log (format "next goal: %s (%d minute%s left, %s to do)"
			   (replace-regexp-in-string
			    " \\{2,\\}"
			    "  "
			    (beeminder-goal-representation goal))
			   minutes
			   (beeminder-plural-ending minutes)
			   (let ((limsum (cdr (assoc 'limsum goal))))
			     (string-match "[[:digit:]]+\\(\\.[[:digit:]]*\\)?" limsum)
			     (match-string 0 limsum))))))


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
  (funcall (nth 3 (assoc (car filter) beeminder-filters)) (cdr filter)))

(defun beeminder-ewoc-header ()
  "Generate header for the Beeminder EWOC."
  (concat (format (if beeminder-short-header
		      "Beeminder goals  user:%s  goals:%s/%d"
		    "Beeminder goals for user %s (%s goals displayed out of %d total)")
		  beeminder-username
		  (if beeminder-goals-ewoc
		      (length (ewoc-collect beeminder-goals-ewoc #'true))
		    0)
		  (length beeminder-goals))
	  (propertize (concat (format (if beeminder-short-header
					  "  srt:%s"
					"\nsorting criterion: %s")
				      (caddr beeminder-current-sorting-setting))
			      (if beeminder-short-header
				  (format "  e%s"
					  (if beeminder-show-everyday "+" "-"))
				(format "      everyday goals: %s"
					(if beeminder-show-everyday
					    "displayed"
					  "omitted")))
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
		      'face 'shadow)
	  (aif beeminder-notification (concat "    " it) "")))

(defun beeminder-create-goals-ewoc ()
  "Return a newly created EWOC for Beeminder goals."
  (ewoc-create (lambda (goal) (insert (beeminder-goal-representation goal)))
	       (beeminder-ewoc-header)))

(defun beeminder-populate-ewoc ()
  "Populate Beeminder EWOC using the goal list.
In particular, apply filtering and sorting settings.  Note: since
only the last sorting criterion is remembered, and sorting is
stable, this might actually change the ordering of goals, which
may have been sorted by another criterion previously."
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
  (aif (get-buffer "*Beeminder goals*")
      (switch-to-buffer it)
    (switch-to-buffer "*Beeminder goals*")
    (buffer-disable-undo)
    (beeminder-mode))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq beeminder-goals-ewoc (beeminder-create-goals-ewoc))
    (beeminder-populate-ewoc))
  (setq truncate-lines t)
  (unless beeminder-goals
    (beeminder-get-goals)))


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
  "Evaluate BODY and bring the point back to the current goal.
If the Beeminder EWOC disappeared (for some reason), just
evaluate the body."
  (declare (indent 0) (debug t))
  `(if beeminder-goals-ewoc
       (with-current-buffer (ewoc-buffer beeminder-goals-ewoc)
	 (let* ((current-goal-slug
		 (if (beeminder-before-first-goal-p)
		     nil
		   (beeminder-get-slug (ewoc-data (ewoc-locate beeminder-goals-ewoc)))))
		(current-line (unless current-goal-slug (line-number-at-pos))))
	   ,@body
	   (cond ((not current-goal-slug)
		  (goto-char (point-min))
		  (forward-line (1- current-line)))
		 (t
		  (ewoc-goto-node beeminder-goals-ewoc (ewoc-nth beeminder-goals-ewoc 0))
		  (let ((current-node (ewoc-nth beeminder-goals-ewoc 0)))
		    (while (and current-node
				(not (eq (beeminder-get-slug (ewoc-data current-node))
					 current-goal-slug)))
		      (ewoc-goto-next beeminder-goals-ewoc 1)
		      (setq current-node (ewoc-next beeminder-goals-ewoc current-node)))
		    (unless current-node (goto-char (point-min))))))))
     ,@body))

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
  (or (increasingp sec1 sec2 time)
      (increasingp time sec1 sec2)
      (increasingp sec2 time sec1)))

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


;; Refreshing view and reloading goals

(defun beeminder-refresh-goals-list ()
  "Refresh the goals list."
  (interactive)
  (save-current-goal
    (beeminder-populate-ewoc)))

(defvar beeminder-reloading-in-progress nil
  "Non-nil if currently reloading data from the server.")

(defun beeminder-clear-reloading-in-progress-flag ()
  "Clear the `beeminder-reloading-in-progress' flag.
Useful in case of an error."
  (interactive)
  (setq beeminder-reloading-in-progress nil))

(defun beeminder-reload-goals-list (&optional force)
  "Reload the goals from the server.
With a prefix argument, do it even if reloading is in progress
\(this is useful when the `beeminder-reloading-in-progress'
variable is somehow messed up)."
  (interactive "P")
  (if (and beeminder-reloading-in-progress
           (not force))
      (beeminder-log "fetching goals already in progress, please wait!")
    (setq beeminder-reloading-in-progress t)
    (beeminder-get-goals)))

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
	(copy-alist beeminder-current-filters))
  (beeminder-log (format "current filter settings %s saved." beeminder-current-filters)))

(defun beeminder-retrieve-filters ()
  "Retrieve saved filters."
  (interactive)
  (setq beeminder-current-filters
	(copy-alist beeminder-saved-filters))
  (beeminder-refresh-goals-list)
  (beeminder-log (format "filter settings %s retrieved." beeminder-current-filters)))

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

(defcustom beeminder-default-filter-urgent-hours 8
  "Default time (in hours) to deadline to consider a goal urgent."
  :type 'integer
  :group 'beeminder)

(defcustom beeminder-show-dirty-donetoday t
  "If non-nil, show dirty goals even if they would be normally
  filtered out by the \"donetoday\" filter."
  :type 'boolean
  :group 'beeminder)

(defcustom beeminder-show-everyday t
  "If non-nil, show \"everyday goals\" irrespective of the
  \"days\" filter.")

(defcustom beeminder-everyday-goals-list
  '()
  "A list of slugs of \"everyday goals\".  These are the goals which
should be done every day, so even when filtering goals with deadline
after some number of days, they should be shown.")

(defun beeminder-toggle-show-everyday (arg)
  "Toggle showing \"everyday goals\" if ARG is zero or nil.
If ARG is positive, turn it on; if negative, off."
  (interactive "P")
  (let ((narg (prefix-numeric-value arg)))
    (cond ((null arg)
	   (setq beeminder-show-everyday (not beeminder-show-everyday)))
	  ((> narg 0)
	   (setq beeminder-show-everyday t))
	  ((< narg 0)
	   (setq beeminder-show-everyday nil))))
  (beeminder-refresh-goals-list))

(define-key beeminder-mode-map "e" #'beeminder-toggle-show-everyday)
(define-key beeminder-filter-map "e" #'beeminder-toggle-show-everyday)

(defun beeminder-days-p (goal days)
  "Return nil if time to derailment of GOAL > DAYS.
If DAYS is negative, return nil if time to derailment of GOAL is
<= -DAYS.  If the goal is in `beeminder-everyday-goals-list',
return t anyway."
  (if (and beeminder-show-everyday
	   (memq (beeminder-get-slug goal) beeminder-everyday-goals-list))
      t
    (let ((days-left (- (beeminder-time-to-days (cdr (assoc 'losedate goal)))
			(beeminder-time-to-days (beeminder-current-time)))))
      (if (>= days 0)
	  (<= days-left days)
	(>= days-left (- days))))))

(defun beeminder-donetoday-p (goal percentage)
  "Return nil if donetoday for GOAL >= PERCENTAGE * day's amount.
If PERCENTAGE is negative, return nil if donetoday of GOAL is
less than PERCENTAGE * day's amount.  Take the variable
`beeminder-show-dirty-donetoday' into account."
  (if (and beeminder-show-dirty-donetoday
	   (beeminder-alist-get (beeminder-get-slug goal) beeminder-dirty-alist))
      t
    (let* ((rate (beeminder-get-rate goal))
	   (daily-rate (/ rate
			  (cl-case (intern (cdr (assoc 'runits goal)))
			    (y 365.0)
			    (m (/ 365.0 12))
			    (w 7.0)
			    (d 1.0)
			    (h (/ 1 24.0)))))
	   (100*donetoday (* 100 (cdr (assoc 'donetoday goal))))
	   (percentage*daily-rate (* percentage daily-rate)))
      (when (> rate 0)
	(cond ((> percentage 0)
	       (< 100*donetoday
		  percentage*daily-rate))
	      ((zerop percentage)
	       (zerop 100*donetoday))
	      (t
	       (>= 100*donetoday (- percentage*daily-rate))))))))

(defun beeminder-calculate-midnight-offset (seconds)
  "Add (* 24 60 60) to SECONDS if negative."
  (if (> seconds 0)
      seconds
    (+ (* 24 60 60) seconds)))

(defun beeminder-urgent-p (goal hours)
  "Return nil if time to deadline for GOAL is > HOURS.
If HOURS is negative or zero, return nil if time to deadline is
<= -HOURS."
  (let* ((deadline (beeminder-calculate-midnight-offset (beeminder-alist-get 'deadline goal)))
	 (now (decode-time (beeminder-current-time)))
	 (now-sec (car now))
	 (now-min (cadr now))
	 (now-hour (caddr now))
	 (now-time (+ now-sec (* 60 now-min) (* 3600 now-hour)))
	 (time-to-deadline (beeminder-calculate-midnight-offset (- deadline now-time))))
    (if (> hours 0)
	(<= time-to-deadline (* 3600 hours))
      (> time-to-deadline (* -3600 hours)))))

(defun beeminder-not-killed-p (goal kill-list)
  "Return nil if GOAL is in the KILL-LIST."
  (not (member (beeminder-get-slug goal) kill-list)))

(defvar beeminder-filters `((losedate ,#'beeminder-days-p
				      ,beeminder-default-filter-days
				      (lambda (days)
					(format (if beeminder-short-header
						    "d2d(%s%d)"
						  "days to derailment (%s%d)")
						(if (>= days 0) "<=" ">=")
						(abs days))))
			    (donetoday ,#'beeminder-donetoday-p
				       ,beeminder-default-filter-donetoday
				       (lambda (donetoday)
					 (format (if beeminder-short-header
						     "dt(%s%d%%)"
						   "done today (%s%d%%)")
						 (cond ((> donetoday 0) "<")
						       ((zerop donetoday) "=")
						       (t ">="))
						 (abs donetoday))))
			    (urgent ,#'beeminder-urgent-p
				    ,beeminder-default-filter-urgent-hours
				    (lambda (hours)
				      (format (if beeminder-short-header
						  "u(%s%dh)"
						"urgent (%s%d hours to deadline)")
					      (if (> hours 0) "<=" ">")
					      (abs hours))))
			    (killed ,#'beeminder-not-killed-p
				    '()
				    (lambda (kill-list)
				      (format (if beeminder-short-header
						  "%dgk"
						"%d goal%s killed")
					      (length kill-list)
					      (beeminder-plural-ending (length kill-list))))))

  "List of possible filters.  Each element is a list, consisting of:
- symbol, denoting the filter,
- predicate (with two arguments - the goal and the parameter),
- default value for the parameter,
- formatting function (with one argument - the parameter).")

(defun beeminder-kill-goal (gnode)
  "Delete GNODE from `beeminder-goals-ewoc'."
  (interactive (list (beeminder-slug-to-gnode (intern (cdr (assoc 'slug (current-or-read-goal)))))))
  (if gnode
      (let ((inhibit-read-only t)
	    (next-goal (or (ewoc-next beeminder-goals-ewoc gnode)
			   (ewoc-prev beeminder-goals-ewoc gnode))))
	(ewoc-delete beeminder-goals-ewoc gnode)
	(ewoc-refresh beeminder-goals-ewoc)
	(beeminder-set-alist-value 'killed
				   'beeminder-current-filters
				   (cons (beeminder-get-slug (ewoc-data gnode))
					 (beeminder-alist-get 'killed beeminder-current-filters)))
	(ewoc-set-hf beeminder-goals-ewoc (beeminder-ewoc-header) "")
	(if next-goal
	    (ewoc-goto-node beeminder-goals-ewoc next-goal)
	  (goto-char (point-min)))
	(beeminder-log (format "goal %s killed (hidden from view)." (cdr (assoc 'slug (ewoc-data gnode))))))
    (beeminder-log (format "goal %s already killed." (cdr (assoc 'slug (ewoc-data gnode)))))))

(define-key beeminder-mode-map (kbd "C-k") #'beeminder-kill-goal)
(define-key beeminder-filter-map "k" #'beeminder-kill-goal)

(defun beeminder-show-kills ()
  "Show all killed goals."
  (interactive)
  (beeminder-log (format "killed goals: %s."
			 (aif (beeminder-alist-get 'killed beeminder-current-filters)
			     (mapconcat #'symbol-name it ", ")
			   "none"))))

(defun beeminder-clear-kills ()
  "Unkill all killed goals."
  (interactive)
  (setq beeminder-current-filters
	(assq-delete-all 'killed beeminder-current-filters))
  (beeminder-refresh-goals-list))

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
  (beeminder-set-alist-value filter 'beeminder-current-filters parameter)
  (setq beeminder-current-filters (rassq-delete-all nil beeminder-current-filters))
  (beeminder-refresh-goals-list))

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

(defun beeminder-filter-by-urgent (&optional hours)
  "Filter out goals with time to deadline greater than HOURS."
  (interactive "P")
  (beeminder-enable-filter 'urgent
			   (beeminder-filter-parameter hours
						       beeminder-default-filter-urgent-hours)))

(define-key beeminder-mode-map (kbd "d") #'beeminder-filter-by-losedate)
(define-key beeminder-filter-map (kbd "d") #'beeminder-filter-by-losedate)
(define-key beeminder-mode-map (kbd "t") #'beeminder-filter-by-donetoday)
(define-key beeminder-filter-map (kbd "t") #'beeminder-filter-by-donetoday)
(define-key beeminder-mode-map (kbd "u") #'beeminder-filter-by-urgent)
(define-key beeminder-filter-map (kbd "u") #'beeminder-filter-by-urgent)


;; Displaying goal details

(defcustom beeminder-goal-template-fields-alist
  '((slug . (propertize (symbol-name (beeminder-get-slug goal)) 'face (beeminder-goal-face goal)))
    (limsum . (propertize (cdr (assoc 'limsum goal)) 'face (beeminder-goal-face goal)))
    (backburnerp . (if (string= (cdr (assoc 'burner goal)) "backburner") "(backburner)" ""))
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
    (datapoints . (propertize (beeminder-format-datapoints goal) 'face
			      'shadow))
    (history-length . (highlight-subtly (let ((hl (cdr (assoc 'history-length goal))))
					  (if (zerop hl) "all" (format "%s" hl))))))
  "Alist of symbols and corresponding pieces of code to evaluate
and insert the result in the goal details info.")

(defun beeminder-display-time-field (alist field)
  "Return ALIST's (unix-time) FIELD formatted."
  (format-time-string "%x %X" (seconds-to-time (cdr (assoc field alist)))))

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
				 (eval (cdr it) `((goal . ,goal) t))
			       (highlight-subtly (format "%s" (cdr (assoc sexp goal))))))
			    (t (eval sexp))))))))

(define-derived-mode beeminder-goal-mode special-mode "Beeminder goal"
  "A major mode for a buffer displaying details of a Beeminder goal,
in particular the history of datapoints.")

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

(defface subtle-highlight '((t :foreground "#004400"))
  "Face for subtly highlighting things.")

(defun highlight-subtly (string)
  "Make STRING stand out, but only a little."
  (propertize string 'face 'subtle-highlight))

(defun number-to-human-string (number &optional width)
  "Convert NUMBER to a human-friendly form, at least WIDTH characters.
If NUMBER is greater than 10, use one decimal place.  Otherwise,
use two.  Trim any non-significant trailing zeros and the decimal
point if needed."
  (let ((str (replace-regexp-in-string
	      "\\.?0+$" ""
	      (format (cond
		       ((> number 10) "%.1f")
		       (t "%.2f"))
		      number))))
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

Recent datapoints (#history-length days):
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

(defvar beeminder-detailed-goal nil
  "The current goal in the details buffer.")

(defun beeminder-display-goal-details (goal)
  "Display details about GOAL in a temporary buffer."
  (interactive (list (current-or-read-goal)))
  (pop-to-buffer "*Beeminder goal details*")
  (remove-images (point-min) (point-max))
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
      (beeminder-request-delete
       (concat "/goals/" (cdr (assoc 'slug beeminder-detailed-goal)) "/datapoints/" id ".json")
       (cl-function (lambda (&rest _)
		      (let ((datapoints (cdr (assoc 'datapoints beeminder-detailed-goal))))
			(beeminder-inc-alist-value 'donetoday
						   beeminder-detailed-goal
						   (- (cdr (assoc 'value (cl-find id datapoints
										  :key (lambda (dp)
											 (cdr (assoc 'id dp)))
										  :test #'string=)))))
			(cl-delete id datapoints :key (lambda (dp) (cdr (assoc 'id dp))) :test #'string=)
			(beeminder-make-goal-dirty (beeminder-get-slug beeminder-detailed-goal))
			(beeminder-refresh-goal-details)
			(beeminder-log (format "datapoint %s succesfully deleted" id)))))
       (cl-function (lambda (&key error-thrown &allow-other-keys)
		      (beeminder-log (format "error while deleting datapoint %s for goal %s: %s"
					     id
					     (cdr (assoc 'slug beeminder-detailed-goal))
					     error-thrown)
				     :error))))))

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
				       (1+ (position
					    id
					    datapoints
					    :key (lambda (dp)
						   (cdr (assoc 'id dp)))
					    :test #'string=)))
				 nil t))))
    (beeminder-log "updating datapoint...")
    (beeminder-request-put (format "/goals/%s/datapoints/%s.json"
				   (beeminder-get-slug beeminder-detailed-goal)
				   id)
			   (list
			    (cons "value" (format "%s" value))
			    (cons "comment" comment)
			    (cons "timestamp" (format "%s" timestamp)))
			   (cl-function (lambda (&rest _) (beeminder-log "updating datapoint...done")))
			   (cl-function (lambda (&rest _) (beeminder-log "updating datapoint failed!" :error))))))

(define-key beeminder-goal-mode-map (kbd "e") #'beeminder-edit-datapoint)

(defun beeminder-display-raw-goal-details ()
  "Display the raw details about GOAL in a temporary buffer.
The internal representation is an alist."
  (interactive)
  (let ((goal beeminder-detailed-goal))
    (pop-to-buffer "*Beeminder raw goal details*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (pp-to-string goal))
      (goto-char (point-min))
      (special-mode))))

(define-key beeminder-goal-mode-map (kbd ".") #'beeminder-display-raw-goal-details)

(defun beeminder-view-in-browser (goal)
  "View GOAL in the web browser."
  (interactive (list (or beeminder-detailed-goal (current-or-read-goal))))
  (browse-url (format "https://beeminder.com/%s/%s" beeminder-username (beeminder-alist-get 'slug goal))))

(define-key beeminder-mode-map (kbd "W") #'beeminder-view-in-browser)
(define-key beeminder-mode-goal-map (kbd "W") #'beeminder-view-in-browser)


;; Downloading more datapoints
(defun beeminder-download-datapoints (slug-str days)
  "Download datapoints for goal named SLUG-STR from last DAYS.
If called interactively in the *Beeminder goal details* buffer, use
current goal; otherwise, ask for the goal.  If called without a prefix
argument, increase the downloaded history by
`beeminder-history-length' days."
  (interactive (list (if beeminder-detailed-goal
			 (cdr (assoc 'slug beeminder-detailed-goal))
		       (cdr (assoc 'slug (current-or-read-goal))))
		     current-prefix-arg))
  (setq days
	(if days
	    (prefix-numeric-value days)
	  beeminder-history-length))
  (setq days
	(cond ((> days 0)
	       (+ (cdr (assoc 'history-length (beeminder-slug-to-goal (intern slug-str))))
		  days))
	      ((< days 0)
	       (- days))
	      (t 0)))
  (beeminder-log (format "fetching datapoints for goal %s..." slug-str))
  (beeminder-request-get (format "/goals/%s.json" slug-str)
			 (cons (cons "datapoints" "true")
			       (unless (zerop days)
				 (cons (cons "diff_since"
					     (number-to-string
					      (- (last-user-midnight (beeminder-current-time))
						 (* days 24 60 60))))
				       nil)))
			 (cl-function (lambda (&key data &allow-other-keys)
					(beeminder-log (format "fetching datapoints for goal %s...done" slug-str))
					(let* ((gl (beeminder-slug-to-goal (intern slug-str)))
					       (dp (assoc 'datapoints gl))
					       (hl (assoc 'history-length gl)))
					  (setcdr dp (beeminder-to-list (cdr (assoc 'datapoints data))))
					  (setcdr hl days)
					  (when beeminder-detailed-goal
					    (beeminder-refresh-goal-details)))))
			 (cl-function (lambda (&key error-thrown &allow-other-keys)
					(beeminder-log (format "fetching datapoints for goal %s...error: %s" slug-str error-thrown))))))

(define-key beeminder-goal-mode-map (kbd "m") #'beeminder-download-datapoints)


;; Statistics
(defun beeminder-determine-date (time day-end)
  "Return date for TIME, taking DAY-END into account.
TIME is the number of seconds counted from the beginning of Unix
epoch; DAY-END is the offset from midnight in seconds.  The date
is a string in ISO 8601 basic format (i.e., \"20160417\" for
April 17, 2016)."
  (format-time-string "%Y%m%d" (time-subtract time day-end)))

(defun beeminder-gather-datapoints-by-day (goal)
  "Return an alist of datapoints, collated by date.
The car of each entry is a string representing the date in ISO
8601 basic format (i.e., \"20160417\" for April 17, 2016), and
the cdr is the list of datapoints.  If
`beeminder-use-goal-midnight-today-values' is nil, use the goal's
\"midnight\" setting to determine the date; otherwise, use
`beeminder-when-the-day-ends'."
  (let ((day-end (if beeminder-use-goal-midnight-today-values
		     beeminder-when-the-day-ends
		   (cdr (assoc 'deadline goal))))
	datapoints-by-day)
    (mapc (lambda (datapoint)
	    (let ((date (beeminder-determine-date (cdr (assoc 'timestamp datapoint)) day-end)))
	      (aif (assoc date datapoints-by-day)
		  (push datapoint (cdr it))
		(push (list date datapoint) datapoints-by-day))))
	  (cdr (assoc 'datapoints goal)))
    datapoints-by-day))

;; (defun beeminder-gather-datapoints-by-day (goal)
;;   "Return an alist of datapoints, collated by date.
;; The car of each entry is a string representing the date in ISO
;; 8601 basic format (i.e., \"20160417\" for April 17, 2016), and
;; the cdr is the list of datapoints.  If
;; `beeminder-use-goal-midnight-today-values' is nil, use the goal's
;; \"midnight\" setting to determine the date; otherwise, use
;; `beeminder-when-the-day-ends'."
;;   (let ((day-end (if beeminder-use-goal-midnight-today-values
;; 		     beeminder-when-the-day-ends
;; 		   (cdr (assoc 'deadline goal))))
;; 	datapoints-by-day)
;;     (mapc (lambda (datapoint)
;; 	    (let ((date (beeminder-determine-date (cdr (assoc 'timestamp datapoint)) day-end)))
;; 	      (aif (assoc date datapoints-by-day)
;; 		  (push datapoint (cdr it))
;; 		(push (list date datapoint) datapoints-by-day))))
;; 	  (cdr (assoc 'datapoints goal)))
;;     datapoints-by-day))

(defun beeminder-uniq-mean (list)
  "Return the mean of numbers in LIST after deleting
  duplicates."
  (let ((list list))
	(/ (apply #'+ (delete-dups list))
	   (length list)
	   1.0)))

(defun beeminder-median (list)
  "Return a median of numbers in LIST.
If LIST contains an even number of elements n, return
the (n/2)-th one."
  (let ((list list) median-list)
    (setq list (sort list #'<)
	  median-list list)
    (while (cddr list)
      (setq median-list (cdr median-list)
	    list (cddr list)))
    (if (cdr list)
	(* 0.5 (+ (car median-list) (cadr median-list)))
      (car median-list))))

(defvar beeminder-aggregation-methods
  '(("sum" . (lambda (dps) (apply #'+ dps)))
    ("last" . car) ; this is no mistake - the list is in reverse order!
    ("first" . (lambda (dps) (car (last dps))))
    ("min" . (lambda (dps) (apply #'min dps)))
    ("max" . (lambda (dps) (apply #'max dps)))
    ("truemean" . (lambda (dps) (/ (apply #'+ dps) (length dps) 1.0)))
    ("uniqmean" . beeminder-uniq-mean)
    ("mean" . beeminder-uniq-mean)
    ("median" . beeminder-median)
    ("jolly" . (lambda (dps) (if dps 1 0)))
    ("binary" . (lambda (dps) (if dps 1 0)))
    ("nonzero" . (lambda (dps) (if (cl-some (lambda (dp) (not (= 0 dp))) dps) 1 0)))
    ("triangle" . (lambda (dps) (let ((sum (apply #'+ dps))) (* sum (1+ sum) 0.5))))
    ("square" . (lambda (dps) (let ((sum (apply #'+ dps))) (* sum sum))))
    ("count" . length))
  "An alist mapping aggregation methods to actual functions.")

(defun beeminder-aggregate-values (values aggday)
  "Aggregate VALUES (from one day) using the AGGDAY method."
  (funcall (beeminder-alist-get aggday beeminder-aggregation-methods)
	   values))

(defun beeminder-aggregate-datapoints (datapoints-by-day aggday)
  "Aggregate DATAPOINTS-BY-DAY using the AGGDAY method."
  (mapcar (lambda (day-datapoints)
	    (cons (car day-datapoints)
		  (beeminder-aggregate-values
		   (mapcar (lambda (dp) (beeminder-alist-get 'value dp))
			   (cdr day-datapoints))
		   aggday)))
	  datapoints-by-day))


;; Displaying graphs

(defun beeminder-download-graph (slug-str)
  "Download graph for goal SLUG and put it in the tmp directory,
under the \"beeminder-el\" subdirectory and filename
\"SLUG.png\".  Return the full filename."
  (make-directory (concat temporary-file-directory "beeminder-el") t)
  (let* ((image-file (concat temporary-file-directory "beeminder-el/" slug-str ".png"))
	 (inhibit-message t))
    (url-copy-file (beeminder-alist-get 'graph_url (beeminder-slug-to-goal (intern slug-str)))
		   image-file
		   t)
    image-file))

(defun beeminder-insert-graph (image)
  "Insert IMAGE at end-of-buffer.  Remove any existing images
first and position the point and window so that the image can be
seen next."
  (remove-images (point-min) (point-max))
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (remove-images (point-min) (point-max))
    (save-excursion
      (insert "\n")
      (put-image image (point-max) "[Graphs are not supported in this Emacs!]")))
  (let* ((size (image-size image))
	 (width (ceiling (car size)))
	 (height (ceiling (cdr size))))
    (fit-window-to-buffer (selected-window) height height width width))
  (recenter 0))

(defun beeminder-display-graph (slug-str)
  "Download the graph of the goal SLUG-STR and display it.
Switch to the \"details\" buffer first if needed.  Do nothing if
the graph is already displayed."
  (interactive (list (if beeminder-detailed-goal
			 (cdr (assoc 'slug beeminder-detailed-goal))
		       (cdr (assoc 'slug (current-or-read-goal))))))
  (let* ((image-file (beeminder-download-graph slug-str))
	 (image (create-image image-file)))
    (beeminder-display-goal-details (beeminder-slug-to-goal (intern slug-str))) 
    (beeminder-insert-graph image)))

(define-key beeminder-goal-mode-map (kbd "i") #'beeminder-display-graph)
(define-key beeminder-mode-map (kbd "i") #'beeminder-display-graph)


;; Org-mode integration

(defcustom beeminder-org-inherit-beeminder-properties nil
  "Make beeminder.el use property inheritance.")

(defcustom beeminder-org-default-comment "%h at %t"
  "Default format of the comment")

(defun beeminder-org-string-substitute (string)
  "Substitute strings for percent-sign codes in STRING.
Codes are: `%t' - current time, `%h' - current headline, `%p' -
current path, `%%' - percent sign."
  (let ((time (beeminder-current-time-string))
	(headline (substring-no-properties (org-get-heading t t)))
	(path (mapconcat #'identity (org-get-outline-path t) "/")))
    (format-spec string
		 `((?% . "%")
		   (?t . ,time)
		   (?h . ,headline)
		   (?p . ,path)))))

(defun beeminder-org-generate-comment ()
  "Given the comment property, generate the comment text.  Assume
that the point is in the right place."
  (let ((comment-prop
	 (or (org-entry-get (point)
			    "comment"
			    beeminder-org-inherit-beeminder-properties)
	     beeminder-org-default-comment)))
    (cond
     ((string= comment-prop "time")
      (concat "via Org-mode at " (beeminder-current-time-string)))
     ((string= comment-prop "ask")
      nil)
     ((or (string= comment-prop "headline")
	  (null comment-prop))
      (substring-no-properties (org-get-heading t t)))
     ((string= comment-prop "path")
      (mapconcat #'identity (org-get-outline-path t) "/"))
     (t (beeminder-org-string-substitute comment-prop)))))

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
		     org-done-keywords))
	(let* ((slug-str (org-entry-get position
					"slug"
					beeminder-org-inherit-beeminder-properties))
	       (amount (aif (org-entry-get position
					   "amount"
					   beeminder-org-inherit-beeminder-properties)
			   (string-to-number it)
			 1))
	       (comment (beeminder-org-generate-comment)))
	  (beeminder-submit-datapoint slug-str amount comment)))))

(defun beeminder-org-submit-clock-at-point ()
  "Submit the data from the clock item at point to Beeminder.
This is mainly useful if submitting on clocking out (see
`beeminder-org-submit-on-clock-out' failed for some reason, so
that the user may want to submit clock items later."
  (interactive)
  (let ((element (org-element-at-point)))
    (if (eq (org-element-type element) 'clock)
	(let ((timestamp (org-element-property :value element))
	      (duration (org-element-property :duration element)))
	  (when (string-match "\\([[:digit:]]+\\):\\([[:digit:]]\\{2\\}\\)" duration)
	    (let* ((minutes (+ (* 60 (string-to-number (match-string 1 duration)))
			       (string-to-number (match-string 2 duration))))
		   (slug-str (org-entry-get (point)
					    "slug"
					    beeminder-org-inherit-beeminder-properties))
		   (comment (beeminder-org-generate-comment))
		   (multiplier (cl-case (intern (or (org-entry-get (point)
								   "unit"
								   beeminder-org-inherit-beeminder-properties)
						    ""))
				 ((hour hours)
				  (/ 1 60.0))
				 ((hail-Mary hail-Marys)
				  3)
					; 1 hail-Mary ≈ 20 seconds
				 (t 1)))
		   (year-end (org-element-property :year-end timestamp))
		   (month-end (org-element-property :month-end timestamp))
		   (day-end (org-element-property :day-end timestamp))
		   (hour-end (org-element-property :hour-end timestamp))
		   (minute-end (org-element-property :minute-end timestamp))
		   (id (format "%04d%02d%02d%02d%02dto%04d%02d%02d%02d%02d"
			       (org-element-property :year-start timestamp)
			       (org-element-property :month-start timestamp)
			       (org-element-property :day-start timestamp)
			       (org-element-property :hour-start timestamp)
			       (org-element-property :minute-start timestamp)
			       year-end
			       month-end
			       day-end
			       hour-end
			       minute-end))
		   (timestamp (time-to-seconds (encode-time
						0
						minute-end
						hour-end
						day-end
						month-end
						year-end))))
	      (beeminder-submit-datapoint slug-str (* minutes multiplier)
					  comment
					  timestamp
					  id))))
      (beeminder-log "no clock at point!" :nolog))))

(defcustom beeminder-org-submit-all-clocks-default-minutes (* 24 60)
  "By default, only the clocks from this many lastminutes will be
submitted by `beeminder-org-submit-all-clocks'.  Does not have to
be an integer (i.e., value like 0.5 means 30 seconds).")

(defun beeminder-org-submit-all-clocks (begin end minutes)
  "Submit all clocks from last MINUTES in the region to Beeminder.
In interactive use, use region if active and current subtree
otherwise.  Use with caution!"
  (interactive (if (use-region-p)
		   (list (region-beginning)
			 (region-end)
			 current-prefix-arg)
		 (list nil nil current-prefix-arg)))
  (unless (numberp minutes)
    (setq minutes beeminder-org-submit-all-clocks-default-minutes))
  (save-excursion
    (save-restriction
      (narrow-to-region
       (or begin (progn (org-back-to-heading t)
			(point)))
       (or end (progn (org-end-of-subtree t t)
		      (when (and (org-at-heading-p) ; see org-narrow-to-subtree
				 (not (eobp)))
			(backward-char 1))
		      (point))))
      (goto-char (point-min))
      (while (re-search-forward "CLOCK: " nil t)
	(let ((eap (org-element-at-point)))
	  (when (and (eq (org-element-type eap)
			 'clock)
		     (eq (org-element-property :status eap)
			 'closed))
	    (let ((ts (org-element-property :value eap)))
	      (when (< (time-to-seconds
			(time-subtract (beeminder-current-time)
				       (encode-time 0
						    (org-element-property :minute-end ts)
						    (org-element-property :hour-end ts)
						    (org-element-property :day-end ts)
						    (org-element-property :month-end ts)
						    (org-element-property :year-end ts))))
		       (* 60 minutes))
		(beeminder-org-submit-clock-at-point)))))))))

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

(define-minor-mode beeminder-org-integration-mode
  "Toggle a (global) minor mode for Org/Beeminder integration.
When on, clocking out and marking as DONE for headlines with suitable
:beeminder: property is submitted automatically."
  :init-value nil
  :global t
  :lighter " B-O"
  (if beeminder-org-integration-mode
      (progn
	(add-hook 'org-trigger-hook #'beeminder-org-submit-on-done)
	(add-hook 'org-clock-out-hook #'beeminder-org-submit-on-clock-out))
    (remove-hook 'org-trigger-hook #'beeminder-org-submit-on-done)
    (remove-hook 'org-clock-out-hook #'beeminder-org-submit-on-clock-out)))


(provide 'beeminder)


;;; beeminder.el ends here
