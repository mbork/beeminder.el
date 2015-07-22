;; This will hopefully become an Emacs client for Beeminder some day

(require 'json)
(require 'request)
(require 'cl)
(require 'ewoc)
(require 'seq)


;; Settings
(defcustom beeminder-username ""
  "User name for the Beeminder account.")

(defcustom beeminder-auth-token ""
  "Authentication token, taken from
https://www.beeminder.com/api/v1/auth_token.json.")

(defcustom beeminder-api-url "https://www.beeminder.com/api/v1/users/"
  "The URL for making API calls.")

(defvar beeminder-goals nil
  "The vector of sexps representing goals.  Updated by
  BEEMINDER-GET-GOALS.")

(defcustom beeminder-default-timeout 4
  "Default timeout for HTTP requests sent over to
beeminder.com.")


;; Beeminder mode

(define-derived-mode beeminder-mode special-mode "Beeminder"
  "A major mode for a buffer with Beeminder goal list.")


;; API interface

(defun beeminder-create-api-url (string)
  "Prepend the Beeminder site address and the username to the
given STRING.  STRING should begin with a slash."
  (concat beeminder-api-url beeminder-username string))

(defun beeminder-request-get (req &optional timeout)
  "Send a GET request to beeminder.com, adding the necessary
details (including the username and the auth token)."
  (request-response-data
   (request (concat (beeminder-create-api-url req)
		    (if (string-match "\\?" req) "&" "?") ; this is hackish...
		    "auth_token="
		    beeminder-auth-token)
	    :parser #'json-read
	    :sync t
	    :timeout (or timeout beeminder-default-timeout))))

(defun beeminder-request-post (req data &optional timeout)
  "Send a POST request to beeminder.com, adding the username and
the auth token."
  (request-response-data
   (request (beeminder-create-api-url req)
	    :type "POST"
	    :data data
	    :parser #'json-read
	    :sync t
	    :timeout (or timeout beeminder-default-timeout))))


;; API calls (currently synchronous only)

(defun beeminder-get-goals ()
  "Get all the user's Beeminder goals.  The request returns
a vector of sexps - each sexp describes one goal."
  (setq beeminder-goals (beeminder-request-get "/goals.json")))

(defun beeminder-refresh-goal (slug)
  "Refresh autodata and graph."
  (beeminder-request-get (concat "/goals/" slug "/refresh_graph.json")))


;; Submitting datapoints

(defun current-time-hmsz-string (&optional timestamp)
  "Return current time (or TIMESTAMP ,given as Unix time) as
a string, in the format hh:mm:ss tz."
  (let ((decoded-time (decode-time (or (seconds-to-time timestamp)
				       (current-time)))))
    (format "%02d:%02d:%02d %s"
	    (caddr decoded-time)
	    (cadr decoded-time)
	    (car decoded-time)
	    (cadr (current-time-zone (apply #'encode-time decoded-time))))))

(defun beeminder-submit-datapoint (slug amount &optional comment timestamp print-message)
  "Submit a datapoint to beeminder goal SLUG with data: AMOUNT,
COMMENT and TIMESTAMP (as Unix time).  If PRINT-MESSAGE is
non-nil, print suitable messages in the echo area."
  (interactive
   (let* ((slug (cdr (assoc 'slug (ewoc-data (ewoc-locate
					      beeminder-goals-ewoc)))))
	  (amount (if (numberp current-prefix-arg)
		      current-prefix-arg
		    (string-to-number (read-string
				       (format "Datapoint value for %s: " slug)
				       nil nil "1"))))
	  (current-timestamp (time-to-seconds (current-time)))
	  (default-comment (concat
			    "via Emacs "
			    (current-time-hmsz-string current-timestamp))))
     (list slug
	   amount
	   (read-string
	    (format "Comment for amount %d for goal %s: " amount slug)
	    nil nil default-comment)
	   (or (if (eq current-prefix-arg '-)
		   (- current-timestamp (* 24 60 60)))
	       (if (consp current-prefix-arg)
		   (ask-for-timestamp))
	       current-timestamp)
	   t)))
  (if print-message (message (format "Submitting datapoint of %d for goal %s..." amount slug)))
  (beeminder-request-post (format "/goals/%s/datapoints.json" slug)
			  (concat (format "auth_token=%s&value=%f&comment=%s&timestamp=%d"
					  beeminder-auth-token
					  amount
					  (or comment default-comment)
					  (or timestamp current-timestamp))))
  (if print-message (message (format "Submitting datapoint of %d for goal %s...  Done." amount slug))))

(define-key beeminder-mode-map (kbd "RET") #'beeminder-submit-datapoint)


;; Sorting EWOC

(defun true (&rest args)
  "Always return t."
  t)

(defun ewoc-sort (ewoc pred)
  "Sort EWOC, comparing its nodes using PRED.  Since the author of
EWOC didn't really care for sorting, and neither do I, we first
collect the nodes into a list, sort it using Elisp's sort, and then
recreate the EWOC."
  (let ((ewoc-list (ewoc-collect ewoc #'true)))
    (ewoc-filter ewoc #'ignore)
    (mapcar (lambda (node) (ewoc-enter-last ewoc node))
	    (sort ewoc-list pred))))


;; Displaying goals

(defvar beeminder-human-time-use-weekday t
  "Whether BEEMINDER-HUMAN-TIME uses weekdays or number of days from
today for times within a week from now.")

(defvar beeminder-tomorrow-code "tom"
  "The abbreviation for \"tomorrow\".")

(defcustom beeminder-when-the-day-ends (* 6 60 60)
  "Number of seconds from midnight when the day is assumed to end.
Times up to this time will be considered to belong to the
previous day.")

(defun beeminder-time-to-days (time)
  "Like time-to-days, but taking `beeminder-when-the-day-ends'
into account."
  (time-to-days (time-add time (- beeminder-when-the-day-ends))))

(defun beeminder-human-time (time)
  "Convert TIME (which is set in the future) to a human-friendly
format:
- for today, the time;
- for tomorrow, the string \"tom\" (by default) and the time;
- for times within a week, abbreviation of the weekday or a plus and
  a number of days (depending on BEEMINDER-HUMAN-TIME-USE-WEEKDAY) and
  the time;
- for later times, iso date without time.
Midnight is treated as belonging to the previous day, not the following one."
  (let ((delta (- (beeminder-time-to-days time)
		  (beeminder-time-to-days (beeminder-current-time)))))
    (cond ((zerop delta) (format-time-string "     %R" time))
	  ((= 1 delta) (concat " " beeminder-tomorrow-code
			       (format-time-string " %R" time)))
	  ((<= delta 7) (concat (if beeminder-human-time-use-weekday
				    (format-time-string " %a" time)
				  (format "  +%d" delta))
				" "
				(format-time-string "%R" time)))
	  (t (format-time-string "%Y-%m-%d" time)))))

(defconst beeminder-lanes-to-faces-plist
  '(-2 beeminder-red -1 beeminder-yellow 1 beeminder-blue 2 beeminder-green)
  "Plist mapping the (normalized) value of lane to goal colors.")

(defun beeminder-normalize-lane (lane)
  "Normalize LANE, i.e., change LANE larger than 2 to 2 and
smaller than -2 to -2."
  (min (max lane -2) 2))

(defun beeminder-goal-pp (goal)
  "A pretty printer for Beeminder goals.  Prints a (currently fixed)
textual representation of a goal."
  (insert (propertize (format "%-12.12s %4.2d/%s %s %-16.16s $%.2f %s"
			      (cdr (assoc 'slug goal))
			      (cdr (assoc 'rate goal))
			      (cdr (assoc 'runits goal))
			      (beeminder-human-time (seconds-to-time
						     (1+ (cdr (assoc 'losedate goal)))))
			      (cdr (assoc 'limsum goal))
			      (cdr (assoc 'pledge goal))
			      (cdr (assoc 'title goal)))
		      'face (plist-get beeminder-lanes-to-faces-plist
				       (* (cdr (assoc 'yaw goal))
					  (beeminder-normalize-lane (cdr (assoc 'lane goal))))))))


;; Faces for goals

(defface beeminder-green '((t :foreground "#080"))
  "Face for displaying Beeminder goals in green.")

(defface beeminder-blue '((t :foreground "#008"))
  "Face for displaying Beeminder goals in blue.")

(defface beeminder-yellow '((t :foreground "#880"))
  "Face for displaying Beeminder goals in green.")

(defface beeminder-red '((t :foreground "#800"))
  "Face for displaying Beeminder goals in red.")


;; Beeminder EWOC

(defvar beeminder-goals-ewoc nil)

(defvar beeminder-sort-criterion "losedate"
  "Current goals sorting criterion.")

(defun beeminder-ewoc-header ()
  "Generate header for the Beeminder EWOC"
  (concat (format "Beeminder goals for user %s"
		  beeminder-username)
	  (propertize (concat (format " (sorted by %s"
				      beeminder-sort-criterion)
			      ")\n")
		      'face 'shadow)))

(defun beeminder-create-ewoc ()
  "Return a newly created EWOC for Beeminder goals."
  (ewoc-create #'beeminder-goal-pp
	       (beeminder-ewoc-header)""))

(defun beeminder-recreate-ewoc ()
  "Recreate Beeminder EWOC from the goal list."
  (ewoc-filter beeminder-goals-ewoc #'ignore)
  (seq-doseq (goal beeminder-goals)
    (ewoc-enter-last beeminder-goals-ewoc goal))
  (setq beeminder-sort-criterion "losedate")
  (ewoc-set-hf beeminder-goals-ewoc (beeminder-ewoc-header) "")
  (ewoc-refresh beeminder-goals-ewoc)
  (goto-char (point-min)))

(defun beeminder-list-goals ()
  "Switch to a buffer containing the list of Beeminder goals."
  (interactive)
  (switch-to-buffer "*Beeminder goals*")
  (buffer-disable-undo)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq beeminder-goals-ewoc (beeminder-create-ewoc))
    (beeminder-recreate-ewoc))
  (beeminder-mode))


;; Current time function

(defalias 'beeminder-current-time 'current-time
  "An alias for current-time, useful for testing/debugging.")


;; Sorting

(defvar beeminder-current-sorting-setting (list 'losedate #'< "losedate"))

(defun beeminder-sort-by-field (field predicate info)
  "Sort entries in beeminder-goals-ewoc by FIELD, using PREDICATE
to compare them and displaying INFO."
  (let ((current-goal-slug (cdr (assoc 'slug (ewoc-data (ewoc-locate beeminder-goals-ewoc))))))
    (ewoc-sort beeminder-goals-ewoc (lambda (x y) (funcall predicate
							   (cdr (assoc field x))
							   (cdr (assoc field y)))))
    (setq beeminder-sort-criterion info)
    (ewoc-refresh beeminder-goals-ewoc)
    (ewoc-set-hf beeminder-goals-ewoc (beeminder-ewoc-header) "")
    (ewoc-goto-node beeminder-goals-ewoc (ewoc-nth beeminder-goals-ewoc 0))
    (let ((current-node (ewoc-nth beeminder-goals-ewoc 0)))
      (while (and current-node
		  (not (string= (cdr (assoc 'slug (ewoc-data current-node)))
				current-goal-slug)))
	(ewoc-goto-next beeminder-goals-ewoc 1)
	(setq current-node (ewoc-next beeminder-goals-ewoc current-node))))
    (setq beeminder-current-sorting-setting (list field predicate info))))

(defun beeminder-sort-by-losedate ()
  "Sort entries in beeminder-goals by losedate."
  (interactive)
  (beeminder-sort-by-field 'losedate #'< "losedate"))

(defun beeminder-seconds-to-from-midnight (time)
  "Convert TIME to seconds from midnight.  If after 6:00, convert to
seconds to midnight (with a minus sign)."
  (let* ((decoded-time (decode-time time))
	 (seconds (+ (car decoded-time)
		     (* 60 (cadr decoded-time))
		     (* 3600 (caddr decoded-time)))))
    (if (> seconds (* 6 60 60))
	(- seconds (* 24 60 60))
      seconds)))

(defun beeminder-earlier-midnight (sec1 sec2 time)
  "Compare SEC1 and SEC2, as midnight times, taking into account
the TIME (expressed as the result of calling
`beeminder-seconds-to-from-midnight'.  If SEC1 < SEC2 < TIME,
return t.  If TIME < SEC1 < SEC2, return t.  If SEC2 < TIME <
SEC1, return t.  In all other cases, return nil."
  (or (< sec1 sec2 time)
      (< time sec1 sec2)
      (< sec2 time sec1)))

(defun beeminder-sort-by-midnight ()
  "Sort entries in beeminder-goals by their midnight, taking current time into consideration."
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


;; Refreshing goals
(defcustom beeminder-refresh-ask-for-download-if-after-losedate t
  "If t, ask for downloading the goal list from the server if the
earliest losedate already passed when refreshing.")

(defcustom beeminder-refresh-ask-for-download-interval (* 24 60 60)
  "If that many seconds passed after last downloading of the goal list
from the server, ask for downloading them again when refreshing.  If
0, never do it.")

(defvar beeminder-last-goal-download-time (seconds-to-time 0)
  "Time of the last downloading of goals from the server.")

(defun beeminder-refresh-goals-list (&optional get-goals)
  "Refresh goals list, applying the last sort.  It works by
recreating the EWOC from the goal list.  If called with a prefix
argument, reload the goals from the server."
  (interactive "P")
  (when (or get-goals
	    (and
	     (or (and
		  (not (zerop beeminder-refresh-ask-for-download-interval))
		  (>= (time-to-seconds (beeminder-current-time))
		      (+ (time-to-seconds beeminder-last-goal-download-time)
			 beeminder-refresh-ask-for-download-interval)))
		 (and beeminder-refresh-ask-for-download-if-after-losedate
		      (> (time-to-seconds (beeminder-current-time))
			 (cdr (assoc 'losedate (elt beeminder-goals 0))))))
	     (y-or-n-p "Reload Beeminder goals from the server? ")))
    (message "Beeminder goals downloading...")
    (beeminder-get-goals)
    (message "Beeminder goals downloading...  Done.")
    (setf beeminder-last-goal-download-time (beeminder-current-time)))
  (beeminder-recreate-ewoc)
  (apply #'beeminder-sort-by-field beeminder-current-sorting-setting))

(define-key beeminder-mode-map "g" #'beeminder-refresh-goals-list)


;; Filtering goals

(defun beeminder-kill-goal (goal)
  "Delete GOAL from `beeminder-goals-ewoc'."
  (interactive (list (ewoc-locate beeminder-goals-ewoc)))
  (let ((inhibit-read-only t)
	(next-goal (or (ewoc-next beeminder-goals-ewoc goal)
		       (ewoc-prev beeminder-goals-ewoc goal))))
    (ewoc-delete beeminder-goals-ewoc goal)
    (ewoc-refresh beeminder-goals-ewoc)
    (if next-goal
	(ewoc-goto-node beeminder-goals-ewoc next-goal)
      (goto-char (point-min)))))

(define-key beeminder-mode-map (kbd "C-k") #'beeminder-kill-goal)


;; slug: string (12)
;; title: string (12)
;; rate: float (4)
;; losedate: timestamp (10)
;; pledge: float (6)
;; runits: string (one character) (1)
;; limsum: string (what remains to do) (16)

