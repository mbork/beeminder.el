;; This will hopefully become an Emacs client for Beeminder some day

(require 'json)
(require 'request)
(require 'cl)
(require 'ewoc)
(require 'seq)


;; Utilities
(defun cdr-assoc (key list)
  "Composition of `cdr' and `assoc' of KEY and LIST."
  (cdr (assoc key list)))


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

(defun next-goal (count)
  "Move COUNT goals forward in the Beeminder buffer."
  (interactive "p")
  (ewoc-goto-next beeminder-goals-ewoc
		  (if (beeminder-before-first-goal-p)
		      (1- count)
		    count)))

(defun previous-goal (count)
  "Move COUNT goals back in the Beeminder buffer.  If on the
first goal, move to (point-min)."
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

(defun last-midnight (goal-deadline now)
  "Return a Unix timestamp of the last \"midnight\" for the given
GOAL-DEADLINE (as an offset from real midnight in seconds), assuming
that the current time is NOW."
  (let* ((now-decoded (decode-time now))
	 (last-real-midnight (encode-time 0 0 0
					  (cadddr now-decoded)
					  (nth 4 now-decoded)
					  (nth 5 now-decoded)
					  (nth 8 now-decoded)))
	 (last-midnight (+ goal-deadline
			   (time-to-seconds last-real-midnight))))
    (if (> last-midnight
	   (time-to-seconds now))
	(- last-midnight (* 24 60 60))
      last-midnight)))

(defun beeminder-get-goals ()
  "Get all the user's Beeminder goals.  The request returns
a list of sexps - each sexp describes one goal."
  (let ((goals (beeminder-request-get "/goals.json"))
	(today-values
	 (mapcar
	  (lambda (goal)
	    (let ((last-midnight (last-midnight (cdr-assoc 'deadline goal)
						(beeminder-current-time))))
	      (cons (cdr-assoc 'slug goal)
		    (cl-reduce #'+
			       (mapcar (lambda (datapoint)
					 (if (> (cdr-assoc 'timestamp datapoint)
						last-midnight)
					     (cdr-assoc 'value datapoint)
					   0))
				       (cdr-assoc 'datapoints goal))))))
	  (cdr-assoc 'goals (beeminder-request-get
			     (format ".json?diff_since=%d"
				     (- (time-to-seconds
					 (current-time))
					(* 24 60 60))))))))
    (setq beeminder-goals (mapcar
			   (lambda (goal) (cons
					   (cons 'donetoday
						 (cdr-assoc
						  (cdr-assoc 'slug goal)
						  today-values))
					   goal))
			   goals))))

(defun beeminder-refresh-goal (slug)
  "Refresh autodata and graph."
  (beeminder-request-get (concat "/goals/" slug "/refresh_graph.json")))


;; Submitting datapoints

(defun beeminder-before-first-goal-p ()
  "Return t is the point is before the first goal in the
Beeminder buffer."
  (< (point)
     (ewoc-location (ewoc-nth beeminder-goals-ewoc 0))))

(defun beeminder-get-slug (goal)
  "Return the slug of GOAL."
  (cdr-assoc 'slug goal))

(defun beeminder-slug-to-goal (slug)
  "Return the goal node corresponding to SLUG."
  (let ((goal-node (ewoc-nth beeminder-goals-ewoc 0)))
    (while (and goal-node
		(not
		 (string= slug (cdr-assoc 'slug (ewoc-data goal-node)))))
      (setq goal-node (ewoc-next beeminder-goals-ewoc goal-node)))
    goal-node))

(defvar beeminder-minibuffer-history nil
  "History of goal slugs entered through minibuffer.")

(defun current-or-read-goal ()
  "Return the goal node the point is on.  If the point is before
the first goal, use `completing-read' to ask for the goal slug
and return that goal node instead."
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
  "Return current time (or TIMESTAMP, given as Unix time) as
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
			    "via Emacs "
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
  (insert (propertize (format "%-12.12s %s %-16.16s %4.2d/%s $%.2f %s"
			      (cdr (assoc 'slug goal))
			      (beeminder-human-time (seconds-to-time
						     (1+ (cdr (assoc 'losedate goal)))))
			      (cdr (assoc 'limsum goal))
			      (cdr (assoc 'rate goal))
			      (cdr (assoc 'runits goal))
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

(defvar beeminder-short-header nil
  "If t, the default header is (extremely) shortened.")

(defun beeminder-toggle-short-header (&optional arg)
  "Toggle shortening the header for Beeminder goal list.  If ARG is
positive, shorten the header; otherwise, do not."
  (interactive "P")
  (setq beeminder-short-header
	(if (null arg)
	    (not beeminder-short-header)
	  (> (prefix-numeric-value arg) 0)))
  (save-current-goal
    (ewoc-set-hf beeminder-goals-ewoc (beeminder-ewoc-header) "")
    (ewoc-refresh beeminder-goals-ewoc)))

(define-key beeminder-mode-map (kbd "=") #'beeminder-toggle-short-header)

(defun plist-mapcar (function plist)
  "Apply FUNCTION (which must accept two arguments) to each pair
in PLIST, and make a list of the results.  This function won't be
needed when we scrap plists."
  (let (result)
    (while plist
      (push (funcall function (car plist) (cadr plist))
	    result)
      (setq plist (cddr plist)))
    (nreverse result)))

(defun plist-to-alist (plist)
  "This is a temporary function, needed only as long as
`beeminder-current-filters' is a plist and not an alist."
  (plist-mapcar #'cons plist))

(defun beeminder-ewoc-header ()
  "Generate header for the Beeminder EWOC"
  (concat (format "Beeminder goals for user %s\n"
		  beeminder-username)
	  (propertize (concat (format "sorting criterion: %s\n"
				      (caddr beeminder-current-sorting-setting))
			      (if beeminder-current-filters
				  (format "filter%s:%s\n"
					  (if (> (length beeminder-current-filters) 2) "s" "")
					  (let ((firstp t))
					    (concat
					     (awhen (plist-get beeminder-current-filters 'days)
					       (setq firstp nil)
					       (format " days to derailment (%d)" it))
					     (awhen (plist-get beeminder-current-filters 'donetoday)
					       (prog1
						   (format "%s done today (%d%%)"
							   (if firstp "" ",")
							   it)
						 (setq firstp nil)))
					     (awhen (plist-get beeminder-current-filters 'killed)
					       (format "%s individual goals killed (%d)"
						       (if firstp "" ",")
						       (length it))))))))
		      'face 'shadow)))

(defun beeminder-create-ewoc ()
  "Return a newly created EWOC for Beeminder goals."
  (ewoc-create #'beeminder-goal-pp
	       (beeminder-ewoc-header)""))

(defun beeminder-recreate-ewoc ()
  "Recreate Beeminder EWOC from the goal list and reapply
filtering and sorting settings."
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
  (beeminder-mode))


;; Current time function

(defalias 'beeminder-current-time 'current-time
  "An alias for current-time, useful for testing/debugging.")


;; Sorting

(defcustom beeminder-default-sorting-setting (list 'losedate #'< "losedate")
  "Default sorting setting for Beeminder goals.")

(defvar beeminder-current-sorting-setting beeminder-default-sorting-setting)

(defmacro save-current-goal (&rest body)
  "Evaluate BODY and bring the point back to the current goal."
  (declare (indent 0) (debug t))
  `(let ((current-goal-slug (cdr-assoc 'slug (ewoc-data (ewoc-locate beeminder-goals-ewoc)))))
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
  "Sort entries in beeminder-goals-ewoc by FIELD, using PREDICATE
to compare them and displaying INFO."
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
(defun beeminder-reload-goals-list (&optional get-goals)
  "Reload the goals from the server."
  (interactive "P")
  (save-current-goal
   (message "Beeminder goals downloading...")
   (beeminder-get-goals)
   (message "Beeminder goals downloading...  Done.")
   (beeminder-recreate-ewoc)))

(define-key beeminder-mode-map "g" #'beeminder-reload-goals-list)


;; Filtering goals

(defvar beeminder-current-filters '()
  "Plist of filters currently in effect.  This should really be an alist.")

(defun beeminder-clear-filters ()
  "Clear all filters."
  (interactive)
  (setq beeminder-current-filters '())
  (beeminder-recreate-ewoc))

(define-key beeminder-mode-map "c" #'beeminder-clear-filters)

(defcustom beeminder-default-filter-days 3
  "Defalt number of days used for filtering.  If the user doesn't
specify the number of days for filtering, all goals with more
than this amount of days left to losedate will be filtered out.")

(defcustom beeminder-default-filter-donetoday 100
  "Default percentage of donetoday used for filtering.")

(defun beeminder-days-p (goal days)
  "Return nil if time to derailment of GOAL is greater than
DAYS."
  (<= (- (beeminder-time-to-days (cdr-assoc 'losedate goal))
	 (beeminder-time-to-days (beeminder-current-time)))
      days))

(defun beeminder-donetoday-p (goal percentage)
  "Return nil if donetoday for GOAL is higher than the PERCENTAGE
of a day's amount."
  (< (/ (* 100 (cdr-assoc 'donetoday goal))
	(/ (cdr-assoc 'rate goal)
	   (cl-case (intern (cdr-assoc 'runits goal))
	     (y 365)
	     (m (/ 365 12))
	     (w 7)
	     (d 1)
	     (h (/ 1 24)))))
     percentage))

(defun beeminder-not-killed-p (goal kill-list)
  "Return nil if GOAL is in the KILL-LIST."
  (not (member (cdr-assoc 'slug goal) kill-list)))

(defvar beeminder-filters `((days ,#'beeminder-days-p
				 ,beeminder-default-filter-days
				 "days to derailment (%d)"
				 "d")
			    (donetoday ,#'beeminder-donetoday-p
				       ,beeminder-default-filter-donetoday
				       "done today (%d%%)"
				       "t")
			    (killed ,#'beeminder-not-killed-p
				    '()
				    (lambda (kill-list) (format "%d goal%s killed"
								(length kill-list)
								(if (= 1 (length kill-list))
								    "" "s")))
				    ""))

  "List of possible filters.  Each element is a list, consisting of:
- symbol, denoting the filter,
- predicate (with two arguments - the goal and the parameter),
- default value for the parameter,
- formatting function (with one argument - the parameter) or a format string (with one placeholder)
- key for enabling the filter (as a string, passed to `kbd').")

(defun beeminder-kill-goal (goal-node)
  "Delete GOAL-NODE from `beeminder-goals-ewoc'."
  (interactive (list (current-or-read-goal)))
  (let ((inhibit-read-only t)
	(next-goal (or (ewoc-next beeminder-goals-ewoc goal-node)
		       (ewoc-prev beeminder-goals-ewoc goal-node))))
    (ewoc-delete beeminder-goals-ewoc goal-node)
    (ewoc-refresh beeminder-goals-ewoc)
    (let ((killed (plist-get beeminder-current-filters 'killed)))
      (setq beeminder-current-filters (plist-put beeminder-current-filters 'killed
						 (cons (cdr-assoc 'slug (ewoc-data goal-node))
						       killed))))
    (ewoc-set-hf beeminder-goals-ewoc (beeminder-ewoc-header) "")
    (if next-goal
	(ewoc-goto-node beeminder-goals-ewoc next-goal)
      (goto-char (point-min)))))

(define-key beeminder-mode-map (kbd "C-k") #'beeminder-kill-goal)

(defun beeminder-clear-kills ()
  "Clear all individual kills."
  (interactive)
  (setq beeminder-current-filters
	(plist-clear (plist-put beeminder-current-filters 'killed nil)))
  (beeminder-recreate-ewoc))

(define-key beeminder-mode-map (kbd "C-w") #'beeminder-clear-kills)

(defun beeminder-apply-filter (filter parameter)
  "Apply FILTER (which should be a symbol), parametrized by PARAMETER.
This means deleting some goals from `beeminder-goals-ewoc'."
  (save-current-goal
    (ewoc-filter beeminder-goals-ewoc
		 (lambda (goal)
		   (funcall (cadr (assoc filter beeminder-filters))
			    goal parameter)))))

(defun plist-mapc (function plist)
  "Iterate FUNCTION (a two-argument function) over PLIST."
  (when plist
    (funcall function (car plist) (cadr plist))
    (plist-mapc function (cddr plist))))

(defun plist-clear (plist)
  "Return PLIST with all nil properties deleted."
  (cond
   ((null (cdr plist)) nil)
   ((null (cadr plist)) (cddr plist))
   (t (cons (car plist) (cons (cadr plist) (plist-clear (cddr plist)))))))

(defun beeminder-apply-filters ()
  "Apply filters from `beeminder-current-filters' in sequence."
  (plist-mapc #'beeminder-apply-filter beeminder-current-filters))

(defun beeminder-filter-command (parameter)
  "Enable the filter from `beeminder-filters' which should be
applied by the key pressed to run this very command.  PARAMETER,
when given, overrides the default."
  (interactive "P")
  (let ((filter beeminder-filters))
    (while (and filter (not (string= (char-to-string last-command-event) (nth 4 (car filter)))))
      (setq filter (cdr filter)))
    (if (not filter)
	(error "This shouldn't happen -- beeminder-filter-command)")
      (setq filter (car filter))
      (setq beeminder-current-filters
	    (plist-clear
	     (plist-put beeminder-current-filters
			(car filter)
			(cond
			 ((eq parameter '-) nil)
			 ((null parameter) (caddr filter))
			 (t (prefix-numeric-value parameter))))))
      (beeminder-recreate-ewoc))))

(define-key beeminder-mode-map (kbd "d") #'beeminder-filter-command)
(define-key beeminder-mode-map (kbd "t") #'beeminder-filter-command)

(provide 'beeminder)


;; slug: string (12)
;; title: string (12)
;; rate: float (4)
;; losedate: timestamp (10)
;; pledge: float (6)
;; runits: string (one character) (1)
;; limsum: string (what remains to do) (16)

