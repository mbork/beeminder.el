;; This will hopefully become an Emacs client for Beeminder some day

(require 'json)
(require 'request)
(require 'cl)
(require 'ewoc)
(require 'seq)

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

(defun beeminder-create-api-url (string)
  "Prepend the Beeminder site address and the username to the
given STRING.  STRING should begin with a slash."
  (concat beeminder-api-url beeminder-username string))

(defun beeminder-request-get (req &optional timeout)
  "Send a GET request to beeminder.com, adding the necessary
details (including the username and the auth token)."
  (request-response-data
   (request (concat (beeminder-create-api-url req) "?auth_token=" beeminder-auth-token)
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

(defun beeminder-get-goals ()
  "Get all the user's Beeminder goals.  The request returns
a vector of sexps - each sexp describes one goal."
  (setq beeminder-goals (beeminder-request-get "/goals.json")))

(defun beeminder-refresh-goal (slug)
  "Refresh autodata and graph."
  (beeminder-request-get (concat "/goals/" slug "/refresh_graph.json")))

(defun beeminder-submit-datapoint (slug amount &optional comment)
  "Submit a datapoint to beeminder.com."
  (beeminder-request-post (format "/goals/%s/datapoints.json" slug)
			  (concat (format "auth_token=%s&value=%f&comment=%s"
					  beeminder-auth-token
					  amount
					  (or comment "entered+by+beeminder.el")))))

(defvar beeminder-human-time-use-weekday t
  "Whether BEEMINDER-HUMAN-TIME uses weekdays or number of days from
today for times within a week from now.")

(defvar beeminder-tomorrow-code "tom"
  "The abbreviation for \"tomorrow\".")

(defun beeminder-human-time (time)
  "Convert TIME (which is set in the future) to a human-friendly
format:
- for today, the time;
- for tomorrow, the string \"tom\" (by default) and the time;
- for times within a week, abbreviation of the weekday or a plus and
  a number of days (depending on BEEMINDER-HUMAN-TIME-USE-WEEKDAY) and
  the time;
- for later times, iso date without time."
  (let ((delta (- (time-to-days time) (time-to-days (current-time)))))
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
  '(-2 'beeminder-red -1 'beeminder-yellow 1 'beeminder-blue 2 'beeminder-green)
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
						     (cdr (assoc 'losedate goal))))
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
  "Face for disppaying Beeminder goals in blue.")

(defface beeminder-yellow '((t :foreground "#880"))
  "Face for displaying Beeminder goals in green.")

(defface beeminder-red '((t :foreground "#800"))
  "Face for displaying Beeminder goals in red.")

;; EWOC
(defvar beeminder-goals-ewoc nil)

(defun beeminder-create-ewoc ()
  "Return a newly created EWOC for Beeminder goals."
  (ewoc-create #'beeminder-goal-pp
	       (format "Beeminder goals for user %s\n" beeminder-username) ""))

;; (defun ewoc-enter-sorted (node ewoc predicate)
;;   "Insert NODE into EWOC at the right position, using PREDICATE as
;; a sorting function."
;;   (let (previous (current (ewoc-nth ewoc 0)))
;;     (if (or (null current)
;; 	    (funcall predicate node (ewoc-data current)))
;; 	(ewoc-enter-first ewoc node)
;;       (while (not (funcall predicate node (ewoc-data current)))
;; 	(setq previous current
;; 	      current (ewoc-next ewoc current)))
;;       (ewoc-enter-after ewoc previous node))))

;; (defun ewoc-sort (ewoc predicate)
;;   "Sort an EWOC, using PREDICATE as a comparing function.  PREDICATE,
;; when called with two data nodes from EWOC, should return non-nil iff
;; they are in the right order.  Works by creating a second, temporary
;; EWOC and inserting nodes to it one by one in the right positions."
;;   (let ((ewoc-temp (ewoc-create #'insert "header!" "footer!")))
;;     (ewoc-map #'ewoc-enter-sorted ewoc ewoc-temp predicate)
;;     (setq ewoc ewoc-temp)))

(defun beeminder-goal-list ()
  "Switch to a buffer containing the list of Beeminder goals."
  (interactive)
  (switch-to-buffer "*Beemider goals*")
  (buffer-disable-undo)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq beeminder-goals-ewoc (beeminder-create-ewoc))
    (seq-doseq (goal beeminder-goals)
      (ewoc-enter-last beeminder-goals-ewoc goal))
    (ewoc-refresh beeminder-goals-ewoc))
  (goto-char (point-min))
  (beeminder-mode)
  ;; (ewoc-nth beeminder-goals-ewoc 0)
  ;; (seq-doseq (goal beeminder-goals)
  ;;   (beeminder-goal-pp goal))
  )

;; Beeminder mode
(define-derived-mode beeminder-mode special-mode "Beeminder"
  "A major mode for a buffer with Beeminder goal list.")


;; slug: string (12)
;; title: string (12)
;; rate: float (4)
;; losedate: timestamp (10)
;; pledge: float (6)
;; runits: string (one character) (1)
;; limsum: string (what remains to do) (16)

;; Beeminder mode

;; (define-derived-mode beeminder-mode special-mode "Beeminder"
;;   "A mode for the (unofficial) Emacs Beeminder client.
 
;; TODO: fill in the docs"
;;   ;;  TODO: mode definitions?
;;   )

