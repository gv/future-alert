;; -*- lexical-binding: t; lisp-indent-offset: 1 -*-
;; Capitalized names are private

;; Config
(defvar future-alert-branch "origin/master")
;; End of config


(defvar-local Future-alert-tooltip
 (format "Changes in '%s'" future-alert-branch))
(defun Future-alert-style (s)
 (propertize s 'help-echo Future-alert-tooltip
  'face '(:background "#FFE0E0")))
(defvar-local Future-alert-progress-str
 (Future-alert-style "... "))
;; Put this here to allow properties to mode line text
(put 'Future-alert-progress-str 'risky-local-variable t)
(defvar-local Future-alert-insert-count 0)
(defvar-local Future-alert-delete-count 0)

(defmacro Future-alert-filter-lines (process b pattern_ &rest body)
 `(let ((ac "") (pattern ,pattern_) consumed)
  (set-process-filter ,process
   (lambda (proc str)
	(vg-message "Read %d" (length str))
    (if (not (buffer-live-p ,b))
     (progn
      (message "Buffer '%s' gone, killing process '%s'" b proc)
      (delete-process proc))
     (setq ac (concat ac str))
     (while (string-match pattern ac)
      (setq consumed (match-end 0))
      ,@body
      (setq ac (substring ac consumed))))))))

(defun Future-alert-spawn-local (name &rest cmd)
 (message "Running %s" cmd)
 ;; In case someone changed it from default value...
 (let ((default-directory (file-name-directory (buffer-file-name))))
  (set (make-local-variable name)
   (apply 'start-process (symbol-name name) nil cmd))))

(defun Future-alert-update-status ()
 (setq-local Future-alert-progress-str
  (Future-alert-style
   (if (and
		(not (eq 'run (process-status Future-alert-process)))
		(not (eq 0 (process-exit-status Future-alert-process))))
	"F"
	(format "+%d/-%d " Future-alert-insert-count
	 Future-alert-delete-count))))
 (force-mode-line-update))

(defun Future-alert-read-changes ()
 (let* ((take-off (float-time)) (b (current-buffer)) n)
  (Future-alert-spawn-local 'Future-alert-process
   "nice" "git" "diff" (format "@..%s" future-alert-branch) "--"
   (file-name-nondirectory (buffer-file-name)))
  (Future-alert-filter-lines Future-alert-process b
   "^\\(?1:\\(?2:[+].*\\)\\|\\(?3:-.*\\)\\|\\(?4:fatal.*\\)\\|\\(.+\\)\\)\n"
   (cond
	((setq n (match-string 2 ac))
	 (setq
	  Future-alert-insert-count (+ Future-alert-insert-count 1)))
	((setq n (match-string 3 ac))
	 (setq
	  Future-alert-delete-count (+ Future-alert-delete-count 1)))
	((setq n (match-string 4 ac))
	 (setq-local Future-alert-tooltip
	  (format "Error reading changes in '%s' = '%s'"
	   future-alert-branch n)))))
  (set-process-sentinel Future-alert-process
   (lambda (process event)
    (setq event (string-trim event))
;;	(if (string-match ("^exited abnormally" event))
;;	 (setq-local Future-alert-state event))
	(Future-alert-update-status)
    (message
     "event='%s' time=%dms" event (* 1000 (- (float-time) take-off)))))))

(define-minor-mode future-alert-mode
 "Warning of changes of the file in a branch"
 :lighter ""
 (if future-alert-mode
  (progn
   (setq-local Future-alert-insert-count 0)
   (setq-local Future-alert-delete-count 0)
   (add-to-list 'mode-line-misc-info
	'(future-alert-mode Future-alert-progress-str))
   (Future-alert-read-changes)))
 (force-mode-line-update))
					
