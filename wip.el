(defun deadgrep--user-options (search-type case context)
  (let (options)
    (cond
     ((eq search-type 'string)
      (push "--fixed-strings" options))
     ((eq search-type 'words)
      (push "--fixed-strings" options)
      (push "--word-regexp" options))
     ((eq search-type 'regexp))
     (t
      (error "Unknown search type: %s" search-type)))
    (cond
     ((eq case 'smart)
      (push "--smart-case" options))
     ((eq case 'sensitive)
      (push "--case-sensitive" options))
     ((eq case 'ignore)
      (push "--ignore-case" options))
     (t
      (error "Unknown case: %s" case)))

    ;; TODO: pass this as an argument.
    (cond
     ((eq deadgrep--file-type 'all))
     ((eq (car-safe deadgrep--file-type) 'type)
      (push (format "--type=%s" (cdr deadgrep--file-type)) options))
     ((eq (car-safe deadgrep--file-type) 'glob)
      (push
       (format "--type-add='custom:%s' --type=custom"
               (cdr deadgrep--file-type))
       options))
     (t
      (error "Unknown file-type: %S" deadgrep--file-type)))
    (when context
      (when (> (car context) 0)
        (push (format "--before-context=%s" (car context)) options))
      (when (> (cdr context) 0)
        (push (format "--after-context=%s" (cdr context)) options)))
    options))
