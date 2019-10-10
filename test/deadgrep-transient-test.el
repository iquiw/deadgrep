(require 'deadgrep-transient)

(defun deadgrep-transient-test-create-option (command &optional value)
  (let ((infix (transient-option :command command)))
    (when value (oset infix value value))
    infix))

(defun deadgrep-transient-test-create-switch (command &optional value)
  (let ((infix (transient-switch :command command)))
    (when value (oset infix value value))
    infix))

(defun deadgrep-transient-test-create-switches (command &optional value)
  (let ((infix (transient-switches :command command)))
    (when value (oset infix value value))
    infix))

(ert-deftest deadgrep-transient--set-options-with-default ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-option 'deadgrep-transient:--after-context)
      (deadgrep-transient-test-create-option 'deadgrep-transient:--before-context)
      (deadgrep-transient-test-create-switch 'deadgrep-transient:--fixed-strings)
      (deadgrep-transient-test-create-switch 'deadgrep-transient:--word-regexp)
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--*-case)))

    (should (null deadgrep--context))
    (should (equal deadgrep--search-type 'regexp))
    (should (equal deadgrep--search-case 'sensitive))))

(ert-deftest deadgrep-transient--set-options-with-context ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-option 'deadgrep-transient:--before-context "2")))
    (should (equal deadgrep--context '(2 . 0)))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-option 'deadgrep-transient:--after-context "1")))
    (should (equal deadgrep--context '(0 . 1)))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-option 'deadgrep-transient:--before-context "4")
      (deadgrep-transient-test-create-option 'deadgrep-transient:--after-context "3")))
    (should (equal deadgrep--context '(4 . 3)))))

(ert-deftest deadgrep-transient--set-options-with-type ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--fixed-strings
                                               "--fixed-strings")))
    (should (equal deadgrep--search-type 'string))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--fixed-strings
                                               "--fixed-strings")
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--word-regexp
                                               "--word-regexp")))
    (should (equal deadgrep--search-type 'words))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--word-regexp
                                               "--word-regexp")))
    ;; no equivalent value in `deadgrep--search-type'.
    (should (equal deadgrep--search-type 'regexp))))

(ert-deftest deadgrep-transient--set-options-with-case ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--*-case
                                               "--case-sensitive")))
    (should (equal deadgrep--search-case 'sensitive))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--*-case
                                               "--ignore-case")))
    (should (equal deadgrep--search-case 'ignore))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--*-case
                                               "--smart-case")))
    (should (equal deadgrep--search-case 'smart))))

(ert-deftest deadgrep-transient--set-options-with-glob ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--glob "*.el")))
    (should (equal deadgrep--file-type '(glob . "*.el")))))

(ert-deftest deadgrep-transient--set-options-with-type ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--type "elisp")))
    (should (equal deadgrep--file-type '(type . "elisp")))))
