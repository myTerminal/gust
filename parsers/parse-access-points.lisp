#!/usr/bin/sbcl --script

(defun string-to-list (input-string char)
  "Converts a string containing NewLine characters into a list of strings."
  (let* ((temp-chars '())
         (items '()))
    (labels ((to-string (chars)
               (coerce (reverse chars) 'string))
             (collect-item ()
               (push (to-string temp-chars) items)
               (setf temp-chars '())))
      (mapc (lambda (c)
              (cond ((eql c char) (collect-item))
                    (t (push c temp-chars))))
            (coerce input-string 'list))
      (collect-item)
      (reverse items))))

(let* ((mode (cadr *posix-argv*))
       (arg (caddr *posix-argv*))
       (lines (string-to-list (cadddr *posix-argv*) #\Newline))
       (relevant-lines (subseq lines 2
                               (position t
                                         (mapcar (lambda (x)
                                                   (if (and (> (length x) 8)
                                                            (string-equal (subseq x 0 7) "Station"))
                                                       t))
                                                 lines))))
       (filtered-lines (remove-if (lambda (x) ; Removes access points with blank names
                                    (let ((value (nth 13 (string-to-list x #\,))))
                                      (or (string-equal " " value)
                                          (null value))))
                                  relevant-lines)))
  (princ (cond ((equal mode "0") (reduce (lambda (a b)
                                           (concatenate 'string a "
" b))
                                         (mapcar (lambda (x)
                                                   (string-trim '(#\Space) (nth 13 (string-to-list x #\,))))
                                                 filtered-lines)))
               ((equal mode "1") (first (string-to-list (nth (parse-integer arg) filtered-lines) #\,)))
               ((equal mode "2") (fourth (string-to-list (nth (parse-integer arg) filtered-lines) #\,))))))
(princ #\Newline)
