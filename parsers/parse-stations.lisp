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

(let* ((lines (string-to-list (cadr *posix-argv*) #\Newline))
       (relevant-lines (subseq lines
                               (1+ (position t
                                             (mapcar (lambda (x)
                                                       (if (and (> (length x) 8)
                                                                (string-equal (subseq x 0 7) "Station"))
                                                           t))
                                                     lines)))))
       (filtered-lines (remove-if (lambda (x) ; Removes entries with blank (or almost blank) names
                                    (<= (length x) 1))
                                  relevant-lines))
       (generated-list (mapcar (lambda (x)
                                 (string-trim '(#\Space) (car (string-to-list x #\,))))
                               filtered-lines)))
  (princ (reduce (lambda (a b)
                   (concatenate 'string a "
" b))
                 generated-list)))
(princ #\Newline)
