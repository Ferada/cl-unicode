(in-package #:cl-unicode)

(defun hangul-decompose (char)
  (let* ((s (char-code char))
         (s-index (- s +s-base+)))
    (ecase (hangul-syllable-type s)
      (cl-unicode-names::lv
       (let* ((l-index (floor s-index +n-count+))
              (v-index (floor (mod s-index +n-count+) +t-count+)))
         (coerce (list (code-char (+ +l-base+ l-index))
                       (code-char (+ +v-base+ v-index)))
                 'string)))
      (cl-unicode-names::lvt
       (let* ((l-index (floor s-index +n-count+))
              (v-index (floor (mod s-index +n-count+) +t-count+))
              (t-index (mod s-index +t-count+)))
         (coerce (list* (code-char (+ +l-base+ l-index))
                        (code-char (+ +v-base+ v-index))
                        (when (> t-index 0)
                          (list (code-char (+ +t-base+ t-index)))))
                 'string))))))

(defun nfd (string)
  (let ((decomposed
          (loop
            for i below (length string)
            appending (let* ((char (char string i)))
                        (if (hangul-syllable-p (char-code char))
                            (let ((mapping (hangul-decompose char)))
                              (or (or (and (not (compatibility-flags char))
                                           (coerce (nfd mapping) 'list))
                                  (list char))))
                            (let ((mapping (map 'string #'code-char (decomposition-mapping char))))
                              (or (and (not (compatibility-flags char))
                                       (coerce (nfd mapping) 'list))
                                  (list char))))))))
    (let (sorted buffer)
      (flet ((spill ()
               (setf sorted (append sorted (sort buffer #'<= :key #'combining-class)))
               (setf buffer NIL)))
        (loop
          for code in decomposed
          do (let ((ccc (combining-class code)))
               (cond
                 ((and (eql ccc 0) buffer)
                  (spill)))
               (push code buffer))
          finally (when buffer
                    (spill))))
      (coerce sorted 'string))))

(defun nfkd (string)
  (let ((decomposed
          (loop
            for i below (length string)
            appending (let* ((char (char string i)))
                        (if (hangul-syllable-p (char-code char))
                            (let ((mapping (hangul-decompose char)))
                              (or (or (coerce (nfkd mapping) 'list)
                                      (list char))))
                            (let ((mapping (map 'string #'code-char (decomposition-mapping char))))
                              (or (coerce (nfkd mapping) 'list)
                                  (list char))))))))
    (let (sorted buffer)
      (flet ((spill ()
               (setf sorted (append sorted (sort buffer #'<= :key #'combining-class)))
               (setf buffer NIL)))
        (loop
          for code in decomposed
          do (let ((ccc (combining-class code)))
               (cond
                 ((and (eql ccc 0) buffer)
                  (spill)))
               (push code buffer))
          finally (when buffer
                    (spill))))
      (coerce sorted 'string))))

(defun primary-composite-p (code-point-1 code-point-2)
  (when (and (<= #x1100 code-point-1 #x1112)
             (<= #x1161 code-point-2 #x1175))
    (let* ((l-index (- code-point-1 +l-base+))
           (v-index (- code-point-2 +v-base+))
           (lv-index (+ (* l-index +n-count+) (* v-index +t-count+))))
      (return-from primary-composite-p
        (+ +s-base+ lv-index))))
  (when (and (eq (hangul-syllable-type code-point-1) 'cl-unicode-names::lv)
             (<= #x11a8 code-point-2 #x11c2))
    (let ((t-index (- code-point-2 +t-base+)))
      (return-from primary-composite-p
        (+ code-point-1 t-index))))
  (cdr (assoc code-point-2 (gethash code-point-1 *primary-composites*))))

(defun canonical-compose (string)
  (let ((length (length string)))
    (when (< length 2)
      (return-from canonical-compose string))
    (let ((buffer (list (char-code (char string 0)))))
      (loop
        for i from 1 below length
        for c = (char-code (char string i))
        for c-ccc = (combining-class c)
        do (loop
             for buffered on buffer
             for car = (car buffered)
             for ccc = (combining-class car)
             for length from 0
             when (eql ccc 0)
             do (let ((primary (primary-composite-p car c)))
                  (when primary
                    (return (setf (car buffered) primary))))
             while (and (> c-ccc ccc) (not (eql ccc 0)))
             finally (push c buffer)))
      (coerce (nreverse (map-into buffer #'code-char buffer)) 'string))))

(defun nfc (string)
  (canonical-compose (nfd string)))

(defun nfkc (string)
  (canonical-compose (nfkd string)))

(defun normalization-test (source nfc nfd nfkc nfkd comment break-on-error)
  (let (error)
    (let ((computed (nfd source)))
      (unless (equal nfd computed)
        (format T "NFD failed on ~S, expected ~S, got ~S instead (~A).~%" (coerce source 'list) (coerce nfd 'list) (coerce computed 'list) comment)
        (setf error T)))
    (let ((computed (nfkd source)))
      (unless (equal nfkd computed)
        (format T "NFKD failed on ~S, expected ~S, got ~S instead (~A).~%" (coerce source 'list) (coerce nfkd 'list) (coerce computed 'list) comment)
        (setf error T)))
    (let ((computed (nfc source)))
      (unless (equal nfc computed)
        (format T "NFC failed on ~S, expected ~S, got ~S instead (~A).~%" (coerce source 'list) (coerce nfc 'list) (coerce computed 'list) comment)
        (setf error T)))
    (let ((computed (nfkc source)))
      (unless (equal nfkc computed)
        (format T "NFKC failed on ~S, expected ~S, got ~S instead (~A).~%" (coerce source 'list) (coerce nfkc 'list) (coerce computed 'list) comment)
        (setf error T)))
    (when (and break-on-error error)
      (break))))

(defun parse-code-points (code-points)
  (coerce 
   (mapcar (lambda (code-point)
             (code-char (parse-integer code-point :radix 16)))
           (cl-ppcre:split " " code-points))
   'string))

(defun normalization-test-suite (&optional break-on-error (pathname (asdf:system-relative-pathname '#:cl-unicode #P"build/data/NormalizationTest.txt")))
  (with-open-file (stream pathname :direction :input)
    (loop
      for line = (read-line stream NIL)
      while line
      do (case (char line 0)
           (#\#)
           (#\@ (format T "testing section ~A~%" line))
           (T (let ((columns (cl-ppcre:split ";" line)))
                (normalization-test
                 (parse-code-points (first columns))
                 (parse-code-points (second columns))
                 (parse-code-points (third columns))
                 (parse-code-points (fourth columns))
                 (parse-code-points (fifth columns))
                 (sixth columns)
                 break-on-error)))))))
