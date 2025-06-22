;; -*- lexical-binding: t -*-

(defun reed-style--parse-compact-length (str)
  "Parse STR into (number . flag) where flag is t if percentage, nil otherwise.
Returns nil if STR is not a valid number."
  (when (and str (stringp str))
    (let ((trimmed (string-trim str)))
      (cond
       ;; Empty string
       ((string-empty-p trimmed) nil)

       ;; Ends with %
       ((string-suffix-p "%" trimmed)
        (let ((num-str (substring trimmed 0 -1)))
          (when (and (not (string-empty-p num-str))
                     (string-match-p "^[+-]?[0-9]*\\.?[0-9]+$" num-str))
            (let ((num (string-to-number num-str)))
              (cons (/ num 100.0) '%)))))

       ;; Regular number
       ((string-suffix-p "pt" trimmed)
        (let ((num-str (substring trimmed 0 -2)))
          (when (and (not (string-empty-p num-str))
                     (string-match-p "^[+-]?[0-9]*\\.?[0-9]+$" num-str))
            (let ((num (string-to-number num-str)))
              (cons num 'pt)))))

       ((string-suffix-p "fr" trimmed)
        (let ((num-str (substring trimmed 0 -2)))
          (when (and (not (string-empty-p num-str))
                     (string-match-p "^[+-]?[0-9]*\\.?[0-9]+$" num-str))
            (let ((num (string-to-number num-str)))
              (cons num 'fr)))))

       ;; Invalid
       (t nil)))))


(defun reed-style--process-style (style)
  (if (consp (car style))
      `(list
        ,@(mapcar
           (lambda (field)
             (let ((key (car field))
                   (value (cdr field)))
               (cond
                ((consp key) (reed-style--process-style field))
                ((eq key 'quote) field)
                (t `(cons
                     ',key
                     ,(cond
                       ((eq value 'AUTO) (string-to-number (reed-taffy-length 'auto 0.0)))
                       ((eq value 'ZERO) (string-to-number (reed-taffy-length 'zero 0.0)))
                       ((symbolp value)
                        (let ((len (reed-style--parse-compact-length (symbol-name value))))
                          (if (not len)
                              value
                            (string-to-number
                             (cond
                              ((eq (cdr len) '%)
                               (reed-taffy-length 'percent (car len)))
                              ((eq (cdr len) 'fr)
                               (reed-taffy-length 'fr (car len)))
                              (t
                               (reed-taffy-length 'length (car len))))))))
                       ((consp value)
                        (if (eq (car value) 'quote) value
                          (reed-style--process-style value)))
                       (t value))))))
             ) style))
    style))

(defmacro style! (&rest body)
  (reed-style--process-style body))

(defmacro style!* (&rest body)
  (prin1-to-string (eval (reed-style--process-style body))))

(provide 'reed-style)
