(in-package :cl-user)
(defpackage antimer.search.porter2
  (:use :cl)
  (:shadowing-import-from :typed
                          :defun)
  (:import-from :alexandria
                :starts-with
                :starts-with-subseq
                :ends-with-subseq)
  (:import-from :ppcre
                :create-scanner)
  (:export :stem)
  (:documentation "An implementation of the Porter2 stemmer."))
(in-package :antimer.search.porter2)

;;; Regular expressions

(defparameter r (create-scanner "[^aeiouy]*[aeiouy]+[^aeiouy](\\w*)"))

(defparameter ewss1 (create-scanner "^[aeiouy][^aeiouy]$"))

(defparameter ewss2 (create-scanner ".*[^aeiouy][aeiouy][^aeiouywxY]$"))

(defparameter ccy (create-scanner "([aeiouy])y"))

(defparameter s1a (create-scanner "[aeiouy]."))

(defparameter s1b (create-scanner "[aeiouy]"))

;;; Utilities

(defmacro startsp (word prefix)
  `(starts-with-subseq ,prefix ,word))

(defmacro endsp (word suffix)
  `(ends-with-subseq ,suffix ,word))

(defmacro regex-search (regex string)
  `(if (ppcre:scan ,regex ,string)
       t
       nil))

(defmacro match (regex string &optional (pos 0))
  "Emulate Python's re.match."
  `(multiple-value-bind (start end start-positions end-positions)
       (ppcre:scan ,regex ,string :start ,pos)
     (declare (ignore start end end-positions))
     start-positions))

(defmacro match-start (match index)
  "Return the start position of the nth match."
  `(elt ,match ,(1- index)))

(defmacro butlast-nth (string negative-index)
  `(subseq ,string 0 (- (length ,string) ,negative-index)))

(defmacro cat (&rest strings)
  `(concatenate 'string ,@strings))

;;; Implementation

(defun get-r1 ((word string)) fixnum
  (cond
    ((or (startsp word "gener")
         (startsp word "arsen"))
     5)
    ((startsp word "commun")
     6)
    (t
     (let ((match (match r word)))
       (if match
           (match-start match 1)
           (length word))))))

(defun get-r2 ((word string)) fixnum
  (let ((match (match r word (get-r1 word))))
    (if match
        (match-start match 1)
        (length word))))

(defun ends-with-short-syllable-p ((word string)) boolean
  (cond
    ((= (length word) 2)
     (if (match ewss1 word)
         t
         nil))
    ((match ewss2 word)
     t)
    (t
     nil)))

(defun short-word-p ((word string)) boolean
  (if (ends-with-short-syllable-p word)
      (if (= (get-r1 word) (length word))
          t
          nil)
      nil))

(defun remove-initial-apostrophe ((word string)) string
  (if (starts-with #\' word)
      (subseq word 1)
      word))

(defun capitalize-consonant-ys ((word string)) string
  (if (starts-with #\y word)
      (concatenate 'string (string #\Y) (subseq word 1))
      (let ((new (ppcre:regex-replace-all ccy word "\\1Y")))
        new)))

(defun step-0 ((word string)) string
  (cond
    ((endsp word "'s'")
     (butlast-nth word 3))
    ((endsp word "'s")
     (butlast-nth word 2))
    ((endsp word "'")
     (butlast-nth word 1))
    (t
     word)))

(defun step-1a ((word string)) string
  (cond
    ((endsp word "sses")
     (concatenate 'string (butlast-nth word 4) "ss"))
    ((or (endsp word "ied")
         (endsp word "ies"))
     (if (> (length word) 4)
         (concatenate 'string (butlast-nth word 3) "i")
         (concatenate 'string (butlast-nth word 3) "ie")))
    ((or (endsp word "us")
         (endsp word "ss"))
     word)
    ((endsp word "s")
     (let ((preceding (butlast-nth word 1)))
       (if (regex-search s1a preceding)
           preceding
           word)))
    (t
     word)))

(defparameter +doubles+
  (vector "bb" "dd" "ff" "gg" "mm" "nn" "pp" "rr" "tt"))

(defun ends-with-double-p ((word string)) boolean
  (loop for double across +doubles+ do
    (if (endsp word double)
        (return-from ends-with-double-p t)))
  nil)

(defun step-1b-helper ((word string)) string
  (cond
    ((or (endsp word "at")
         (endsp word "bl")
         (endsp word "iz"))
     (cat word "e"))
    ((ends-with-double-p word)
     (butlast-nth word 1))
    ((short-word-p word)
     (cat word "e"))
    (t
     word)))

(defparameter +s1b-suffixes+
  (vector "ed" "edly" "ing" "ingly"))

(defun step-1b ((word string) (r1 fixnum)) string
  (cond
    ((endsp word "eedly")
     (if (>= (- (length word) 5) r1)
         (butlast-nth word 3)
         word))
    ((endsp word "eed")
     (if (>= (- (length word) 3) r1)
         (butlast-nth word 1)
         word))
    (t
     (loop for suffix across +s1b-suffixes+ do
       (when (endsp word suffix)
         (let ((preceding (butlast-nth word (length suffix))))
           (if (regex-search s1b preceding)
               (return-from step-1b (step-1b-helper preceding))
               (return-from step-1b word)))))
     word)))

(defun step-1c ((word string)) string
  (when (and (or (endsp word "y")
                 (endsp word "Y"))
             (> (length word) 1))
    (when (not (member (elt word (- (length word) 2))
                       (list #\a #\e #\i #\o #\u #\y)
                       :test #'char=))
      (if (> (length word) 2)
          (return-from step-1c
            (cat (butlast-nth word 1) "i")))))
  word)

(defun step-2-helper ((word string) (r1 fixnum) (end string)
                      (repl string) (prev list)) (or string null)
  (when (endsp word end)
    (when (>= (- (length word) (length end)) r1)
      (let ((subword (butlast-nth word (length end))))
        (when (null prev)
          (return-from step-2-helper (cat subword repl)))
        (loop for p in prev do
          (when (endsp subword p)
            (return-from step-2-helper (cat subword repl))))))
    word)
  nil)

(defparameter +s2-triples+
  '(("ization" "ize" ())
    ("ational" "ate" ())
    ("fulness" "ful" ())
    ("ousness" "ous" ())
    ("iveness" "ive" ())
    ("tional" "tion" ())
    ("biliti" "ble" ())
    ("lessli" "less" ())
    ("entli" "ent" ())
    ("ation" "ate" ())
    ("alism" "al" ())
    ("aliti" "al" ())
    ("ousli" "ous" ())
    ("iviti" "ive" ())
    ("fulli" "ful" ())
    ("enci" "ence" ())
    ("anci" "ance" ())
    ("abli" "able" ())
    ("izer" "ize" ())
    ("ator" "ate" ())
    ("alli" "al" ())
    ("bli" "ble" ())
    ("ogi" "og" ("l"))
    ("li" "" ("c" "d" "e" "g" "h" "k" "m" "n" "r" "t"))))

(defun step-2 ((word string) (r1 fixnum)) string
  (loop for triple in +s2-triples+ do
    (destructuring-bind (end repl prev)
        triple
      (let ((attempt (step-2-helper word r1 end repl prev)))
        (when attempt
          (return-from step-2 attempt)))))
  word)

(defun step-3-helper ((word string) (r1 fixnum) (r2 fixnum)
                      (end string) (repl string) (r2-necessary boolean))
  (or string null)
  (when (endsp word end)
    (when (>= (- (length word) (length end)) r1)
      (let ((subword (butlast-nth word (length end))))
        (if (not r2-necessary)
            (return-from step-3-helper (cat subword repl))
            (when (>= (- (length word) (length end)) r2)
              (return-from step-3-helper (cat subword repl))))))
    word)
  nil)

(defparameter +s3-triples+
  '(("ational" "ate" nil)
    ("tional" "tion" nil)
    ("alize" "al" nil)
    ("icate" "ic" nil)
    ("iciti" "ic" nil)
    ("ative" "" t)
    ("ical" "ic" nil)
    ("ness" "" nil)
    ("ful" "" nil)))

(defun step-3 ((word string) (r1 fixnum) (r2 fixnum)) string
  (loop for triple in +s3-triples+ do
    (destructuring-bind (end repl necessary)
        triple
      (let ((attempt (step-3-helper word r1 r2 end repl necessary)))
        (when attempt
          (return-from step-3 attempt)))))
  word)

(defparameter +s4-delete-list+
  '("al" "ance" "ence" "er" "ic" "able" "ible" "ant" "ement"
    "ment" "ent" "ism" "ate" "iti" "ous" "ive" "ize"))

(defun step-4 ((word string) (r2 fixnum)) string
  (loop for end in +s4-delete-list+ do
    (when (endsp word end)
      (when (>= (- (length word) (length end)) r2)
        (return-from step-4 (butlast-nth word (length end))))
      (return-from step-4 word)))

  (when (or (endsp word "sion")
            (endsp word "tion"))
    (when (>= (- (length word) 3) r2)
      (return-from step-4 (butlast-nth word 3))))

  word)

(defun step-5 ((word string) (r1 fixnum) (r2 fixnum)) string
  (cond
    ((endsp word "l")
     (if (and (>= (1- (length word)) r2)
              (char= (elt word (- (length word) 2)) #\l))
         (butlast-nth word 1)
         word))
    ((endsp word "e")
     (cond
       ((>= (1- (length word)) r2)
        (butlast-nth word 1))
       ((and (>= (1- (length word)) r1)
             (not (ends-with-short-syllable-p (butlast-nth word 1))))
        (butlast-nth word 1))
       (t
        word)))
    (t
     word)))

(defun normalize-ys ((word string)) string
  (substitute #\y #\Y word))

(defparameter +exceptional-forms+
  '(("skis" "ski")
    ("skies" "sky")
    ("dying" "die")
    ("lying" "lie")
    ("tying" "tie")
    ("idly" "idl")
    ("gently" "gentl")
    ("ugly" "ugli")
    ("early" "earli")
    ("only" "onli")
    ("singly" "singl")
    ("sky" "sky")
    ("news" "news")
    ("howe" "howe")
    ("atlas" "atlas")
    ("cosmos" "cosmos")
    ("bias" "bias")
    ("andes" "andes")))

(defparameter +other-exceptional-forms+
  '("inning"
    "outing"
    "canning"
    "herring"
    "earring"
    "proceed"
    "exceed"
    "succeed"))

(defun stem ((word string)) string
  (when (<= (length word) 2)
    (return-from stem word))
  (let ((word (remove-initial-apostrophe word)))
    ;; Handle exceptional situations
    (let ((pair (assoc word +exceptional-forms+ :test #'string=)))
      (when pair
        (return-from stem (cadr pair))))

    (setf word (capitalize-consonant-ys word))
    (let ((r1 (get-r1 word))
          (r2 (get-r2 word)))
      (setf word (step-0 word))
      (setf word (step-1a word))

      ;; Handle more exceptional forms
      (when (member word +other-exceptional-forms+ :test #'string=)
        (return-from stem word))

      (setf word (step-1b word r1))

      (setf word (step-1c word))

      (setf word (step-2 word r1))

      (setf word (step-3 word r1 r2))

      (setf word (step-4 word r2))

      (setf word (step-5 word r1 r2))

      (setf word (normalize-ys word))

      word)))
