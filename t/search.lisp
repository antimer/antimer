(in-package :cl-user)
(defpackage antimer-test.search
  (:use :cl :fiveam)
  (:export :search))
(in-package :antimer-test.search)

(def-suite search
  :description "Tests of the information retrieval system.")
(in-suite search)

(defparameter +test-words+
  '("caresses"
    "flies"
    "dies"
    "mules"
    "denied"
    "died"
    "agreed"
    "owned"
    "humbled"
    "sized"
    "meeting"
    "stating"
    "siezing"
    "itemization"
    "sensational"
    "traditional"
    "reference"
    "colonizer"
    "plotted"))

(defparameter r1
  (list 3
        5
        4
        3
        3
        4
        2
        2
        3
        3
        4
        4
        4
        2
        3
        4
        3
        3
        4))

(defparameter r2
  (list 5
        5
        4
        5
        6
        4
        6
        5
        7
        5
        6
        6
        6
        4
        6
        6
        5
        5
        7))

(defparameter ewss
  (list t
        nil
        nil
        t
        nil
        nil
        nil
        t
        t
        t
        nil
        nil
        nil
        nil
        t
        t
        nil
        t
        t))

(defparameter step-1a
  (list "caress"
        "fli"
        "die"
        "mule"
        "deni"
        "die"
        "agreed"
        "owned"
        "humbled"
        "sized"
        "meeting"
        "stating"
        "siezing"
        "itemization"
        "sensational"
        "traditional"
        "reference"
        "colonizer"
        "plotted"))

(defparameter steps-1a-1b
  (list "caress"
        "fli"
        "die"
        "mule"
        "deni"
        "die"
        "agree"
        "own"
        "humble"
        "size"
        "meet"
        "state"
        "siez"
        "itemization"
        "sensational"
        "traditional"
        "reference"
        "colonizer"
        "plot"))

(defparameter steps-1a-1b-1c
  (list "caress"
        "fli"
        "die"
        "mule"
        "deni"
        "die"
        "agree"
        "own"
        "humble"
        "size"
        "meet"
        "state"
        "siez"
        "itemization"
        "sensational"
        "traditional"
        "reference"
        "colonizer"
        "plot"))

(defparameter stem
  (list "caress"
        "fli"
        "die"
        "mule"
        "deni"
        "die"
        "agre"
        "own"
        "humbl"
        "size"
        "meet"
        "state"
        "siez"
        "item"
        "sensat"
        "tradit"
        "refer"
        "colon"
        "plot"))

(test porter2
  (flet ((test-list (function list)
           (loop for (word . known-value) in (mapcar #'cons +test-words+ list) do
             (is
              (equal (funcall function word) known-value)))))
    (test-list #'antimer.search.porter2::get-r1 r1)
    (test-list #'antimer.search.porter2::get-r2 r2)
    (test-list #'antimer.search.porter2::ends-with-short-syllable-p ewss)
    (test-list #'antimer.search.porter2::step-1a step-1a)
    (test-list #'(lambda (word)
                   (antimer.search.porter2::step-1b
                    (antimer.search.porter2::step-1a word)
                    (antimer.search.porter2::get-r1 word)))
               steps-1a-1b)
    (test-list #'(lambda (word)
                   (antimer.search.porter2::step-1c
                    (antimer.search.porter2::step-1b
                     (antimer.search.porter2::step-1a word)
                     (antimer.search.porter2::get-r1 word))))
               steps-1a-1b-1c)
    (test-list #'antimer.search.porter2:stem stem)
    (let ((path (asdf:system-relative-pathname :antimer-search
                                               #p"t/porter2.txt")))
      (mapcar #'(lambda (line)
                  (unless (uiop:emptyp line)
                    (destructuring-bind (word known-stem)
                        (split-sequence:split-sequence #\Space
                                                       line
                                                       :remove-empty-subseqs t)
                      (is
                       (equal
                        (antimer.search.porter2:stem word)
                        known-stem)))))
              (split-sequence:split-sequence #\Newline
                                             (uiop:read-file-string path))))))
