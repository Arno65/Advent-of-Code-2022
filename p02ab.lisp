;;;;    Advent of Code 2022 - Day 2 task A & B
;;;;    Solutions in Common Lisp
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The total score for strategy 1:  9759
;;;;    The total score for strategy 2: 12429
;;;;
;;;;    (cl) by Arno Jacobs, 2022-12-06
;;;;    

;;;; First - read the data-set
(defun read-data (&optional (file "data/inputDay02_2022.txt"))
    (with-open-file (data file)
        (let (*read-eval*)   
            (loop   :for     line = (read-line data nil nil)
                    :while   line
                    :collect line))))
 
;;; The data is 'stored' in *day1*
(defvar *DAY2* (read-data))

;;; Lisp like patern matching for strategy part 1
(defun rps-s1 (one-round)
    (cond   ((string-equal one-round "A X") 4)
            ((string-equal one-round "A Y") 8)
            ((string-equal one-round "A Z") 3)
            ((string-equal one-round "B X") 1)
            ((string-equal one-round "B Y") 5)
            ((string-equal one-round "B Z") 9)
            ((string-equal one-round "C X") 7)
            ((string-equal one-round "C Y") 2)
            ((string-equal one-round "C Z") 6)))
            
;;; Lisp like patern matching for strategy part 2
(defun rps-s2 (one-round)
    (cond   ((string-equal one-round "A X") 3)
            ((string-equal one-round "A Y") 4)
            ((string-equal one-round "A Z") 8)
            ((string-equal one-round "B X") 1)
            ((string-equal one-round "B Y") 5)
            ((string-equal one-round "B Z") 9)
            ((string-equal one-round "C X") 2)
            ((string-equal one-round "C Y") 6)
            ((string-equal one-round "C Z") 7)))
            
;;; Rock-Paper-Scissors ~ select strategy ~ round by round
(defun rps (strategy one-round)
    (if (= strategy 1)
        (rps-s1 one-round)
        (rps-s2 one-round)))

;; Play the game and count ther scores
(defun work-strategy (strategy score game-data)
    (if (= (length game-data) 0)
        score
        (work-strategy  strategy 
                        (+ score (rps strategy (car game-data)))
                        (cdr game-data))))

;;; The main program
(progn 
    (princ "Advent of Code 2022 - day 2  (Lisp)")
    (princ #\newline)
    (princ "The total score for strategy 1:  ")
    (princ (work-strategy 1 0 *day2*))
    (princ #\newline)
    (princ "The total score for strategy 2: ")
    (princ (work-strategy 2 0 *day2*))
    (princ #\newline)
    (princ "0K.")
    (princ #\newline)
    (princ #\newline)
)
