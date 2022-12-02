;;;;    Advent of Code 2022 - Day 1 task A & B
;;;;    Solutions in Lisp
;;;;    Also solving in other languages like Haskell
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The Elf carrying the most Calories, has a total of:  70116
;;;;    The three Elves carrying the most Calories, have a total of: 206582
;;;;
;;;;    (cl) by Arno Jacobs, 2022-12-02
;;;;    

;;;; "Save" string to integer conversion
(defun string-to-int (snumber)
    (if (= (length snumber) 0)
        0
        (parse-integer snumber)))

;;;; First - read the data-set
(defun read-data (&optional (file "data/inputDay01_2022.txt"))
    "Returns the numerical data in FILE as a list."
    (with-open-file (data file)
        (let (*read-eval*)   
            (loop   :for     line = (read-line data nil nil)
                    :while   line
                    :collect (string-to-int line )))))
 
;;; Some constants
(defvar *DAY1* (read-data))

;;; Select the set of calories belonging to one Elf
(defun take-calories (numbers)
    (loop for n in numbers
        while ((lambda (x) (> x 0)) n)
        collect n))

;;; Drop the 'taken' set and select the remaining set of calories
(defun drop-calories (numbers)
    (if (= (length numbers) 0)
        '()
        (if (= 0 (car numbers)) 
            (cdr numbers)
            (drop-calories (cdr numbers)))))

;;; Total the calories perElf
(defun calories (numbers)
    (setq *nl* (drop-calories numbers))
    (if (= (length numbers) 0)
        '()
        (cons 
            (reduce '+ (take-calories numbers))
            (if (= (length *nl*) 0)
                '()
                (calories *nl*)))))

;;; Return the maximum number from the list
(defun max-list (numbers)
    (reduce #'max numbers))
    
;;; Remove the element from the list numbers, 
;;; but only the first in the list
(defun remove-from-list (element numbers)
    (setq *head* (car numbers))
    (if (= (length numbers) 0)
        '()
        (if (= element *head*)
            (cdr numbers)
            (cons *head* (remove-from-list element (cdr numbers))))))

;;; More generic solution is to sort the calories list in ascending order
;;; Then sum the first n (in this case 3) of the list
;;; Here - just pick the 3 top values with 
;;; 'max-list' and 'remove-from-list' functions.
(defun sum-top-3 (numbers)
    (setq *top1*  (max-list numbers))
    (setq *rest1* (remove-from-list *top1* numbers))
    (setq *top2*  (max-list *rest1*))
    (setq *rest2* (remove-from-list *top2* *rest1*))
    (setq *top3*  (max-list *rest2*))
    (+ *top1* *top2* *top3*))

;;; The main program
(progn 
    (princ "Advent of Code 2022 - day 1  (Lisp)")
    (princ #\newline)
    (princ "The Elf carrying the most Calories,         has  a total of:  ")
    (princ (max-list (calories *day1*)))
    (princ #\newline)
    (princ "The three Elves carrying the most Calories, have a total of: ")
    (princ (sum-top-3 (calories *day1*)))
    (princ #\newline)
    (princ "0K.")
    (princ #\newline)
    (princ #\newline)
)
