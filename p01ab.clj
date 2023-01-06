;;;;    Advent of Code 2022 - Day 1 task A & B
;;;;    https://adventofcode.com/2022/day/1
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The Elf carrying the most Calories,         has  a total of:  70116
;;;;    The three Elves carrying the most Calories, have a total of: 206582
;;;;
;;;;    (cl) by Arno Jacobs, 2023-01-06
;;;;    

;;;; First - read the data-set

;; Importing 'split-lines' from 'string'
(require ['clojure.string :as 'str])

;; In the list of integers sub-lists will be marked by a 0
(def marker 0)

;; A 'safe' string to an int 
;; Empty strings will be converted to 0 
;; 0 is marker for a new sub-list
(defn parse-int [s]
  (if (= s "")
    marker
    (Integer. (re-find #"\d+" s))))

;; Convert text file (with numbers) to a list of strings
;; 'map' over list of strings and convert to ints
(defn get-lines [file]
  (map #(parse-int %) (str/split-lines (slurp file))))

(def data-set (get-lines "./data/inputDay01_2022.txt"))

;; Take and return sub-list of the first elements until 0 
(defn take-sub [lst]
  (if (= lst ())
    ()
    (if (= (first lst) marker)
      '()
      (cons (first lst) (take-sub (rest lst))))))

;; Drop sub-list of the first elements until 0 
;; Return the remaining list after 0
(defn drop-sub [lst]
  (if (= lst ())
    ()
    (if (= (first lst) marker)
      (rest lst)
      (drop-sub (rest lst)))))

;;; Total the calories per Elf
(defn calories [lst]
  (if (= lst ())
    ()
    (cons (reduce + (take-sub lst))
          (calories (drop-sub lst)))))

;; Pick the 'sc'-number of biggest Ints and return its sum
;; For part 1 the maximum value is asked
;; For part 2 the sum of the three highest values is asked
;;
(defn sum-top [sc subl]
  (reduce + (take sc (reverse (sort (into [] subl))))))


;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2022 - day 1  (Clojure)")
  (print   "The Elf carrying the most Calories,         has  a total of:  ")
  (def clrs (calories data-set))
  (println (sum-top 1 clrs))
  (print   "The three Elves carrying the most Calories, have a total of: ")
  (println (sum-top 3 clrs))
  (println "0K.\n"))

;; And just run the 'main'
;;
;; Run in terminal via:
;; $ clojure -M p01ab.clj
;;
(program)
