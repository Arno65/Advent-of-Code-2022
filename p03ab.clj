;;;;    Advent of Code 2022 - Day 3 task A & B
;;;;    https://adventofcode.com/2022/day/3
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The sum of the 'rucksack'  priorities of the item types: 7446
;;;;    The sum of the 'Elf group' priorities of the item types: 2646
;;;;
;;;;    (cl) by Arno Jacobs, 2023-01-06
;;;;    

;;; First - read the data-set

;; Importing 'split-lines' from 'string'
(require ['clojure.string :as 'str])

;; Convert text file to a list of strings
(defn get-lines [file]
  (->> file
       slurp
       (str/split-lines)))

(def data-set (get-lines "./data/inputDay03_2022.txt"))

;;; Convert character to value
;;; a..z  ->   1..26
;;; A..Z  ->  27..52
;;; With this function \& will return 0
(defn char-to-value [c]
  (def vc (int c))
  (if (> vc 96)
    (- vc 96)
    (- vc 38)))

;;;; convert "string" to ((\s \t \r)(\i \n \g))  a pair of lists
;;; Get the left list with 'first' 
;;; Get the right list with 'last' 
(defn split-half [s]
  (def half (/ (count s) 2))
  (cons (->> s
             (take half))
        (cons (->> s
                   (drop half))
              ())))

;; match triplet p1 with p2 and p3
;; SO use this match for pairs (with p3 = p2) & triplets
(defn match [p1 p2 p3]
  (if (= p1 ())
    nil
    (if (and (contains? (set p2) (first p1))
             (contains? (set p3) (first p1)))
      (-> p1
          first
          char-to-value)
      (match (rest p1) p2 p3))))

;; match with help of match triplet 
;; set the last two parameters equal to each other
(defn match-line [ls]
  (def lr (split-half ls))
  (match (first lr) (last lr) (last lr)))

(defn match-triplet [pl]
  (def p1 (first pl))
  (def p2 (second pl))
  (def p3 (second (rest pl)))
  (match p1 p2 p3))

;;; Part 1
(defn sum-rucksack-priorities [pl]
  (->>  pl
        (map #(match-line %))
        (reduce +)))

;;; Part 2
;;; Take every 3 lines and match them 
(defn sumElfGroupPriorities [pl]
  (if (= pl ())
    0
    (+ (match-triplet (take 3 pl))
       (sumElfGroupPriorities (drop 3 pl)))))

;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2022 - day 3  (Clojure)")
  (print   "The sum of the 'rucksack'  priorities of the item types: ")
  (println (sum-rucksack-priorities data-set))
  (print   "The sum of the 'Elf group' priorities of the item types: ")
  (println (sumElfGroupPriorities data-set))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p03ab.clj
(program)
