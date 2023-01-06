;;;;    Advent of Code 2022 - Day 2 task A & B
;;;;    https://adventofcode.com/2022/day/2
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    The total score for strategy 1:  9759
;;;;    The total score for strategy 2: 12429
;;;;
;;;;    (cl) by Arno Jacobs, 2023-01-06
;;;;    

;;; First - read the data-set

;; Importing 'split-lines' from 'string'
(require ['clojure.string :as 'str])

;; Convert text file (with numbers) to a list of strings
;; 'map' over list of strings and convert to ints
(defn get-lines [file]
  (str/split-lines (slurp file)))

(def data-set (get-lines "./data/inputDay02_2022.txt"))

;;; Lisp like patern matching for strategy part 1
(defn rps-s1 [one-round]
  (cond   (= one-round "A X") 4
          (= one-round "A Y") 8
          (= one-round "A Z") 3
          (= one-round "B X") 1
          (= one-round "B Y") 5
          (= one-round "B Z") 9
          (= one-round "C X") 7
          (= one-round "C Y") 2
          (= one-round "C Z") 6))

;;; Lisp like patern matching for strategy part 2
(defn rps-s2 [one-round]
  (cond   (= one-round "A X") 3
          (= one-round "A Y") 4
          (= one-round "A Z") 8
          (= one-round "B X") 1
          (= one-round "B Y") 5
          (= one-round "B Z") 9
          (= one-round "C X") 2
          (= one-round "C Y") 6
          (= one-round "C Z") 7))

;;; Rock-Paper-Scissors ~ select strategy ~ round by round
(defn rps [strategy one-round]
  (if (= strategy 1)
    (rps-s1 one-round)
    (rps-s2 one-round)))

;; Play the game and count ther scores
(defn work-strategy [strategy score game-data]
  (if (= game-data ())
    score
    (work-strategy  strategy
                    (+ score (rps strategy (first game-data)))
                    (rest game-data))))

;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2022 - day 2  (Clojure)")
  (print   "The total score for strategy 1:  ")
  (println (work-strategy 1 0 data-set))
  (print   "The total score for strategy 2: ")
  (println (work-strategy 2 0 data-set))
  (println "0K.\n"))

;; And just run the 'main'
;;
;; Run in terminal via:
;; $ clojure -M p02ab.clj
;;
(program)
