;;;;    Advent of Code 2022 - Day 6 task A & B
;;;;    https://adventofcode.com/2022/day/6
;;;;
;;;;    Solutions in Clojure
;;;;    (Ter leering ende vermaeck...)
;;;;
;;;;    Start of message marker for part 1 is at: 1766
;;;;    Start of message marker for part 2 is at: 2383
;;;;
;;;;    (cl) by Arno Jacobs, 2023-01-06
;;;;    

;;; First - read the data-set
;;; In this quest just one long string with all lowercase letters
;;; The string is put into a list of characters
(def data-set (-> "./data/inputDay06_2022.txt"
                  slurp
                  seq))

;;;; Recursively testing for unique elements
;;;  Return -1 if NO marker is found
(defn marker [mpos mln lst]
  (def subl (take mln lst))
  (if (> mln (count lst))
    -1
    (if (= (distinct subl) subl)
      mpos
      (->> (rest lst)
           (marker (-> mpos
                       inc)
                   mln)))))

;; Including start position (counter) 
(defn messageMarker [mln lst]
  (marker mln mln lst))

;;; The 'main' program - - -
(defn program []
  (println "Advent of Code 2022 - day 6  (Clojure)")
  (print   "Start of message marker for part 1 is at: ")
  (println (messageMarker  4 data-set))
  (print   "Start of message marker for part 2 is at: ")
  (println (messageMarker 14 data-set))
  (println "0K.\n"))

;; Run in terminal via: clojure -M p06ab.clj
(program)
