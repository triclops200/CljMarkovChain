(ns markovchain.core
  (:use (markovchain [graph]))
  (:use clojure.java.io)
  (:gen-class))

(defn -main
  "Main function, takes list of files as parameters,
returns up to 10 random sentences from the list"
  [& argv]
  (if (not (and (> (count argv) 0) (every? #(.exists (as-file %)) argv )))
    (print  "Usage: java -jar mchain.jar  <file 1> [file 2] ... ")
    (let [g (reduce (fn [acc file] (add-text acc (slurp file))) {} argv)]
      (println (reduce #(str %1 "\n" %2) (filter #(not (= % "")) (map (fn [x] (random-walk g)) (range 10)))))))
  (flush))
