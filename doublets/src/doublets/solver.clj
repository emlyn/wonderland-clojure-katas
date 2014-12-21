(ns doublets.solver
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def words (-> "words.edn"
               (io/resource)
               (slurp)
               (read-string)))

(defn diff-chars [w1 w2]
  (count (filter not (map = w1 w2))))

(defn add-links [links word]
  (reduce #(if (= 1 (diff-chars %2 word))
             (-> %1
                 (update-in [word] conj %2)
                 (update-in [%2] conj word))
             %1)
          (assoc links word #{})
          (keys links)))

(defn build-links [words]
  (reduce add-links {} words))

;; If there are multiple solutions, this will just return one arbitrarily,
;; not necessarily the shortest, but it seems to be enough to pass the tests.
(defn doublets [word1 word2]
  (let [n     (count word1)
        links (build-links (filter #(= n (count %))
                                   words))]
    (letfn [(recurse [path next-words]
              (when (seq next-words)
                (if (next-words word2)
                  (conj path word2)
                  (first (remove nil?
                                 (for [w (remove (set path) next-words)]
                                   (recurse (conj path w) (links w))))))))]
      (or (recurse [word1] (links word1))
          []))))
