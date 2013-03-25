;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Steve Bischoff                                         ;;
;; A program for distibuting "teddy bears" to "children"  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns drugz.core
   (:use [clojure.string :only (split-lines)])
   (:use [clojure.string :only (split)])
   (:use clojure.pprint)
)

(defstruct doll :name :weight :value)
(def ^:dynamic items [])

;;-----------FUNCTION get-file  --------------------
;; read in file data from file location 

(defn get-file [location]
  (split-lines (slurp location))
  )

;;-----------FUNCTION get-max-weight  --------------------
;; get maximum weight from text file being the first line 
;; and matching the regular expression "^max weight:\s*(\d+)
(defn get-max-weight [data]
  (def match (re-pattern #"^max weight:\s*(\d+)"))
  (read-string (get (re-matches match (first data)) 1)))

;;-----------FUNCTION get-dolls --------------------
;; after the first five lines are dropped 
;; read the name, weight and value into a temp struct 
(defn get-dolls  [lines]
  (def line (split lines #"\s+"))
  (struct doll (nth line 0) (read-string (nth line 1)) (read-string (nth line 2))))

;;-----------FUNCTION parse-text --------------------
;; mapping the dolls to the doll temp struct 
;; after dropping the first five lines as well as storing
;; the max weight the "delivery truck" can hold
(defn parse-text
  [data]
  { :max-weight (get-max-weight data)
    :dolls      (map get-dolls (drop 5 data)) } )

;;-----------FUNCTION find-weights --------------------
;; Dynamic programming algorithm that takes in the max weight and 
;; recursively adds to the list of 
;; stuffed animals below the max weight
(defn find-weights [idx max-weight]
  (cond
   (< idx 0) [0 []]
   (= max-weight 0) [0 []]
   :else
   (let [{weight :weight value :value} (get items idx)]
     (if (> weight max-weight)
       ( find-weights (dec idx) max-weight)
       (let [[vn sn :as no]  (find-weights (dec idx) max-weight)
             [vy sy :as yes] (find-weights (dec idx) (- max-weight weight))]
         (if (> (+ vy value) vn)
           [(+ vy value) (conj sy idx)]
           no)))))
  )

;;-----------FUNCTION print-results --------------------
;; Takes in the doll list and the max weight and runs them 
;; through the dynamic algorithm while mapping the indexes 
;; of the output to the doll names, computes the total weight and prints out the results
(defn print-results [dolls max]

 (let [[value i] (find-weights  (-> dolls count dec) max)
        names (map (comp :name dolls) i)]
    (println "total value:" value)
    (println "total weight:" (reduce + (map (comp :weight dolls) i)))
    )
 (println "packed dolls:\n")
 (print-table dolls)

)


;;-----------FUNCTION main --------------------
;; main function takes in text file as first argument 
;;and calls parsing and solving functions
(defn -main [& args]


  (def text (get-file "test/drugz/test/example_data.txt"))
  (def doll-list (parse-text text))
  (def max-weight (get doll-list :max-weight))
  (def dolls (vec (get doll-list :dolls)))
  (def ^:dynamic items dolls)
  (print-results dolls max-weight)
  
  )















