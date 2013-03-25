(ns drugz.test.core
  (:use [drugz.core])
  (:use [clojure.test]))


(testing "parses max-weight"
  (deftest max-weight-parse
    (is (= 900 (get-max-weight ["max weight: 900"])))))

(testing "parse doll from a string"
  (deftest doll-parse
    (is (= (struct doll "ricardo" 10 400) (get-dolls "ricardo        10   400")))))

(testing "file location"
  (deftest file-test
    (is (= ["max weight: "](get-file "test/drugz/test/test.txt")))))