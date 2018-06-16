(ns kira.core-test
  (:require [clojure.test :refer :all]
            [clj-time.coerce :as ct]
            [kira.core :refer :all]))

(def to-long (comp ct/to-long ct/from-string))

(deftest test-date-time
  (testing "Should return :invalid for invalid inputs"
    (are [x y] (= x y)
      "1970-01-01T00:00:02.000Z" (str (date-time 2000))
      "2000-01-01T00:00:00.000Z" (str (date-time "2000"))
      :invalid (date-time nil) ;;nil
      :invalid (date-time nil) ;;nil
      :invalid (date-time true) ;;boolean
      :invalid (date-time (char 97)) ;;charactor
      :invalid (date-time (keyword "key")) ;; keyword
      :invalid (date-time (symbol "symbol")) ;;symbole
      :invalid (date-time (list 1 2)) ;; collection
      :invalid (date-time (hash-map 1 2)) ;; map
      :invalid (date-time (constantly nil)) ;; function
      :invalid (date-time (date-time (bigint 123456789101112131415))) ;;bigint
      :invalid (date-time "Not date-time string")))) ;;string

(deftest test-my-compare
  (testing "Should compare nil, num, timestamp, string, inst, joda
            Should catch exception when compare different types"
    (are [x y] (= x y)

      true (my-compare false [nil nil] [nil nil])
      false (my-compare true [nil nil] [nil nil])

      true (my-compare false ["11" 14] [11 "12"])
      nil  (my-compare false ["11" "14"] [11 12])

      true (my-compare false
                       [13 14]
                       [14 15])
      false (my-compare true
                        [13 14]
                        [14 15])

      true (my-compare false
                       [(to-long "2018-06-15T13:00:00.000Z") (to-long "2018-06-15T14:00:00.000Z")]
                       [(to-long "2018-06-15T14:00:00.000Z") (to-long "2018-06-15T15:00:00.000Z")])
      false (my-compare true
                        [(to-long "2018-06-15T13:00:00.000Z") (to-long "2018-06-15T14:00:00.000Z")]
                        [(to-long "2018-06-15T14:00:00.000Z") (to-long "2018-06-15T15:00:00.000Z")])

      true (my-compare false
                       [#inst "2018-06-15T13:00:00.000Z" #inst "2018-06-15T14:00:00.000Z"]
                       [#inst "2018-06-15T14:00:00.000Z" #inst "2018-06-15T15:00:00.000Z"])
      false (my-compare true
                        [#inst "2018-06-15T13:00:00.000Z" #inst "2018-06-15T14:00:00.000Z"]
                        [#inst "2018-06-15T14:00:00.000Z" #inst "2018-06-15T15:00:00.000Z"])

      true (my-compare false
                       ["2018-06-15T13:00:00.000Z" "2018-06-15T14:00:00.000Z"]
                       ["2018-06-15T14:00:00.000Z" "2018-06-15T15:00:00.000Z"])
      false (my-compare true
                        ["2018-06-15T13:00:00.000Z" "2018-06-15T14:00:00.000Z"]
                        ["2018-06-15T14:00:00.000Z" "2018-06-15T15:00:00.000Z"])

      true (my-compare false
                       [(ct/from-string "2018-06-15T13:00:00.000Z") (ct/from-string "2018-06-15T14:00:00.000Z")]
                       [(ct/from-string "2018-06-15T14:00:00.000Z") (ct/from-string "2018-06-15T15:00:00.000Z")])
      false (my-compare true
                        [(ct/from-string "2018-06-15T13:00:00.000Z") (ct/from-string "2018-06-15T14:00:00.000Z")]
                        [(ct/from-string "2018-06-15T14:00:00.000Z") (ct/from-string "2018-06-15T15:00:00.000Z")]))))

(deftest test-overlapped?
  (testing
      "Should return nil when have any invalid inputs
       Should return Boolean for comparable values "
    (are [x y] (= x y)
      nil (overlapped? true ["A" "14"] [10 12])
      true (overlapped? true [11 14] [10 12])
      false (overlapped? true ["2000" "2014"] [2001 2012]))))

(deftest test-overlap
  (testing "Should find overlapped pairs in coll"
    (are [x y] (= x y)
      nil (seq (overlap '([1 2] [2 3] [3 4]) true))
      '(([1 2] [2 3]) ([2 3] [3 4])) (seq (overlap '([1 2] [2 3] [3 4]) false)))))
