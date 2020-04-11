(ns score-generator.core-test
  (:require [clojure.test :refer :all]
            [score-generator.core :as sut]))

(deftest parse-key-test
  (testing "Testing that a key is parsed correctly."
    (is (= (sut/parse-key "Key: Ab") "-4"))))

(deftest parse-time-signature-test
  (testing "Testing that time signature is being parsed correctly"
    (is (= (sut/parse-time-signature "Time Signature: 4/8") {:beats "4"
                                                             :beat-type "8"}))))

(deftest build-attributes-test
  (testing "Testing that building attributes works correctly."
    (let [key-str "Key: C"
          time-str "Time Signature: 4/8"]
      (is (= (sut/build-attributes key-str time-str)
             [:ATTRIBUTES
              [:DIVISIONS "16"]
              [:KEY [:FIFTHS "0"]]
              [:TIME [:BEATS "4"] [:BEAT-TYPE "8"]]
              [:CLEF [:SIGN "G"] [:LINE "2"]]])))))

(deftest parse-measure-test
  (let [key-str "Key: C"
        time "Time Signature: 4/8"
        attr (sut/build-attributes key-str time)]
    (is (= (sut/parse-measure attr 5 "e4/2 c2/2,-1")
           [:MEASURE
            {:number "5"}
            [:ATTRIBUTES
             [:DIVISIONS "16"]
             [:KEY [:FIFTHS "0"]]
             [:TIME [:BEATS "4"] [:BEAT-TYPE "8"]]
             [:CLEF [:SIGN "G"] [:LINE "2"]]]
            [[:NOTE [:PITCH [:STEP "e"] [:OCTAVE "4"] [:DURATION "8"]]]
             [:NOTE [:PITCH [:STEP "c"] [:OCTAVE "2"] [:DURATION "8"] [:ALTER "-" "1"]]]]]))))

(deftest parse-measure-test-exception-fail
  (let [key-str "Key: C"
        time "Time Signature: 4/8"
        attr (sut/build-attributes key-str time)]
    (is (thrown? clojure.lang.ExceptionInfo (sut/parse-measure attr 5 "e4/4 c2/2,-2")))))
