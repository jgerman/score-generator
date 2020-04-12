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

(deftest parse-single-note-test
  (is (= (sut/parse-note "e4/4")
         [:note [:pitch [:step "E"] [:octave "4"]] [:duration "4"]])))

(deftest parse-single-with-alter-test
  (is (= (sut/parse-note "e4,-1/4")
         [:note [:pitch [:step "E"] [:octave "4"] [:alter "-" "1"]] [:duration "4"]])))

(deftest parse-single-with-dot-test
  (is (= [:note [:pitch [:step "E"] [:octave "4"]] [:duration "4" [:dotted]] [:dot]]
       (sut/parse-note "e4/4."))))

(deftest parse-single-rest-test
  (is (= (sut/parse-note "r4")
         [:note [:rest [:duration "4"]]])))

(deftest note-tree-duration
  (is (= (sut/note-tree->duration (sut/parse-note "e4,-1/4"))
         "4")))

(deftest note-tree-duration-rest-test
  (is (= (sut/note-tree->duration (sut/parse-note "r4"))
         "4")))

(deftest build-attributes-test
  (testing "Testing that building attributes works correctly."
    (let [key-str "Key: C"
          time-str "Time Signature: 4/8"]
      (is (= (sut/build-attributes key-str time-str)
             [:attributes
              [:divisions "4"]
              [:key [:fifths "0"]]
              [:time [:beats "4"] [:beat-type "8"]]
              [:clef [:sign "G"] [:line "2"]]])))))

(deftest parse-measure-test
  (let [key-str "Key: C"
        time "Time Signature: 4/4"
        attr (sut/build-attributes key-str time)]
    (is (= (sut/parse-measure attr 5 "e4/2 c2,-1/2")
           [:measure
            {:number "5"}
            [:attributes
             [:divisions "4"]
             [:key [:fifths "0"]]
             [:time [:beats "4"] [:beat-type "4"]]
             [:clef [:sign "G"] [:line "2"]]]
            [[:note [:pitch [:step "E"] [:octave "4"]] [:duration "8"]]
             [:note [:pitch [:step "C"] [:octave "2"] [:alter "-" "1"]] [:duration "8"]]]]))))

(deftest parse-measure-with-dots-test
  (let [key-str "Key: C"
        time "Time Signature: 4/4"
        attr (sut/build-attributes key-str time)]
    (is (= [:measure
            {:number "5"}
            [:attributes
             [:divisions "4"]
             [:key [:fifths "0"]]
             [:time [:beats "4"] [:beat-type "4"]]
             [:clef [:sign "G"] [:line "2"]]]
            [[:note [:pitch [:step "E"] [:octave "4"]] [:duration "12"]]
             [:note [:pitch [:step "C"] [:octave "4"] [:alter "-" "1"]] [:duration "4"]]]]
           (sut/parse-measure attr 5 "e4/2. c4,-1/4")))))

(deftest parse-measure-test-exception-fail
  (let [key-str "Key: C"
        time "Time Signature: 4/8"
        attr (sut/build-attributes key-str time)]
    (is (thrown? clojure.lang.ExceptionInfo (sut/parse-measure attr 5 "e4/4 c2,-2/2")))))
