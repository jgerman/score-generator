(ns score-generator.core
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [hiccup.core :as hiccup]))

;; divisions will always be 16
;; clef will always be treble

#_(def notes
  (insta/parser
   "<NOTES>  = PITCH | PITCH <SPACE+> [NOTES]
    PITCH    = (NOTE [ACC] OCTAVE <'/'> TIMING [DOTTED]) | REST
    ACC      = 'b' | '#' | 'n'
    REST     = <'R'> TIMING [DOTTED]
    NOTE     = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g'
    OCTAVE   = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8'
    TIMING   = '1' | '2' | '4' | '8' | '16'
    DOTTED   = <'.'>
    SPACE    = ' '"))

(defn key->int [s]
  "Takes a key Am, F#, etc and retuns it's position on the circle of 5ths."
  (case s
    ("C" "a") 0
    ("G" "e") 1
    ("D b")   2
    ("A" "f#") 3
    ("E" "c#") 4
    ("B" "g#") 5
    ("Gb" "d#") 6
    ("F" "d") -1
    ("Bb" "b") -2
    ("Eb" "g") -3
    ("Ab" "f") -4
    ("Db" "a$") -5))

(defn transform-key [key-tree]
  "Takes a key tree, and turns it into an integer.
   A key tree looks like [:KEY [:STEP 'A'] [:ACC 'b']]"
  (let [txfm {:STEP (fn [x] x)
              :ACC (fn [x] x)}]
    (->> key-tree
         (insta/transform txfm)
         rest
         str/join
         key->int)))

(def simple-score
  (insta/parser
   "KEY    = <'Key: '> STEP [ACC]
    ACC    = 'b' | '#'
    <TIMESIG> = <'Time Signature: '> BEATS <'/'> TYPE
    BEATS   = BNUM
    TYPE    = TNUM
    <BNUM>  = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8'
    <TNUM>  = '1'|'2'|'4'|'8'
    MEASURE    = NOTES
    <NOTES>      = NOTE | (NOTE <' '> NOTES)
    SPACE      = <' '>
    NOTE       = PITCH
    PITCH      = STEP OCTAVE <'/'> DURATION [<','>ALTER]
    DURATION   = '1' | '2' | '4' | '8' | '16'
    ALTER      = ('+'|'-')('1'|'2')
    STEP       = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G'
    OCTAVE     = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8'"))

(defn calc-duration [divisions timing]
  "Returns the appropriate duration for note timings base on divisions per measure."
  [:DURATION
   (str (/ divisions (Integer/parseInt timing)))])


(defn parse-line [s start-rule & {:keys [txfm-map]}]
  (let [parse-fn (fn [x]
                   (simple-score x :start start-rule))]
    (cond->> s
      true parse-fn
      txfm-map (insta/transform txfm-map))))

(defn parse-key [s]
  (-> s
      (parse-line :KEY)
      transform-key
      str))

(def time-signature-txfm {:BEATS (fn [x]  x)
                          :TYPE  (fn [x]  x)})

(defn parse-time-signature [s]
  (set/rename-keys
   (into {}
         (-> s
             (parse-line :TIMESIG)))
      {:BEATS :beats
       :TYPE :beat-type}))

(defn build-attributes [key-line sig-line]
  "Builds an attributes tree based on key and beat lines."
  (let [k (parse-key key-line)
        {:keys [beats beat-type]} (parse-time-signature sig-line)]
    [:ATTRIBUTES
     [:DIVISIONS "16"]
     [:KEY [:FIFTHS k]]
     [:TIME
      [:BEATS beats]
      [:BEAT-TYPE beat-type]]
     [:CLEF
      [:SIGN "G"]
      [:LINE "2"]]]))

(defn validate-measure
  "Todo ... come back and fix this sloppy fragile mess."
  [measure-tree divisions]
  (let [notes (nth measure-tree 3)
        durations
        (map #(-> % rest first (nth 3) second Integer/parseInt) notes)
        beat-count (reduce + 0 durations)]
    (when (not= divisions
                beat-count)
      (throw (ex-info "Wrong number of beats in measure."
                      {:measure measure-tree})))
    measure-tree))

(defn build-measure-txfm [attributes line-no]
  {:DURATION (partial calc-duration 16)
   :MEASURE  (fn [& args] [:MEASURE  {:number (str line-no)} attributes args])})

(defn parse-measure [attribute-tree line-no s]
  (-> s
      (parse-line :MEASURE :txfm-map (build-measure-txfm attribute-tree line-no))
      (validate-measure 16))) ;; TODO cleanup



(defn read-file [filename]
  (str/split-lines
   (slurp (io/resource filename))))

;; definitely into quick and dirty territory now to see if this works at all
;; hiccup.page should have functions to make this bit easier
;; and the measure tree can be wrapped with parts etc

(defn header []
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!DOCTYPE score-partwise PUBLIC
 \"-//Recordare//DTD MusicXML 3.1 Partwise//EN\"
 \"http://www.musicxml.org/dtds/partwise.dtd\">
<score-partwise version=\"3.1\">
 <part-list>
 <score-part id=\"P1\">
 <part-name>Music</part-name>
 </score-part>
 </part-list>
 <part id=\"P1\">")

(defn footer[]
  "</part>
</score-partwise>")

(defn generate-musicxml [filename]
  (let [lines (read-file filename)
        key-sig (parse-key (first lines))
        beats   (parse-time-signature (second lines))
        attrs   (build-attributes (first lines) (second lines))
        measures (map-indexed (fn [idx itm]
                                (parse-measure attrs (+ 1 idx) itm))
                              (drop 2 lines))]
    (spit "test.xml" (header))
    (spit "test.xml" (hiccup/html measures) :append true)
    (spit "test.xml" (footer) :append true)))
