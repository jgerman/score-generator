(ns score-generator.core
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [hiccup.core :as hiccup]))

;; divisions will always be 4
(def DIVISIONS 4)
;; clef will always be treble

(defn key->int [s]
  "Takes a key Am, F#, etc and retuns it's position on the circle of 5ths."
  (case s
    ("C" "a") 0
    ("G" "e") 1
    ("D" "b")   2
    ("A" "f#") 3
    ("E" "c#") 4
    ("B" "g#") 5
    ("Gb" "d#") 6
    ("F" "d") -1
    ("Bb" "g") -2
    ("Eb" "c") -3
    ("Ab" "f") -4
    ("Db" "a$") -5))

(defn transform-key [key-tree]
  "Takes a key tree, and turns it into an integer.
   A key tree looks like [:KEY [:STEP 'A'] [:ACC 'b']]"
  (let [txfm {:step (fn [x] x)
              :acc (fn [x] x)}]
    (->> key-tree
         (insta/transform txfm)
         rest
         str/join
         key->int)))

;; line one is assumed to be the title
(def simple-score
  (insta/parser
   " key    = <'Key: '> step [acc]
    acc    = 'b' | '#'
    <timesig> = <'Time Signature: '> beats <'/'> type
    beats   = bnum
    type    = tnum
    <bnum>  = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8'
    <tnum>  = '1'|'2'|'4'|'8'
    measure    = notes
    <notes>      = note | (note <' '> notes)
    space      = <' '>
    note       = pitch <'/'> duration | rest
    rest       = <'r'> duration
    pitch      = step octave [<','>alter]
    duration   = ('1' | '2' | '4' | '8' | '12' | '16') [dotted]
    dotted     = <'.'>
    alter      = ('+'|'-')('1'|'2')
    step       = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G'
    octave     = '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8'"))


(defn dotted?
  "Using the always frowned upon flatten, but it gets me my answer quickly."
  [note-args]
  (some #{:dotted} (flatten note-args)))

;; TODO look closer at this, because we force divisions to 4 and represent the note type by integers
;; 1, 2, 4, 8, 16
;; we always divice 16 by the note to get the duration
;; TODO can probably just conj rest on...
(defn calc-duration [divisions timing & rest]
  "Returns the appropriate duration for note timings base on divisions per measure."
  (let [dotted (dotted? rest)
        multiplier (if rest
                     1.5
                     1)
        base-element
        [:duration
         (str (int (* multiplier (/ 16 (Integer/parseInt timing)))))]]
    (if dotted
      (conj base-element [:dotted])
      base-element)))


(defn parse-line [s start-rule & {:keys [txfm-map]}]
  (let [parse-fn (fn [x]
                   (simple-score x :start start-rule))]
    (cond->> s
      true parse-fn
      txfm-map (insta/transform txfm-map))))

(defn parse-key [s]
  (-> s
      (parse-line :key)
      transform-key
      str))

(def time-signature-txfm {:beats (fn [x]  x)
                          :type  (fn [x]  x)})

(defn parse-time-signature [s]
  (set/rename-keys
   (into {}
         (-> s
             (parse-line :timesig)))
      {:beats :beats  ;; now that I'm lowercaseing this probably isn't needed TODO
       :type :beat-type}))

(defn build-attributes [key-line sig-line]
  "Builds an attributes tree based on key and beat lines."
  (let [k (parse-key key-line)
        {:keys [beats beat-type]} (parse-time-signature sig-line)]
    [:attributes
     [:divisions (str DIVISIONS)]
     [:key [:fifths k]]
     [:time
      [:beats beats]
      [:beat-type beat-type]]
     [:clef
      [:sign "G"]
      [:line "2"]]]))

(defn attribute-tree->duration-per-measure
  "This only works for X/4 time right now, I'll have to include the divisions in later."
  [attr]
  (let [time (nth attr 3)
        beats (-> time second second Integer/parseInt)
        btype (-> time (nth 2) second Integer/parseInt)]
    (* beats btype)))

(defn note-tree->duration [tree]
  (let [note-type ((comp first second) tree)]
    (case note-type
        :pitch (second (nth tree 2))
        :rest  ((comp second second second) tree))))

(defn validate-measure
  "Todo ... come back and fix this sloppy fragile mess. It's currently wrong in a number of areas, but most notably that it only handles X/4 time sigs."
  [measure-tree attr-tree divisions]
  (let [notes (nth measure-tree 3)
        total_duration (attribute-tree->duration-per-measure attr-tree)
        durations
        (map #(-> % note-tree->duration Integer/parseInt) notes)
        beat-count (reduce + 0 durations)]
    (when (not= total_duration
                beat-count)
      (throw (ex-info "Wrong number of beats in measure."
                      {:measure measure-tree})))
    measure-tree))

(defn maybe-add-dot [& xs]
  (let [type ((comp first first) xs)
        dotted (dotted? xs)
        base-element (case type
                       :rest [:note (first xs)]
                       :pitch [:note (first xs) (second xs)])]
    (if dotted
      (conj base-element [:dot])
      base-element)))

(defn build-measure-txfm [attributes line-no]
  {:note maybe-add-dot
   :duration (partial calc-duration 16)
   :measure  (fn [& args] [:measure  {:number (str line-no)} attributes args])
   :step (fn [x] [:step (str/upper-case x)])})

(defn parse-measure [attribute-tree line-no s]
  (-> s
      (parse-line :measure :txfm-map (build-measure-txfm attribute-tree line-no))
      (validate-measure attribute-tree 16))) ;; TODO cleanup

;; not used in mainline parsing but useful to make sure notes are correct
(def note-txfm
  {:step (fn [x] [:step (str/upper-case x)])
   :note maybe-add-dot})

(defn parse-note [s]
  (-> s
      (parse-line :note :txfm-map note-txfm)))

(defn read-file [filename]
  (str/split-lines
   (slurp (io/resource filename))))

;; definitely into quick and dirty territory now to see if this works at all
;; hiccup.page should have functions to make this bit easier
;; and the measure tree can be wrapped with parts etc

(defn header [title]
  (str  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!DOCTYPE score-partwise PUBLIC
 \"-//Recordare//DTD MusicXML 3.1 Partwise//EN\"
 \"http://www.musicxml.org/dtds/partwise.dtd\">
<score-partwise version=\"3.1\">
 <work>
    <work-title>"
        title
        "</work-title>
 </work>
 <part-list>
 <score-part id=\"P1\">
 <part-name>guitar</part-name>
 </score-part>
 </part-list>
 <part id=\"P1\">"))

(defn footer[]
  "</part>
</score-partwise>")

(defn generate-musicxml [infile outfile]
  (let [lines (read-file infile)
        title (first lines)
        key-sig (parse-key (second lines))
        beats   (parse-time-signature (second lines))
        attrs   (build-attributes (nth lines 1) (nth lines 2))
        measures (map-indexed (fn [idx itm]
                                (parse-measure attrs (+ 1 idx) itm))
                              (drop 3 lines))]
    (spit outfile (header title))
    (spit outfile (hiccup/html measures) :append true)
    (spit outfile (footer) :append true)))
