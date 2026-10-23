(ns interpreter.lexer
  (:require [clojure.string :as str])
  (:gen-class))

(defn skip-when-fn
  [f]
  (fn [^String line cursor]
    (let [len (count line)]
      (loop [c cursor]
        (if (and (< c len)
                 (f (.charAt line c)))
          (recur (inc c))
          c)))))

(defn skip-whitespace
  [^String line cursor]
  (apply
    (skip-when-fn ^[char] Character/isWhitespace)
    [line cursor]))

(defn skip-letter
  [^String line cursor]
  (apply
    (skip-when-fn ^[char] Character/isLetter)
    [line cursor]))

(defn skip-non-double-quote
  [^String line cursor]
  (apply
    (skip-when-fn #(not= % \"))
    [line cursor]))

(defn skip-digit
  [^String line cursor]
  (apply
    (skip-when-fn ^[char] Character/isDigit)
    [line cursor]))

(defn skip-letter-digit
  [^String line cursor]
  (apply
    (skip-when-fn ^[char] Character/isLetterOrDigit)
    [line cursor]))

(defrecord MatchResult [start end token])

(defn match-fn
  ([fast-checker skip-fn]
   (match-fn fast-checker skip-fn nil))
  ([fast-checker skip-fn start-fn]
   (fn [^String line cursor]
     (let [len (count line)
           c (skip-whitespace line cursor)]
       (when (and (< c len) (fast-checker line c))
         (let [start (if (nil? start-fn)
                       c
                       (start-fn c))
               end (skip-fn line start)]
           (when (< start end)
             (->MatchResult start (dec end) (subs line start end)))))))))

(def ^:dynamic *throw-exception* true)

(defn match-keyword
  [^String line cursor]
  (if-let [r (apply
               (match-fn #(^[char] Character/isLetter (.charAt %1 %2)) skip-letter)
               [line cursor])]
    r
    (when *throw-exception*
      (throw (ex-info "Keyword expected" {:col cursor})))))

(defn match-string
  [^String line cursor]
  (if-let [r (apply
               (match-fn #(= \" (.charAt %1 %2)) skip-non-double-quote inc)
               [line cursor])]
    r
    (when *throw-exception*
      (throw (ex-info "String expected" {:col cursor})))))

(defn match-number
  [^String line cursor]
  (if-let [r (apply
               (match-fn #(^[char] Character/isDigit (.charAt %1 %2)) skip-digit)
               [line cursor])]
    r
    (when *throw-exception*
      (throw (ex-info "Number expected" {:col cursor})))))

(defn match-variable-name
  [^String line cursor]
  (if-let [r (apply
               (match-fn #(^[char] Character/isLetter (.charAt %1 %2)) skip-letter-digit)
               [line cursor])]
    r
    (when *throw-exception*
      (throw (ex-info "Variable expected" {:col cursor})))))

(defn match
  [^String line cursor ^String text]
  (let [len (count line)
        c (skip-whitespace line cursor)]
    (if (< c len)
      (if (str/starts-with? (subs line c) text)
        (->MatchResult c (dec (+ c (count text))) text)
        (when *throw-exception* (throw (ex-info (str text " expected") {:col c}))))
      (when *throw-exception* (throw (ex-info (str text " expected") {:col c}))))))

(defn match-eol
  [^String line cursor]
  (let [len (count line)
        c (skip-whitespace line cursor)
        r (>= c len)]
    (if r
      (->MatchResult cursor c (subs line cursor c))
      (when *throw-exception*
        (throw (ex-info "End of line expected" {:col cursor}))))))

(comment
  (skip-whitespace "  xx" 0)
  (skip-letter "xx " 0)
  (skip-non-double-quote "abc\"" 0)
  (skip-digit "123 " 0)
  (skip-letter-digit "abc123 " 0)
  (match-keyword " REM " 0)
  (match-string "  \"adda ad  \"" 0)
  (match-number "  11 " 0)
  (match-variable-name "  abc11 " 0)
  (match-variable-name "  1abc " 0)
  (match " abc " 0 "ab")
  (match " abc " 2 "ab")
  (match-eol "  \r\n" 0)
  )
