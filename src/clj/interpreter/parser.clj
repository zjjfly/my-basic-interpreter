(ns interpreter.parser
  (:require [clojure.string :as str]
            [interpreter.common :as common]
            [interpreter.lexer :as lex])
  (:gen-class))

(common/defrecord+ ValueMatchResult
                   [type] lex/MatchResult)

(defonce variables (atom {}))

(defn get-var [k]
  (k @variables))

(defn set-var [k v]
  (swap! variables assoc k v))

(defn match-value
  [^String line cursor]
  (binding [lex/*throw-exception* false]
    (condp apply [line cursor]
      lex/match-string :>> #(map->ValueMatchResult (merge % {:type :str}))
      lex/match-number :>> #(map->ValueMatchResult (merge % {:type :number}))
      lex/match-variable-name :>> #(map->ValueMatchResult (merge % {:type :var}))
      )))

(defn eval-value
  [^ValueMatchResult r]
  (let [token (:token r)]
    (case (:type r)
      :str token
      :number (^[String] Integer/parseInt token)
      :var (-> token
               keyword
               get-var)
      )))

(defn match-variable-exp
  [^String line cursor match-fn separator]
  (binding [lex/*throw-exception* false]
    (if-let [r (match-fn line cursor)]
      (if-let [s (lex/match line (inc (:end r)) separator)]
        (cons r (match-variable-exp line (inc (:end s)) match-fn separator))
        [r])
      [])))

(defn parse
  [^String line cursor match-fns]
  (if-let [match-fn (first match-fns)]
    (when-let [r (match-fn line cursor)]
      (cons r (parse line (inc (:end r)) (rest match-fns))))
    (do (lex/match-eol line cursor)
        [])))

(defn parse-let
  [^String line cursor]
  (let [[var-name _ value] (parse line cursor [lex/match-variable-name
                                               #(lex/match %1 %2 "=")
                                               match-value])]
    (set-var (keyword (:token var-name)) (eval-value value))))

(defn parse-print
  [^String line cursor]
  (binding [lex/*throw-exception* false]
    (if-let [_ (lex/match-eol line cursor)]
      (println)
      (->> (match-variable-exp line cursor match-value ",")
           (map eval-value)
           (apply println)))))

(defn parse-statement
  [^String line]
  (if-let [keyword (lex/match-keyword line 0)]
    (case (str/lower-case (:token keyword))
      "let" (parse-let line (inc (:end keyword)))
      "print" (parse-print line (inc (:end keyword)))
      (throw (ex-info "Unknown statement" {})))
    (throw (ex-info "Statement expected" {}))))

(comment
  (parse-statement "LET x=3")
  (parse-statement "PRINT \"Hello,World!\"")
  (parse-statement "PRINT 1")
  (parse-statement "PRINT x")
  (parse-statement "PRINT x,1,\"abc\"")
  (set-var :x 1)
  (match-value "\"xx\"" 0)
  (match-value "123" 0)
  (match-value "x" 0)
  (match-variable-exp "1,2,3" 0 match-value ",")
  )
