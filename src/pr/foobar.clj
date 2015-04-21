(ns pr.foobar
  (:require [clojure.string :as str]
            [table.core :as table]))

;;; Modifications to Table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-rows-and-fields
  "The same as table.core/generate-rows-and-fields, but uses uses pr-str instead
  of str when converting its values to strings.

  Returns rows and fields. Rows are a vector of vectors containing string cell
  values."
  [table options]
  (let [top-level-vec (not (coll? (first table)))
        fields (cond
                 top-level-vec [:value]

                 (map? (first table))
                 (or (:fields options)
                     (->> table (map keys) flatten vec distinct))

                 (map? table) [:key :value]

                 :else (first table))
        rows (cond
               top-level-vec (map #(vector %) table)
               (map? (first table)) (map #(map (fn [k] (get % k)) fields) table)
               (map? table) table
               :else (rest table))
        rows (map (fn [row] (map #(if (nil? %) "" (pr-str %)) row)) rows)
        sort-opt (options :sort)
        rows (if (and sort-opt (some #{sort-opt} (conj fields true)))
               (sort-by
                 #(nth % (if (true? sort-opt) 0 (.indexOf fields sort-opt)))
                 rows)
               rows)
        rows (->> rows (map vec) (map (fn [row] (map #'table/escape-newline row))))]
    [rows fields]))

(defn render-rows
  "The same as table.core/render-rows, but patched for using our own version of
  generate-rows-and-fields.

  Generates a list of formatted string rows given almost any input"
  [table options]
  (let [[rows fields] (generate-rows-and-fields table options)
        rendered-rows (#'table/render-rows-with-fields rows fields options)]
    (if (:desc options)
      (concat rendered-rows [(format "%s rows in set" (count rows))])
      rendered-rows)))

(defn table-str
  "The same as table.core/table-str, but patched to use our own version of
  render-rows in order to properly print strings and nil.

  Same options as table but returns table as a string"
  [args & {:keys [style off-set]
           :or {style :plain off-set 0}
           :as options}]
  (binding [table/*style* style]
    (let [rows (render-rows args (if (map? options) options {}))]
      (apply str
        (str/join "\n"
          (if (pos? off-set)
            (map #(apply str (concat (repeat off-set \ ))) rows)
            rows))))))

(defn table
   "Generates an ascii table for almost any input that fits in your terminal.
   Multiple table styles are supported.

   Options:

   * :sort   When set with field name, sorts by field name. When set to true
             sorts by first column. Default is false.
   * :fields An optional vector of fields used to control ordering of fields.
             Only works with rows that are maps.
   * :desc   When set to true, displays row count after table. Default is nil.
   * :style  Sets table style. Available styles are :plain, :org, :unicode and
             :github-markdown. Default is :plain."
  [& args]
  (println (apply table-str args)))

(defn pr-table
  "Helper function for calling table. The argument `headers` should be the
  titles of the columns, while `cols` should be a collection of collections with
  the same amount of items in each sub collection as there are headers."
  ([headers] (table [headers] :style :unicode))
  ([headers cols]
   (assert (every? (partial = (count headers)) (map count cols))
           (str "Mismatch between the header count and the number of items"
                "in each of cols."))
   (binding [table.width/*width* (delay 100)]
     (table (into [headers] cols) :style :unicode))))

;;; Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn exception? [x]
  (when (instance? Exception x) x))

(defmacro throw-maybe
  [x]
  `(if-let [err# (exception? ~x)]
     (throw err#)
     ~x))

(defmacro pass-exception [expr]
  `(try ~expr (catch Exception e# e#)))

(defmacro pass-exception-bool [expr]
  (let [res `(pass-exception ~expr)]
    `(if (exception? ~res)
       ~res
       (boolean ~res))))

(defn destr-gensym? [x]
  {:pre [(symbol? x)]}
  (boolean (#{"vec__" "map__"} (apply str (take 5 (name x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro pr->
  "Acts like clojure.core/->, but prints out each step/form and its result to a
  table.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  [& body]
  (let [[init & exprs] body
        syms (repeatedly (count body) gensym)
        vals (mapcat (fn [exp sym prev]
                       [sym `(when-not (exception? ~prev)
                               (pass-exception
                                 ~(if (seq? exp)
                                    `(~(first exp) ~prev ~@(next exp))
                                    `(~exp ~prev))))])
               exprs (rest syms) syms)
        ress `(take-while identity ~(vec (rest syms)))]
    `(let ~(vec (concat [(first syms) init] vals))
       (pr-table
         [(format "-> %s" ~init) "step"]
         (map vector ~(list `quote exprs) ~ress))
       (throw-maybe (last ~ress)))))

(defmacro pr->>
  "Acts like clojure.core/->>, but prints out each step/form and its result to a
  table.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  [& body]
  (let [[init & exprs] body
        syms (repeatedly (count body) gensym)
        vals (mapcat (fn [exp sym prev]
                       [sym `(when-not (exception? ~prev)
                               (pass-exception
                                 ~(if (seq? exp)
                                    `(~(first exp) ~@(next exp) ~prev)
                                    `(~exp ~prev))))])
               exprs (rest syms) syms)
        ress `(take-while identity ~(vec (rest syms)))]
    `(let ~(vec (concat [(first syms) init] vals))
       (pr-table
         [(format "->> %s" ~init) "step"]
         (map vector ~(list `quote exprs) ~ress))
       (throw-maybe (last ~ress)))))

(defmacro pr-let
  "Acts just like clojure.core/let, but prints out the result of each let-bound
  variable and its value in a table.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  [bindings & body]
  {:pre [(even? (count bindings))]}
  (let [locals (take-nth 2 bindings)
        exprs  (take-nth 2 (rest bindings))
        guard  (map (fn [f] `(pass-exception ~f)) exprs)
        bs     (destructure (vec (interleave locals guard)))
        syms   (remove destr-gensym? (take-nth 2 bs))]
    `(let [vals# (let ~bs ~(vec syms))
           res#  (pass-exception (let ~bindings ~@body))]
       (pr-table ["let" "value"]
         (conj (mapv vector
                 ~(mapv (fn [x] `(quote ~x)) syms)
                 vals#)
               ['~'RESULT res#]))
       (throw-maybe res#))))

(defmacro pr-when-let
  "Acts like clojure.core/when-let, but prints out the boolean value of the
  test-clause, the value of the binding if the test succeeded and the result of
  the body in a table format.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  [bindings & body]
  {:pre [(= (count bindings) 2)]}
  (let [locals (take-nth 2 bindings)
        exprs  (take-nth 2 (rest bindings))
        guard  (map (fn [f] `(pass-exception ~f)) exprs)
        bs     (destructure (vec (interleave locals guard)))
        syms   (remove destr-gensym? (take-nth 2 bs))]
    `(let [vals# (let ~bs ~(vec syms))
           res#  (pass-exception (when-let ~bindings ~@body))]
       (pr-table [(format "when-let %s" '~(first exprs))
                  (pr-str ~(boolean (second bindings)))]
         (conj (mapv vector
                 ~(mapv (fn [x] `(quote ~x)) syms)
                 vals#)
               ['~'RESULT res#]))
       (throw-maybe res#))))

(defmacro pr-if-let
  "Acts like clojure.core/if-let, but prints out the boolean value of the
  test-clause, the value of the local binding and the result of the body in a
  table format.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  [bindings & body]
  {:pre [(= (count bindings) 2)]}
  (let [locals (take-nth 2 bindings)
        exprs  (take-nth 2 (rest bindings))
        guard  (map (fn [f] `(pass-exception ~f)) exprs)
        bs     (destructure (vec (interleave locals guard)))
        syms   (remove destr-gensym? (take-nth 2 bs))]
    `(let [vals# (let ~bs ~(vec syms))
           res# (pass-exception (if-let ~bindings ~@body))]
       (pr-table [(format "if-let %s" '~(first exprs))
                  (str ~(boolean (second bindings)))]
         (conj (mapv vector
                 ~(mapv (fn [x] `(quote ~x)) syms)
                 vals#)
               ['~'RESULT res#]))
       (throw-maybe res#))))

(defmacro pr-if
  "Acts like regular clojure.core/if, but prints out the result of the
  test-clause, the expression of the succeeding if-branch and the result of the
  expression as a table.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  ([test x] `(pr-if ~test ~x nil))
  ([test x y]
   (let [tst `(pass-exception ~test)]
     `(let [res# (when-not (exception? ~tst)
                   (if ~tst
                     ['~x (pass-exception ~x)]
                     ['~y (pass-exception ~y)]))]
        (pr-table [(format "if %s" '~test) (str ~tst)] [res#])
        (throw-maybe (second res#))))))

(defmacro pr-if-not
  "Acts like regular clojure.core/if-not, but prints out the result of the
  test-clause, the expression of the succeeding if-branch and the result of the
  expression as a table.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  ([test x] `(pr-if-not ~test ~x nil))
  ([test x y]
   (let [tst `(pass-exception ~test)]
     `(let [res# (when-not (exception? ~tst)
                   (if-not ~tst
                     ['~x (pass-exception ~x)]
                     ['~y (pass-exception ~y)]))]
        (pr-table [(format "if-not %s" '~test) (str ~tst)] [res#])
        (throw-maybe (second res#))))))

(defmacro pr-when
 "Acts like regular clojure.core/when, but prints out the result of the
  test-clause, the expression in the body and the value it produced as a table.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  [test & body]
  (let [tst `(pass-exception ~test)]
    `(let [res# (when-not (exception? ~tst)
                  (pass-exception
                    (when ~tst ~@body)))]
       (pr-table [(format "when %s" '~test) (str ~tst)]
         [[(if (> (count '~body) 1)
             '~(cons 'do body)
             '~(first body))
           res#]])
       (throw-maybe res#))))

(defmacro pr-when-not
  "Acts like regular clojure.core/when-not, but prints out the result of the
  test-clause, the expression in the body and the value it produced as a table.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  [test & body]
  (let [tst `(pass-exception ~test)]
    `(let [res# (when-not (exception? ~tst)
                  (pass-exception
                    (when-not ~tst ~@body)))]
       (pr-table [(format "when-not %s" '~test) (str ~tst)]
         [[(if (> (count '~body) 1)
             '~(cons 'do body)
             '~(first body))
           res#]])
       (throw-maybe res#))))

(defmacro pr-cond
  "Like regular clojure.core/cond, but prints out the results of each cond-pair
  as a table. Handles exceptions gracefully.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  [& body]
  {:pre [(even? (count body))]}
  (let [header  ["cond" "test" "result"]
        binds   (mapcat
                  (fn [[tst expr]]
                    ;; Executing the tests-exprs multiple times might be
                    ;; expensive so using delays to avoid doing that.
                    [(gensym "test__") `(delay (pass-exception-bool ~tst))
                     (gensym "expr__") `(delay (pass-exception ~expr))])
                  (partition 2 body))
        parts   (partition 4 binds)
        tst-row (fn [[ts td]]
                  `['~(second (second td)) ; quoted test
                    (deref ~ts)            ; test result
                    ""])
        exp-row (fn [[ts td es ed]]
                  `['~(second (second td)) ; quoted test
                    (deref ~ts)            ; test result
                    (if (deref ~ts) (deref ~es) "")])]
    `(let ~(vec binds)
       (cond
         ~@(mapcat (fn [[ts td es ed] i]
                     (let [[tst-row exp-row]
                           ((juxt (partial mapv tst-row)
                                  (partial mapv exp-row)) (take i parts))]
                       [`(exception? (deref ~ts))
                        `(do (pr-table ~header ~tst-row)
                             (throw (deref ~ts)))
                        `(deref ~ts)
                        `(do (pr-table ~header ~exp-row)
                             (if (exception? (deref ~es))
                               (throw (deref ~es))
                               (deref ~es)))]))
             parts (iterate inc 1))))))

(defmacro pr-and
  "Like clojure.core/and, but prints out the result of each of the clauses, plus
  the result of the overall and-expression as a table.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  [& clauses]
  (let [binds (mapcat (fn [c]
                        [(gensym "cls__")
                         `(delay (pass-exception ~c))])
                clauses)
        res   [(gensym "res__")
               `(pass-exception
                  (and ~@(map (fn [gs] `(throw-maybe (deref ~gs)))
                           (take-nth 2 binds))))]]
    `(let ~(vec (concat binds res))
       (pr-table ["and" "result"]
         ~(conj (mapv (fn [c dc] `['~c (deref ~dc)])
                  clauses
                  (take-nth 2 binds))
                [''RESULT (first res)]))
       (throw-maybe ~(first res)))))

(defmacro pr-or
  "Like clojure.core/or, but prints out the result of each of the clauses, plus
  the result of the overall or-expression as a table.

  Should not be used if any of the forms contain side-effects, as this might
  result in the missiles getting launched multiple times."
  [& clauses]
  (let [binds (mapcat (fn [c]
                        [(gensym "cls__")
                         `(delay (pass-exception ~c))])
                clauses)
        res   [(gensym "res__")
               `(pass-exception
                  (or ~@(map (fn [gs] `(throw-maybe (deref ~gs)))
                          (take-nth 2 binds))))]]
    `(let ~(vec (concat binds res))
       (pr-table ["or" "result"]
         (reduce (fn [ret# [s# expr#]]
                   ;; this works bc Exceptions evaluate to true
                   (if-let [v# (deref s#)]
                     (reduced (conj ret# [expr# v#]))
                     (conj ret# [expr# (deref s#)])))
           [] ~(mapv (fn [[s expr]] [s `(quote ~(second (second expr)))])
                 (partition 2 binds))))
       (throw-maybe ~(first res)))))

