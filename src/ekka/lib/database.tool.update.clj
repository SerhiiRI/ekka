(ns ekka.database.update
  (:require
   [clojure.string :as string]
   ;; [ekka.database.tool :as tool]
   ))


(def ^:dynamic *accepted-udpate-rules* [:update-table :set :where])
(def ^:dynamic *accepted-insert-rules* [:values :set])
(def ^:dynamic *accepted-delete-rules* [:where])
(def ^:dynamic *where-border* false)

(defn pair-where-pattern
  ([key value table] (str (symbol table) "." (pair-where-pattern key value)))
  ([key value] (format (cond
                         (string? value) "%s='%s'"
                         (or (boolean? value) (number? value)) "%s=%s"
                         :else "%s=%s") (symbol key) value)))

(defmacro and-processor [& args]
  (let [v (vec (for [x (vec args)]
                 `(binding [*where-border* true]
                    (where-procedure-parser ~x))))]
    `(into-border (string/join " AND " ~v))))

(defmacro or-processor [& args]
  (let [v (vec (for [x (vec args)]
                 `(binding [*where-border* true]
                    (where-procedure-parser ~x))))]
    `(into-border (string/join " OR " ~v))))

;; (and-processor (= 1 2) (= 1 (- 2 3)))
;; (and-processor 1 2)
;; (or-processor (= 1 2) (= 1 (- 2 3)))
;; (or-processor 1 2)




(defmacro where-procedure-parser [where-clause]
  (cond (nil? where-clause) `(str "null")
        (symbol? where-clause) `(format "%s" (str '~where-clause))
        (string? where-clause) `(format "'%s'" ~where-clause)
        (keyword? where-clause) `(str (symbol ~where-clause))
        (seqable? where-clause) (let [function (first where-clause) args (rest where-clause)]
                                  (condp = function
                                    'or `(or-processor ~@args)
                                    'and `(and-processor ~@args)
                                    '> `(define-operator '~function ~@args)
                                    '< `(define-operator '~function ~@args)
                                    '= `(define-operator '~function ~@args)
                                    '>= `(define-operator '~function ~@args)
                                    '<= `(define-operator '~function ~@args)
                                    '<> `(define-operator '~function ~@args)
                                    '!= `(define-operator '<> ~@args)
                                    'between `(between-procedure ~@args)
                                    'like `(define-operator '~(symbol 'LIKE) ~@args)
                                    'in `(define-operator '~(symbol 'LIKE) ~@args)
                                    'and `(define-operator '~(symbol 'LIKE) ~@args)
                                    (if (and (symbol? function) (resolve function))
                                      (let [result (eval where-clause)]
                                        `(where-procedure-parser ~result))
                                      (let [element-primitives (vec (for [x where-clause]
                                                                      `(where-procedure-parser ~x)))]
                                        `(binding [*where-border* true]
                                           (into-border (string/join ", " ~element-primitives)))))))
        :else (str where-clause)))

(defn into-border [some-string]
  (if *where-border* 
    (format "(%s)" some-string)
    some-string))


(defn between-procedure [field v1 v2]
  (format "%s BETWEEN %s AND %s"
          (eval `(where-procedure-parser ~field))
          (eval `(where-procedure-parser ~v1))
          (eval `(where-procedure-parser ~v2))))

(defn define-operator [operator field-1 field-2]
  (string/join " " [(eval `(where-procedure-parser ~field-1))
                    operator
                    (eval `(where-procedure-parser ~field-2))]))

(defmacro or-processor-map [& args]
  (let [v (vec (for [x (vec args)]
                 `(binding [*where-border* true]
                    (where-procedure-parser ~x))))]
    `(into-border (string/join " OR " ~v))))


(defn where-string [current-string sql-dictionary table-name]
  (str current-string
       (eval 
        (if-let [key-where (get sql-dictionary :where)]
          (do (println key-where)
            (cond (string? key-where) `(str " WHERE " ~key-where)
                  (map? key-where) (if (empty? (get sql-dictionary :join-on))
                                     `(str " WHERE " (string/join " AND " (map #(apply pair-where-pattern %) (seq ~key-where))))
                                     `(str " WHERE " (string/join " AND " (map #(let [[k v] %]
                                                                                  (if (string/includes? k ".")
                                                                                    (pair-where-pattern k v)
                                                                                    (pair-where-pattern k v ~table-name)))
                                                                               (seq ~key-where)))))
                  (seqable? key-where) `(str " WHERE " (where-procedure-parser ~key-where))))))))



(defn column-string [current-string sql-dictionary table-name]
  (str current-string
       (if-let [columns (get sql-dictionary :column)]
         (str " " (string/join ", " (map (comp str symbol) columns)))
         " *") " FROM " table-name))

(defn create-rule-pipeline [keys rules]
  (let [key-in? (fn [k col]
                  (when (some #(= k %) col)
                    (symbol (str (symbol k) "-string"))))]
    (reduce #(if-let [k (key-in? %2 keys)] (conj %1 k) %1) [] rules)))


;;;;;;;;;;;;;;
;;; UPDATE ;;;
;;;;;;;;;;;;;;

(defn set-string [current-string update-map tabel-name]
  (str current-string
       (if-let [set-map (:set update-map)]
         (when-not (empty? set-map) (str " "
                                         (symbol tabel-name)
                                         " SET "
                                         (string/join ", " (map #(apply pair-where-pattern %) (seq set-map))))))))

(defn update-table-string [current-string map table-name]
  (str current-string " " table-name))

(defn low-priority-string [current-string map table-name]
  (str current-string " LOW_PRIORITY"))

(defmacro update-sql [table-name & {:as args}]
  (let [table-name-symb (gensym 'table-name)
        dictionary-symb (gensym 'args)
        list-of-rules (create-rule-pipeline (keys args) *accepted-insert-rules*)]
    `(let [~table-name-symb (symbol ~table-name)
           ~dictionary-symb '~args]
       (eval (-> "UPDATE"
                 ~@(for [F list-of-rules]
                     `(~F ~dictionary-symb ~table-name-symb)))))))



;;;;;;;;;;;;;;
;;; INSERT ;;;
;;;;;;;;;;;;;;

(defn values-string [current-string key-map table-name]
  (str current-string " " table-name
       (if-let [values (:values key-map)]
         (let [into-sql-values
               (fn [some-list]
                 (str "(" (string/join ", " (map #(eval `(where-procedure-parser ~%)) some-list)) ")"))
               into-sql-map
               (fn [some-list]
                 (str "(" (string/join ", " (map #(eval `(where-procedure-parser ~%)) (vals some-list))) ")"))]
           (cond (map? values) (str " SET "(string/join ", " (map #(apply pair-where-pattern %) values)))
                 (and (seqable? values) (map? (first values))) (str " VALUES " (string/join ", " (map into-sql-map values)))
                 (and (seqable? values) (seqable? (first values))) (str " VALUES "(string/join ", " (map into-sql-values values)))
                 (seqable? values) (str " VALUES " (into-sql-values values))
                 :else nil)))))

(defmacro insert [table-name & {:as args}]
  (let [table-name-symb (gensym 'table-name)
        dictionary-symb (gensym 'args)
        list-of-rules (create-rule-pipeline (keys args) *accepted-insert-rules*)]
    `(let [~table-name-symb (symbol ~table-name)
           ~dictionary-symb '~args]
       (eval (-> "INSERT INTO"
                 ~@(for [F list-of-rules]
                     `(~F ~dictionary-symb ~table-name-symb)))))))


;;;;;;;;;;;;;;
;;; DELETE ;;;
;;;;;;;;;;;;;;

(defmacro delete [table-name & {:as args}]
  (let [table-name-symb (gensym 'table-name)
        dictionary-symb (gensym 'args)
        list-of-rules (create-rule-pipeline (keys args) *accepted-delete-rules*)]
    `(let [~table-name-symb (symbol ~table-name)
           ~dictionary-symb '~args]
       (eval (-> (str "DELETE FROM " ~table-name-symb)
                 ~@(for [F list-of-rules]
                     `(~F ~dictionary-symb ~table-name-symb)))))))


;; (defmulti values-string class)
;; (defmethod values-string clojure.lang.PersistentVector [values-vector])
;; (defmethod values-string clojure.lang.PersistentArrayMap [values-map])

;; (= clojure.lang.PersistentVector (class []))
;; (= clojure.lang.PersistentArrayMap (class {})) 


;; (delete :user
;;         :where {:id 12})

;; (update-sql :user
;;             :set {:login "aleks"
;;                   :password "XXXpussy_destroyer69"}
;;             :where {:id 2 :bliat "costam"})

;; (insert :user
;;         :set {:login "aleks"
;;               :password "XXXpussy_destroyer69"}
;;         :where {:id 2 :bliat "costam"})

;; (insert :user
;;         :values [[1, "vasia", "123", 20] [2, "vasia", "123", 20]])

;; (insert :user
;;         :values [{:id 1 :login "vasia" :password "123" :age 20} {:id 1 :login "vasia" :password "123" :age 20}])

