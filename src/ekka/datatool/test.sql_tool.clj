(ns ekka.lib.datatool.sql-tool-test-module
  (:require
   [clojure.string :as string]))


;;; PARSER RULES 
(def ^:dynamic *accepted-select-rules* [:column :inner-join :right-join :left-join :outer-left-join :outer-right-join :where])
(def ^:dynamic *accepted-update-rules* [:update-table :set :where])
(def ^:dynamic *accepted-insert-rules* [:values :set])
(def ^:dynamic *accepted-delete-rules* [:where])
(def ^:dynamic *where-border* false)


;;;;;;;;;;;;;;;;;;;;;;;
;;; String helperts ;;;
;;;;;;;;;;;;;;;;;;;;;;;


(defn pair-where-pattern
  "Constuct key-value string view for SQL language parametrized queries
  Example:
  (pair-where-pattern :TABLE :name \"value\")
  ;;=> TABLE.name=\"value\"
  (pair-where-pattern :TABLE :some-bool true)
  ;;=> TABLE.some-bool=true
  "
  ([key value table] (str (symbol table) "." (pair-where-pattern key value)))
  ([key value] (format (cond
                         (string? value) "%s=\"%s\""
                         (or (boolean? value) (number? value)) "%s=%s"
                         :else "%s=%s") (symbol key) value)))

(defn tkey
  "Function split dot-linked keyword name
  and return array of string, divided on <.>
  character
  :table.value => ('table' 'value')"
  [k] (string/split (str (symbol k)) #"\."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Joining Preprocessor ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn join-keyword-string [main-table joining-table]
  (let [[table join-column] (list (symbol joining-table)
                                  (symbol (str "id_" (string/lower-case (symbol joining-table)))))]
    (format "%s ON %s.id=%s.%s" table table main-table join-column)))

(defn join-string-string [main-table joining-table]
  (let [[table join-column] (list joining-table
                                  (str "id_" (string/lower-case joining-table)))]
    (format "%s ON %s.id=%s.%s" table table main-table join-column)))

(defn join-map-keyword-string [main-table [k v]]
  (let [[table join-column] (list (symbol k) (symbol v))]
    (format "%s ON %s.id=%s.%s" table table main-table join-column)))

(defn join-vector-keyword-string [main-table joining-table]
  (let [[table join-column] (list (symbol joining-table)
                                  (symbol (str "id_" (string/lower-case (symbol joining-table)))))]
    (format "%s ON %s.id=%s.%s" table table main-table join-column)))

(defn join-vector-string-string [main-table on-join-construction]
  on-join-construction)

(defn join-dot-map-string [main-table [k v]]
  (if-let [[[t1 id1] [t2 id2]]
           (and (some #(= \. %) (str k))
                (some #(= \. %) (str v))
                (list (string/split (str (symbol k)) #"\.")
                      (string/split (str (symbol v)) #"\.")))]
    (format "%s ON %s=%s" t1 (str (symbol k)) (str(symbol v)))))

(defn get-function-by-join-type [join]
 (cond (keyword? join) join-keyword-string
       (string? join) join-string-string
       (map? join) (if-let [value-of-key (second (first join))]
                     (when (keyword? value-of-key)
                       (if (and (some #(= \. %1) (str value-of-key))
                                (some #(= \. %1) (str (first (first join)))))
                         join-dot-map-string
                         join-map-keyword-string)))
       (vector? join) (if-let [first-value (first join)]
                        (cond (keyword? first-value) join-vector-keyword-string
                              (string? first-value) join-vector-string-string))))

;; (let [t "USER"
;;       j "SUKA"
;;       f (get-function-by-join-type j)]
;;   (f t j))

;; (let [t "USER"
;;       j {:CREDENTIAL :id_credential
;;          :META :id_metadata}
;;       f (get-function-by-join-type j)]
;;   (map #(f t %1) j))

;; (let [t "USER"
;;       j [:CREDENTIAL :OtherTable]
;;       f (get-function-by-join-type j)]
;;   (map #(f t %1) j))

;; (let [t "USER"
;;       j {:CREDENTIAL.id_usk :OtherTable.id_curwa}
;;       f (get-function-by-join-type j)]
;;   (map #(f t %1) j))

;; (let [t "USER"
;;       j ["user ON user.id_user"
;;          "suka ON suka.is=user.id"]
;;       f (get-function-by-join-type j)]
;;   (map #(f t %1) j))


;; (defn join-rule-string [join-type join-string]
;;   (fn [current-string sql-dictionary table-name]
;;     (str current-string
;;          (if-let [joins (get sql-dictionary join-type)]
;;            (if-let [join-function (get-function-by-join-type joins)]
;;              (if (and (seqable? joins) (not (string? joins)))
;;                (str " " (string/join " " (map #(str join-string " " (join-function table-name %1)) joins)))
;;                (str " " join-string " " (join-function table-name joins))))))))


(defmacro define-joinrule [rule-name]
  (let [rule-array (string/split (str rule-name) #"\-")  rule-lenght (- (count rule-array) 1)
        rule-keyword (keyword (string/join "-"(take rule-lenght rule-array)))
        rule-string (string/join " " (map string/upper-case (take rule-lenght rule-array)))]
    `(defmacro ~rule-name [~'current-string ~'joins-form ~'table-name]
       (if-not (symbol? ~'joins-form)
         (if-let [join-function# (get-function-by-join-type ~'joins-form)]
           (if (and (seqable? ~'joins-form) (not (string? ~'joins-form)))
             `(str ~~'current-string " " (string/join " " (map (fn [~'s#] (str ~~rule-string " " (~join-function# (name ~~'table-name) ~'s#))) ~~'joins-form)))
             `(str ~~'current-string " " ~~rule-string " " (~join-function# (name ~~'table-name) ~~'joins-form))))
         `(if-let [~'join-function# (get-function-by-join-type ~~'joins-form)]
            (if (and (seqable? ~~'joins-form) (not (string? ~~'joins-form)))
              (str ~~'current-string " " (string/join " " (map (fn [~'s#] (str ~~rule-string " " (~'join-function# (name ~~'table-name) ~'s#))) ~~'joins-form)))
              (str ~~'current-string " " ~~rule-string " " (~'join-function# (name ~~'table-name) ~~'joins-form))))))))

;; (define-joinrule left-join-string)

;; (let [suka "dukra"]
;;   (left-join-string "12" [:SUKA :BLIAT] "mytable"))
;; (left-join-string "12" ["SUKA ON mytable.id_suka = SUKA.id "] "mytable")
;; (left-join-string "SELECT" :T2 "T1")
;; (let [s [:ONE :TWO]]
;;   (left-join-string "12" s "mytable"))

;; (defsqljoinrule* inner-join-string)
;; DEPRECATED
;; (defn join-rule-string* [join-type join-string]
;;   (fn [current-string sql-dictionary table-name]
;;     (str current-string
;;          (if-let [joins (get sql-dictionary join-type)]
;;            (if-let [join-function (get-function-by-join-type joins)]
;;              (if (and (seqable? joins) (not (string? joins)))
;;                (str " " (string/join " " (map #(str join-string " " (join-function table-name %1)) joins)))
;;                (str " " join-string " " (join-function table-name joins))))))))


;; (defmacro defsqljoinrule [rule-name]
;;   (let [rule-array (string/split (str rule-name) #"\-")  rule-lenght (- (count rule-array) 1)
;;         rule-keyword (keyword (string/join "-"(take rule-lenght rule-array)))
;;         rule-string (string/join " " (map string/upper-case (take rule-lenght rule-array)))]
;;     `(def ~rule-name
;;        (join-rule-string ~rule-keyword ~rule-string))))

(define-joinrule inner-join-string)
(define-joinrule left-join-string)
(define-joinrule right-join-string)
(define-joinrule outer-right-join-string)
(define-joinrule outer-left-join-string)

;; (let [oljf {:CREDENTIAL :is_user_metadata}]
;;   (outer-left-join-string "SELECT * FROM user" oljf "user"))

 
;; (outer-left-join-string "SELECT * FROM user" {:CREDENTIAL :is_user_metadata} "user")

;; (outer-left-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
;;                                                  :suka 2
;;                                                  :METADATA.merried true}
;;                                          :column [:bliat :suka]
;;                                          :outer-left-join {:CREDENTIAL :is_user_metadata
;;                                                       :METADATA :id_user_metadata}} "user")

;; (inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
;;                                                  :suka 2
;;                                                  :METADATA.merried true}
;;                                          :column [:bliat :suka]
;;                                          :inner-join {:CREDENTIAL.id_self :user.id_user_credential
;;                                                       :METADATA.id_self :USER.id_user_metadata}} "user")

;; (inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
;;                                                  :suka 2
;;                                                  :METADATA.merried true}
;;                                          :column [:bliat :suka]
;;                                          :inner-join [:CREDENTIAL :METADATA]} "user")

;; (inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
;;                                                  :suka 2
;;                                                  :METADATA.merried true}
;;                                          :column [:bliat :suka]
;;                                          :inner-join :suka} "user")

;; (inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
;;                                                  :suka 2
;;                                                  :METADATA.merried true}
;;                                          :column [:bliat :suka]
;;                                          :inner-join "suka"} "user")

;; (inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
;;                                                  :suka 2
;;                                                  :METADATA.merried true}
;;                                          :column [:bliat :suka]
;;                                          :inner-join ["suka ON suka.id=user.id_suka"
;;                                                       "dupa ON dupa.id=user.id_dupara"]} "user")

;; (inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
;;                                                  :suka 2
;;                                                  :METADATA.merried true}
;;                                          :column [:bliat :suka]
;;                                          :inner-join {:CREDENTIAL :id_credential
;;                                                       :METADATA :id_metadata}} "user")

;; (inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
;;                                                  :suka 2
;;                                                  :METADATA.merried true}
;;                                          :column [:bliat :suka]
;;                                          :inner-join ["INNER JOIN XXX ON user.id_columnt=XXX.id"]} "user")

;; (left-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
;;                                                 :suka 2
;;                                                 :METADATA.merried true}
;;                                         :column [:bliat :suka]
;;                                         :left-join [:CREDENTIAL :METADATA]} "user")


;;; -- DEPRECATED
;; (defn column-string [current-string sql-dictionary table-name]
;;   (str current-string
;;        (if-let [columns (get sql-dictionary :column)]
;;          (str " " (string/join ", " (map (comp str symbol) columns)))
;;          " *") " FROM " table-name))


(defmacro column-string [current-string col-vec table-name]
  `(str ~current-string " " (string/join ", " (map (comp str symbol) ~col-vec)) " FROM " (name ~table-name)))

(defmacro empty-select-string [current-string _ table-name]
  `(str ~current-string " * FROM " (name ~table-name)))

;; (column-string "SELECT" {:where {:bliat "slia" :suka 2 :what? true}
;;                          :column [:bliat :suka]
;;                          :join-on {:CREDENTIAL :id_credential}} "user")

;; (column-string "SELECT" [:bliat :suka] "user")

;; (select :kurwa
;;         :column [:one :two :three]
;;         :where (= 1 2))

;;;;;;;;;;;;;
;;; Where ;;;
;;;;;;;;;;;;;

(defn into-border [some-string]
  (if *where-border* 
    (format "(%s)" some-string)
    some-string))

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
        (symbol? where-clause) where-clause
        (string? where-clause) `(format "\"%s\"" ~where-clause)
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



;;; to test
;; (where-procedure-parser [1 2 3])
;; (where-procedure-parser (in :skur [1 2 3]))
;; (where-procedure-parser (or (between :user "suka" (+ 2 3)) (= 4 31)))
;; (where-procedure-parser (or 1 2))
;; (where-procedure-parser (> 1 2))
;; (where-procedure-parser (between :bqliat 1 (= 1 2)))
;; (where-procedure-parser (between :user 2 1))
;; (where-procedure-parser (between :user 2 1))
;; (where-procedure-parser (like :suka "blait"))
;; (where-procedure-parser (< :suka "blait"))
;; (where-procedure-parser 1)
;; (where-procedure-parser (+ 1 2))
;; (where-procedure-parser 2)
;; (where-procedure-parser [1 2 3 4])
;; (where-procedure-parser (or (= 1 2) (> 2 (- 3 3))))
;; (where-procedure-parser (or (= :suka 1)
;;                             (= :some "bliat")
;;                             (and (= :suka 2)
;;                                  (= :some (= 1 "fuck"))
;;                                  (between :one 1 2))))
;; (where-procedure-parser (or (= :f1 1)
;;                             (>= :f1 "bliat")
;;                             (and (> :f2 2)
;;                                  (= :f2 "fuck")
;;                                  (between :f1 1 (+ 10 1000))
;;                                  (or (= :suka "one")
;;                                      (in :one [1 2 3 (+ 1 2)])))))



(defn between-procedure [field v1 v2]
  (format "%s BETWEEN %s AND %s"
          (eval `(where-procedure-parser ~field))
          (eval `(where-procedure-parser ~v1))
          (eval `(where-procedure-parser ~v2))))

(defn define-operator [operator field-1 field-2]
  (string/join " " [(eval `(where-procedure-parser ~field-1))
                    operator
                    (eval `(where-procedure-parser ~field-2))]))



;; DEPRECATED
;; (defn where-string [current-string sql-dictionary table-name]
;;   (str current-string
;;        (eval 
;;         (if-let [key-where (get sql-dictionary :where)]
;;           (do (println key-where)
;;             (cond (string? key-where) `(str " WHERE " ~key-where)
;;                   (map? key-where) (if (empty? (get sql-dictionary :join-on))
;;                                      `(str " WHERE " (string/join " AND " (map #(apply pair-where-pattern %) (seq ~key-where))))
;;                                      `(str " WHERE " (string/join " AND " (map #(let [[k v] %]
;;                                                                                   (if (string/includes? k ".")
;;                                                                                     (pair-where-pattern k v)
;;                                                                                     (pair-where-pattern k v ~table-name)))
;;                                                                                (seq ~key-where)))))
;;                   (seqable? key-where) `(str " WHERE " (where-procedure-parser ~key-where))))))))


;;; DEPRECATED
;; (defmacro where-string [s where-block table-name]
;;   (cond (string? where-block) `(str ~s " WHERE " ~where-block)
;;         (map? where-block) (if (empty? (get where-block :join-on))
;;                            `(str ~s " WHERE " ~(string/join " AND " (map #(apply pair-where-pattern %) (seq ~where-block))))
;;                            `(str ~s " WHERE " ~(string/join " AND " (map #(let [[k v] %]
;;                                                                             (if (string/includes? k ".")
;;                                                                               (pair-where-pattern k v)
;;                                                                               (pair-where-pattern k v ~table-name)))
;;                                                                          (seq ~where-block)))))
;;         (seqable? where-block) `(str ~s " WHERE " (where-procedure-parser ~where-block))))

(defmacro where-string [s where-block table-name]
  (if (symbol? where-block)
    `(cond (string? ~where-block) (str ~s " WHERE " ~where-block)
           (map? ~where-block) (str ~s " WHERE " (string/join " AND " (map #(apply pair-where-pattern %) ~where-block)))
           (seqable? ~where-block) (str ~s " WHERE " (where-procedure-parser ~where-block)))
    (cond (string? where-block) `(str ~s " WHERE " ~where-block)
          (map? where-block) `(str ~s " WHERE " (string/join " AND " (map #(apply pair-where-pattern %) ~where-block)))
          (seqable? where-block) `(str ~s " WHERE " (where-procedure-parser ~where-block)))))

;; (where-string "" {:login "admin"} "table")

;; (let [ll {:login.com "admin" :hcuj 123}]
;;   (where-string "" ll "table"))

;; (let [k [1 2 3]]
;;   (where-procedure-parser [1 2 3 k]))




;; (defmacro bliat [key]
;;   (let [x `'{:where (= :costam ~key)}]
;;    `(where-string "" ~x "table")))

;; (select :suka
;;         :where (= :costam "dsaf"))

;; (let [sql-dictionary '{:where (= :costam "dsaf")}]
;;   (get `~sql-dictionary :where))

;; (bliat "costfdaam")

;; (where-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
;;                                             :suka 2
;;                                             :METADATA.merried true}
;;                                     :column [:bliat :suka]
;;                                     :join-on {:CREDENTIAL :id_credential
;;                                               :METADATA :id_metadata}} "user")

;; (where-string "SELECT * FROM user" {:column [ :bliat :suka]
;;                                     :join-on {:CREDENTIAL :id_credential
;;                                               :METADATA :id_metadata}} "user")

;; (where-string "SELECT * FROM user" '{:where {:CREDENTAIL.login "anatoli"
;;                                             :suka 2
;;                                             :METADATA.merried true}} "user")

;; (where-string "SELECT * FROM user"
;;               '{:where (or (= :f1 1)
;;                           (>= :f1 "bliat")
;;                           (and (> :f2 2)
;;                                (= :f2 "fuck")
;;                                (between :f1 1 (+ 10 1000))
;;                                (or (= :suka "one")
;;                                    (in :one [1 2 3 (+ 1 2)]))))} "user")







;; (defn create-rule-pipeline [keys accepted-lexical-rule]
;;   (let [key-in? (fn [k col]
;;                   (when (some #(= k %) col)
;;                     (symbol (str (symbol k) "-string"))))]
;;     (reduce #(if-let [k (key-in? %2 keys)] (conj %1 k) %1) [] accepted-lexical-rule)))

(defn create-rule-pipeline [keys accepted-lexical-rule]
  (let [key-in? (fn [k col] (when (some #(= k %) col) (symbol (str (symbol k) "-string"))))]
    (reduce #(if-let [k (key-in? %2 keys)] (conj %1 [%2 k]) %1) [] accepted-lexical-rule)))

;; (create-rule-pipeline [:where :column] *accepted-select-rules*)

(defn select-empty-table-pipeline-applier [key-pipeline]
  (if (some #(= :column (first %1)) key-pipeline)
    key-pipeline
    (vec (concat [[:column 'empty-select-string]] key-pipeline))))

;; (let [rules (create-rule-pipeline [:where :column ] *accepted-select-rules*)]
;;   (supply-other-rule rules))

;; (defmacro select [table-name & {:as args}]
;;   (let [table-name-symb (gensym 'table-name)
;;         dictionary-symb (gensym 'args)
;;         list-of-rules (create-rule-pipeline (keys args) *accepted-select-rules*)]
;;     `(let [~table-name-symb (symbol ~table-name)
;;            ~dictionary-symb '~args]
;;        (eval (-> "SELECT"
;;                  ~@(for [F list-of-rules]
;;                      ;; `(~F ~dictionary-symb ~table-name-symb)
;;                      `(~F ~args ~table-name)
;;                      ))))))

;; (defmacro select [table-name & {:as args}]
;;   (let [RULE-GENERATOR (comp supply-other-rule create-rule-pipeline)
;;         list-of-rules (RULE-GENERATOR (keys args) *accepted-select-rules*)]
;;     `(eval (-> "SELECT"
;;                ~@(for [[k F] list-of-rules]
;;                    `(~F ~(k args) ~table-name))))))

;; (let [x 123]
;;     (select :suka
;;             :left-join {:METADATA :id_metadata} 
;;             :inner-join :TEMPORARY
;;             :where (= :key x)))

;; (let [columns [:name :dla_mamusi]
;;       tbl :user_table
;;       passwd "ALEKSANDR_BOG69"
;;       where-clouse {:one "one" :two 2 :three true}
;;       lf {:METADATA :id_metadata}]
;;     (select tbl
;;             :left-join lf
;;             :inner-join {:CREDENTIAL :id_credential}
;;             ;; :column [:name :dla_mamusi :CREDENTAIL.login]
;;             :column columns
;;             :where where-clouse
;;             ;; :where (= :key 123)
;;             ;; :where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
;;             ;;         ;; :CREDENTAIL.password "Aleksandr_Bog69"
;;             ;;         :CREDENTIAL.password passwd
;;             ;;         :name "Aleksandr"
;;             ;;         :dla_mamusi "Olek"
;;             ;;         :METADATA.merried false}
;;             ))


;; (select :user_table)

;; (select :user_table
;;         :left-join {:METADATA :id_metadata} 
;;         :inner-join {:CREDENTIAL :id_credential}
;;         :column [:name :dla_mamusi :CREDENTAIL.login]
;;         :where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
;;                 :CREDENTAIL.password "Aleksandr_Bog69"
;;                 :name "Aleksandr"
;;                 :dla_mamusi "Olek"
;;                 :METADATA.merried false})

;; (select :user-table
;;         :inner-join {:CREDENTIAL :is_user_metadata :METADATA :id_user_metadata}
;;         :right-join {:A1.id_self :user.id_user_a1 :B1.id_self :USER.id_user_b2}
;;         :left-join ["suka ON suka.id=user.id_suka" "dupa ON dupa.id=er.id_dupara"]
;;         :outer-left-join [:suka :bliat]
;;         :outer-right-join :credential
;;         :column [:name :dla_mamusi :CREDENTAIL.login]
;;         :where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
;;                 :CREDENTAIL.password "Aleksandr_Bog69"
;;                 :name "Aleksandr"
;;                 :dla_mamusi "Olek"
;;                 :METADATA.merried false})

;; (select :user-table
;;         :inner-join {:CREDENTIAL :is_user_metadata :METADATA :id_user_metadata}
;;         :right-join {:A1.id_self :user.id_user_a1 :B1.id_self :USER.id_user_b2}
;;         :left-join ["suka ON suka.id=user.id_suka" "dupa ON dupa.id=er.id_dupara"]
;;         :outer-left-join [:suka :bliat]
;;         :outer-right-join :credential
;;         :column [:name :dla_mamusi :CREDENTAIL.login]
;;         :where (or (= :f1 1)
;;                    (>= :f1 "bliat")
;;                    (and (> :f2 2)
;;                         (= :f2 "fuck")
;;                         (between :f1 1 (+ 10 1000))
;;                         (or (= :suka "one")
;;                             (in :one [1 2 3 (+ 1 2)])))))

;; (select :user-table
;;         :inner-join {:CREDENTIAL :is_user_metadata :METADATA :id_user_metadata}
;;         :right-join {:A1.id_self :user.id_user_a1 :B1.id_self :USER.id_user_b2}
;;         :left-join ["suka ON suka.id=user.id_suka" "dupa ON dupa.id=er.id_dupara"]
;;         :outer-left-join [:suka :bliat]
;;         :outer-right-join :credential
;;         :column [:name :dla_mamusi :CREDENTAIL.login]
;;         :where "SOME.id=one AND SOME_THER.one=4")



;;;;;;;;;;;;;;
;;; UPDATE ;;;
;;;;;;;;;;;;;;

(defmacro set-string [current-string update-map tabel-name]
  `(str ~current-string " "
        (symbol ~tabel-name)
        " SET "
        (string/join ", " (map #(apply pair-where-pattern %)  ~update-map))))

(defmacro update-table-string [current-string map table-name]
  `(str ~current-string "" ~table-name))

(defmacro low-priority-string [current-string map table-name]
  `(str ~current-string " LOW_PRIORITY"))

;; (defmacro update [table-name & {:as args}]
;;   (let [table-name-symb (gensym 'table-name)
;;         dictionary-symb (gensym 'args)
;;         list-of-rules (create-rule-pipeline (keys args) *accepted-insert-rules*)]
;;     `(let [~table-name-symb (symbol ~table-name)
;;            ~dictionary-symb '~args]
;;        (eval (-> "UPDATE"
;;                  ~@(for [F list-of-rules]
;;                      `(~F ~dictionary-symb ~table-name-symb)))))))


;;;;;;;;;
;; OLD ;;
;;;;;;;;;

;; (defmacro update [table-name & {:as args}]
;;   (let [RULE-GENERATOR #'create-rule-pipeline
;;         list-of-rules (RULE-GENERATOR (keys args) *accepted-udpate-rules*)]
;;     `(eval (-> "UPDATE"
;;                ~@(for [[k F] list-of-rules]
;;                    `(~F ~(k args) ~table-name))))))

;; (update :user
;;         :set {:login "aleks"
;;               :password "XXXpussy_destroyer69"}
;;         :where {:id 2 :bliat "costam"})

;; (let [sset {:login "aleks"
;;             :password "XXXpussy_destroyer69"}
;;       where {:id 2 :bliat "costam"}]
;;   (update :user
;;           :set sset
;;           :where where))


;;;;;;;;;;;;;;
;;; INSERT ;;;
;;;;;;;;;;;;;;

(defmacro values-string [current-string values table-name]
  (let [into-sql-values (fn [some-list]
                          (str "(" (string/join ", " (map #(eval `(where-procedure-parser ~%)) some-list)) ")"))
        into-sql-map (fn [some-list]
                       (str "(" (string/join ", " (map #(eval `(where-procedure-parser ~%)) (vals some-list))) ")"))]
    `(str ~current-string " " ~(name table-name)
          (cond (map? ~values)
                (str " SET "      (string/join ", " (map #(apply pair-where-pattern %) ~values)))

                (and (seqable? ~values) (map? (first ~values)))
                (str " VALUES "   (string/join ", " (map ~into-sql-map ~values)))

                (and (seqable? ~values) (seqable? (first ~values)))
                (str " VALUES "   (string/join ", " (map ~into-sql-values ~values)))

                (seqable? ~values)
                (str " VALUES "   (~into-sql-values ~values))

                :else nil))))

;; (values-string "" {:a 1 :b 2 :c 3} "mitinko")
;; (values-string "" [{:a 1 :b 2 :c 3} {:a 4 :b 5 :c 6}] "mitinko")
;; (values-string "" [[1 1 1] [2 2 2]] "mitinko")
;; (values-string "" [1 1 1] "mitinko")

;; (let [v1 {:a 1 :b 2 :c 3}
;;       v2 [{:a 1 :b 2 :c 3} {:a 4 :b 5 :c 6}]
;;       v3 [[1 1 1] [2 2 2]]
;;       v4 [1 1 1]]
;;   (values-string "" v1 "mitinko"))

;;;;;;;;;
;; OLD ;;
;;;;;;;;;

;; (defmacro insert [table-name & {:as args}]
;;   (let [RULE-GENERATOR #'create-rule-pipeline
;;         list-of-rules (RULE-GENERATOR (keys args) *accepted-insert-rules*)]
;;     `(eval (-> "INSERT INTO"
;;                ~@(for [[k F] list-of-rules]
;;                    `(~F ~(k args) ~table-name))))))

;; (insert :user
;;         :values {:id 1 :login "vasia" :password "123" :age 20})

;; (insert :user
;;         :values [[1, "vasia", "123", 20] [2, "vasia", "123", 20]])

;; (insert :user
;;         :values [{:id 1 :login "vasia" :password "123" :age 20} {:id 1 :login "vasia" :password "123" :age 20}])



;;;;;;;;;;;;;;
;;; DELETE ;;;
;;;;;;;;;;;;;;

;; (defmacro delete [table-name & {:as args}]
;;   (let [RULE-GENERATOR #'create-rule-pipeline
;;         list-of-rules (RULE-GENERATOR (keys args) *accepted-delete-rules*)]
;;     `(eval (-> "DELETE"
;;                ~@(for [[k F] list-of-rules]
;;                    `(~F ~(k args) ~table-name))))))

(defmacro define-sql-operation [operation-name accepted-rules pipeline-function]
  `(defmacro ~operation-name [~'table-name & {:as ~'args}]
     (let [list-of-rules# (~pipeline-function (keys ~'args) ~accepted-rules)]
       `(eval (-> ~~(string/upper-case (name operation-name))
                  ~@(for [[~'k ~'F] list-of-rules#]
                      `(~~'F ~~'(k args) ~~'table-name)))))))

(define-sql-operation insert *accepted-insert-rules* create-rule-pipeline)
(define-sql-operation delete *accepted-delete-rules* create-rule-pipeline)
(define-sql-operation update *accepted-update-rules* create-rule-pipeline)
(define-sql-operation select *accepted-select-rules* (comp select-empty-table-pipeline-applier create-rule-pipeline))


;; (select :user_table)

;; (select :user_table
;;         :left-join {:METADATA :id_metadata} 
;;         :inner-join {:CREDENTIAL :id_credential}
;;         :column [:name :dla_mamusi :CREDENTAIL.login]
;;         :where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
;;                 :CREDENTAIL.password "Aleksandr_Bog69"
;;                 :name "Aleksandr"
;;                 :dla_mamusi "Olek"
;;                 :METADATA.merried false})


;; (let [w1 {:id 12}]
;;     (delete :user
;;             :where w1))


;; (let [a {:left-join {:METADATA :id_metadata} 
;;         :inner-join {:CREDENTIAL :id_credential}
;;         :column [:name :dla_mamusi :CREDENTAIL.login]
;;         :where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
;;                 :CREDENTAIL.password "Aleksandr_Bog69"
;;                 :name "Aleksandr"
;;                 :dla_mamusi "Olek"
;;                 :METADATA.merried false}}]
;;   (keys a))



;; (column-string "SELECT" {} "user")



;; (alter-table :user
;;              :drop-column :bliat)

;; (alter-table :user
;;              :drop-foreign-key :bliat)

;; (alter-table :user
;;              :add-foreign-key [{:id_permission :permission} {:update :cascade}])

;; (alter-table :user
;;              :add-column {:suka [:boolean]})

;; (select :user_table)


;; (select :user_table
;;         :left-join {:METADATA :id_metadata} 
;;         :inner-join {:CREDENTIAL :id_credential}
;;         :column [:name :dla_mamusi :CREDENTAIL.login]
;;         :order [:name :desc]
;;         :where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
;;                 :CREDENTAIL.password "Aleksandr_Bog69"
;;                 :name "Aleksandr"
;;                 :dla_mamusi "Olek"
;;                 :METADATA.merried false})

;; (create-table :table
;;               :columns [{:name [:varchar-100 :null]}
;;                         {:some-int :integer-200}
;;                         {:id_other_table :bigint-20}]
;;               :foreign-keys [{:id_other_table :other_table} {:update :cascade :delete :null}]
;;               :table-config {:engine "InnoDB" :charset "utf8"})



