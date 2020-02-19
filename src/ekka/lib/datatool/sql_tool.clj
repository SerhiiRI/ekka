(ns ekka.lib.datatool.sql-tool
  (:require
   [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;
;;; parser rules ;;;
;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *accepted-alter-table-rules* [:add-column :drop-column :drop-foreign-key :add-foreign-key])
(def ^:dynamic *accepted-forkey-rules* [:restrict :cascade :null :no-action :default])
(def ^:dynamic *accepted-ctable-rules* [:columns :foreign-keys :table-config])
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
;;; Joining preprocessor ;;;
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
    (format "%s ON %s.id=%s.%s" table table main-table (name join-column))))

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


(define-joinrule inner-join-string)
(define-joinrule left-join-string)
(define-joinrule right-join-string)
(define-joinrule outer-right-join-string)
(define-joinrule outer-left-join-string)

(defmacro column-string [current-string col-vec table-name]
  `(str ~current-string " " (string/join ", " (map (comp str symbol) ~col-vec)) " FROM " (name ~table-name)))

(defmacro empty-select-string [current-string _ table-name]
  `(str ~current-string " * FROM " (name ~table-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; where preprocessor ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

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



(defn between-procedure [field v1 v2]
  (format "%s BETWEEN %s AND %s"
          (eval `(where-procedure-parser ~field))
          (eval `(where-procedure-parser ~v1))
          (eval `(where-procedure-parser ~v2))))

(defn define-operator [operator field-1 field-2]
  (string/join " " [(eval `(where-procedure-parser ~field-1))
                    operator
                    (eval `(where-procedure-parser ~field-2))]))


(defmacro where-string [s where-block table-name]
  (if (symbol? where-block)
    `(cond (string? ~where-block) (str ~s " WHERE " ~where-block)
           (map? ~where-block) (str ~s " WHERE " (string/join " AND " (map #(apply pair-where-pattern %) ~where-block)))
           (seqable? ~where-block) (str ~s " WHERE " (where-procedure-parser ~where-block)))
    (cond (string? where-block) `(str ~s " WHERE " ~where-block)
          (map? where-block) `(str ~s " WHERE " (string/join " AND " (map #(apply pair-where-pattern %) ~where-block)))
          (seqable? where-block) `(str ~s " WHERE " (where-procedure-parser ~where-block)))))





;;;;;;;;;;;;;;;;;;;;;;;;
;;; set preprocessor ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro set-string [current-string update-map tabel-name]
  `(str ~current-string " "
        (symbol ~tabel-name)
        " SET "
        (string/join ", " (map #(apply pair-where-pattern %)  ~update-map))))

(defmacro update-table-string [current-string map table-name]
  `(str ~current-string "" ~table-name))

(defmacro low-priority-string [current-string map table-name]
  `(str ~current-string " LOW_PRIORITY"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; values preprocessor ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro values-string [current-string values table-name]
  (let [into-sql-values (fn [some-list]
                          (str "(" (string/join ", " (map #(eval `(where-procedure-parser ~%)) some-list)) ")"))
        into-sql-map (fn [some-list]
                       (str "(" (string/join ", " (map #(eval `(where-procedure-parser ~%)) (vals some-list))) ")"))]
    `(str ~current-string " " (name ~table-name)
          (cond (map? ~values)
                (str " SET " (string/join ", " (map #(apply pair-where-pattern %) ~values)))

                (and (seqable? ~values) (map? (first ~values)))
                (str " VALUES " (string/join ", " (map ~into-sql-map ~values)))

                (and (seqable? ~values) (seqable? (first ~values)))
                (str " VALUES " (string/join ", " (map ~into-sql-values ~values)))

                (seqable? ~values)
                (str " VALUES " (~into-sql-values ~values))

                :else nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create-table preprocessor ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn formater
  "Format column table specyficator. Example:
  
  :bigint-120 => BIGINT(120)
  :bigint-signed => BIGINT SIGNED
  :double-4.5 => DOUBLE(4,5)
  :bool => TINYINT(1)
  :nnul => NOT NULL
  ....
  for more, see the code `condp` block
  "
  [k] (if (string? k) k
          (let [[sql-type n s & _] (string/split (string/lower-case (name k)) #"-")
                is? (fn [col x] (if (string? col) (= x col) (some #(= % x) col)))
                charchain-types (fn [tt nn] (if-not nn (string/upper-case tt) (format "%s(%s)" (string/upper-case tt) nn)))
                numeral-types (fn [tt nn] (if-not nn (string/upper-case tt)
                                                  (format (if (is? ["signed" "unsigned" "zerofill"] nn) "%s %s"
                                                              (if (and s (not (empty? s)))
                                                                (if-let [_tmp (formater (keyword s))]
                                                                  (str "%s(%s) " _tmp)
                                                                  "%s(%s)") "%s(%s)"))
                                                          (string/upper-case tt)
                                                          (string/replace (string/upper-case nn) "." "," ))))]
            (condp is? sql-type
              "null"       "NULL"
              "nnull"      "NOT NULL"
              "date"       "DATE"
              "datetime"   "DATETIME"
              "time"       "TIME"
              
              ["tinyint" "smallint"
               "mediumint" "int"
               "integer" "bigint"
               "double" "float"
               "real"] (numeral-types sql-type n)
              
              ["bool" "boolean"] "TINYINT(1)"

              ["tinyblob" "blob" "mediumblob"  "longblob" 
               "tinytext" "text" "mediumtext" "longtext"
               "json"] (string/upper-case sql-type)

              "varchar" (charchain-types sql-type n)

              ["auto_increment" "autoincrement" "auto"] "AUTO_INCREMENT"

              ["default" "signed" "unsigned" "zerofill" ] (string/upper-case sql-type)
              nil))))



(defn create-column
  "Cretea column by map-typed specyfication:

  The key of map is column name, value - is column specyfication, which conctruct to SQL by `formater` function.
  Accepted argument forms
  
  {:id [:bigint-20 \"NOT NULL\" :auto]}  
  {:id \"bigint(20) NOT NULL AUTO_INCREMENT\"}
  {:id [:bigint-20 :nnull :auto]}
  {:id :bigint-290}
  "
  [map-col]
  (let [[[col-name value]] (seq map-col)]
    (cond (keyword? value) (str (format "`%s`" (name col-name)) (if-let [x (formater value)] (str " " x)))
          (string? value)  (str (format "`%s`" (name col-name)) (if-not (empty? value) (str " " value)))
          (seqable? value) (str (format "`%s`" (name col-name)) (let [x (string/join " " (reduce #(if-let [f (formater %2)] (conj %1 f) %1) [] value))]
                                                                  (if-not (empty? x) (str " " x))))
          :else "")))


(defmacro default-table-config-string [current-string _ table-name]
  `(str ~current-string ") ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_general_ci;"))

(defmacro table-config-string
  "Get configuration map with next keys parameters
  :engine - table engine(defautl: InnoDB)
  :charset - charset for table(default: utf8)
  :collate - table collate(default: utf8_general_ci)"
  [current-string conifig-map table-name]
  `(let [{:keys [~'engine ~'charset ~'collate] :or {~'engine "InnoDB"
                                                    ~'charset "utf8"
                                                    ~'collate "utf8_general_ci"}} ~conifig-map]
     (str ~current-string (format ") ENGINE=%s DEFAULT CHARSET=%s COLLATE=%s;" ~'engine ~'charset ~'collate ))))


(defmacro columns-string
  "create columns with type data specyfications.
  Example using for `column-spec` argument:
  
  \"`id` BIGINT(150) NOT NULL AUTO_INCREMENT\"
  {:fuck [:bigint-20 \"NOT NULL\" :auto]}  
  [{:blia [:bigint-20 \"NOT NULL\" :auto]} {:suka [:varchar-210]}]
  [{:id :bigint-100} {:suka \"TINYTEXT\"}]"
  [current-string column-spec table-name]
  `(str ~current-string (format " `%s` (" ~table-name)
        (string/join ", " [(create-column {:id [:bigint-20-unsigned :nnull :auto]}) 
                           (let [cls# ~column-spec]
                             (cond (string? cls#) cls# 
                                   (map? cls#) (create-column cls#)
                                   (vector? cls#) (string/join ", " (map #(create-column %) cls#))
                                   :else nil)) 
                           "PRIMARY KEY (`id`)"])))




(defn constraint-create
  "example
  {:id_permission :permission} {:update :cascade :delete :restrict}"
  ([table-name tables]
   (let [[colm rel-tbl] (map name (first (seq tables)))
         key-name (gensym table-name)]
      (str (format "KEY `%s` (`%s`), " (str key-name) colm)
           (format "CONSTRAINT `%s` FOREIGN KEY (`%s`) REFERENCES `%s` (`id`)" (str key-name) colm rel-tbl))))
  ([table-name tables update-delete]
   (let [on-action #(condp = %2
                      :cascade (format " ON %s CASCADE" %1)
                      :restrict (format " ON %s RESTRICT" %1)
                      :null (format " ON %s SET NULL" %1 )
                      :no-action (format " ON %s NO ACTION" %1)
                      :default (format " ON %s SET DEFAULT" %1) nil)]
     (let [[colm rel-tbl] (map name (first (seq tables)))
           key-name (gensym table-name)
           { on-delete :delete on-update :update} update-delete]
       (str (format "KEY `%s` (`%s`), " key-name colm)
            (format "CONSTRAINT `%s` FOREIGN KEY (`%s`) REFERENCES `%s` (`id`)" (str key-name) colm rel-tbl)
            (if on-delete (on-action "DELETE" on-delete))
            (if on-update (on-action "UPDATE" on-update))
            )))))

(defn alter-table-constraint-create
  "example
  {:id_permission :permission} {:update :cascade :delete :restrict}"
  ([table-name tables]
   (let [[colm rel-tbl] (map name (first (seq tables)))
         key-name (gensym table-name)]
      (str (format "CONSTRAINT `%s` FOREIGN KEY (%s) REFERENCES `%s` (`id`)" (str key-name) colm rel-tbl))))
  ([table-name tables update-delete]
   (let [on-action #(condp = %2
                      :cascade (format " ON %s CASCADE" %1)
                      :restrict (format " ON %s RESTRICT" %1)
                      :null (format " ON %s SET NULL" %1 )
                      :no-action (format " ON %s NO ACTION" %1)
                      :default (format " ON %s SET DEFAULT" %1) nil)]
     (let [[colm rel-tbl] (map name (first (seq tables)))
           key-name (gensym table-name)
           { on-delete :delete on-update :update} update-delete]
       (str (format "CONSTRAINT `%s` FOREIGN KEY (%s) REFERENCES `%s` (`id`)" (str key-name) colm rel-tbl)
            (if on-delete (on-action "DELETE" on-delete))
            (if on-update (on-action "UPDATE" on-update))
            )))))

(defmacro foreign-keys-string
  "Function get specyfication in `foreign-keys` argument and regurn linked foreighn key for two table.
  Foreign key map specyfication by example:
  Constraint by string => \"CONSTRAINT (`id`) blablabla\"
  Constraint by map => [{:id_permission :permission} {:update :cascade :delete :restrict}] 
  Constraint by Vecotor of maps => [[{:id_permission :permission} {:update :cascade :delete :restrict}] [{:id_chujnia :chujnia} {:update :nset :delete :restricted}]]"
  [current-string foreign-keys table-name]
  (cond
    (string? foreign-keys) `(str ~current-string ", " ~foreign-keys)
    (and (vector? foreign-keys) (map? (first foreign-keys))) `(str ~current-string ", " (apply constraint-create ~table-name ~foreign-keys))
    (and (vector? foreign-keys) (vector? (first foreign-keys))) `(str ~current-string ", " (string/join ", " (map #(apply constraint-create ~table-name %) ~foreign-keys)))
    :else current-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; alter-table functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro drop-foreign-key-string
  "Do drop column from table. Using in `alter table`:
  :drop-foreign-key :KEY_name"
  [current-string column-specyfication table-name]
  `(str ~current-string (format " `%s` DROP FOREIGN KEY %s;" ~table-name (name ~column-specyfication ))))

(defmacro drop-column-string
  "Do drop column from table. Using in `alter table`:
  :drop-column :name"
  [current-string column-specyfication table-name]
  `(str ~current-string (format " `%s` DROP COLUMN `%s`;" ~table-name (name ~column-specyfication ))))

(defmacro add-foreign-key-string
  "Add foreign key to table to table. Using in `alter table`:
  [{:id_permission :permission} {:update :cascade :delete :restrict}]"
  [current-string column-specyfication table-name]
  `(str ~current-string (format " `%s` ADD %s;" ~table-name (apply alter-table-constraint-create ~table-name ~column-specyfication))))

(defmacro add-column-string
  "Add column to table. Using in `alter table`:
  :add-column {:suka [:bigint-20 \"NOT NULL\"]}"
  [current-string column-specyfication table-name]
  `(str ~current-string (format " `%s` ADD %s;" ~table-name (create-column ~column-specyfication))))


;;;;;;;;;;;;;;;;;;;;;;;;
;;; pipeline helpers ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-rule-pipeline [keys accepted-lexical-rule]
  (let [key-in? (fn [k col] (when (some #(= k %) col) (symbol (str (symbol k) "-string"))))]
    (reduce #(if-let [k (key-in? %2 keys)] (conj %1 [%2 k]) %1) [] accepted-lexical-rule)))

(defn select-empty-table-pipeline-applier [key-pipeline]
  (if (some #(= :column (first %1)) key-pipeline)
    key-pipeline
    (vec (concat [[:column 'empty-select-string]] key-pipeline))))

(defn get-first-macro-from-pipeline [key-pipeline]
  (if (> (count key-pipeline) 0) [(first key-pipeline)] []))

(defn empty-engine-pipeline-applier [key-pipeline]
  (if (some #(= :table-config (first %1)) key-pipeline) key-pipeline
    (conj key-pipeline [:table-config 'default-table-config-string])))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define-operations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-sql-operation
  ([operation-name accepted-rules pipeline-function]
   `(define-sql-operation ~operation-name ~(string/upper-case (name operation-name)) ~accepted-rules ~pipeline-function))
  ([operation-name operation-string accepted-rules pipeline-function]
   `(defmacro ~operation-name [~'table-name & {:as ~'args}]
     (let [list-of-rules# (~pipeline-function (keys ~'args) ~accepted-rules)]
       `(eval (-> ~~operation-string
                  ~@(for [[~'k ~'F] list-of-rules#]
                      `(~~'F ~~'(k args) (name ~~'table-name)))))))))

(define-sql-operation insert *accepted-insert-rules* create-rule-pipeline)
(define-sql-operation delete *accepted-delete-rules* create-rule-pipeline)
(define-sql-operation update *accepted-update-rules* create-rule-pipeline)
(define-sql-operation select *accepted-select-rules* (comp select-empty-table-pipeline-applier create-rule-pipeline))
(define-sql-operation create-table "CREATE TABLE IF NOT EXISTS" *accepted-ctable-rules* (comp empty-engine-pipeline-applier create-rule-pipeline))
(define-sql-operation alter-table "ALTER TABLE" *accepted-alter-table-rules* (comp get-first-macro-from-pipeline create-rule-pipeline))



;;;;;;;;;;;;;;;;;;;;;;;
;;; Create database ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defn drop-database [database-name]
  {:pre [(string? database-name)]}
  (format "DROP DATABASE `%s`;" (string/trim database-name)))

(defn create-database [database-name & {:keys [charset collate] :or {charset "utf8" collate "utf8_general_ci"}}]
  {:pre [(string? database-name)]}
  (apply format "CREATE DATABASE `%s` CHARACTER SET = '%s' COLLATE = '%s';" (map string/trim [database-name charset collate]) ))

(defn show-databases []
  "SHOW DATABASES")

(defn drop-table [database-table]
  {:pre [(string? database-table)]}
  (format "DROP TABLE `%s` IF EXISTS" (string/trim database-table)) );

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

