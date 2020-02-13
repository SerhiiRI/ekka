(ns ekka.lib.datatool.sql-tool
  (:require
   [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;
;;; parser rules ;;;
;;;;;;;;;;;;;;;;;;;;

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


(defn create-rule-pipeline [keys accepted-lexical-rule]
  (let [key-in? (fn [k col] (when (some #(= k %) col) (symbol (str (symbol k) "-string"))))]
    (reduce #(if-let [k (key-in? %2 keys)] (conj %1 [%2 k]) %1) [] accepted-lexical-rule)))

(defn select-empty-table-pipeline-applier [key-pipeline]
  (if (some #(= :column (first %1)) key-pipeline)
    key-pipeline
    (vec (concat [[:column 'empty-select-string]] key-pipeline))))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; delete preprocessor ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-sql-operation [operation-name accepted-rules pipeline-function]
  `(defmacro ~operation-name [~'table-name & {:as ~'args}]
     (let [list-of-rules# (~pipeline-function (keys ~'args) ~accepted-rules)]
       `(eval (-> ~~(string/upper-case (name operation-name))
                  ~@(for [[~'k ~'F] list-of-rules#]
                      `(~~'F ~~'(k args) ~~'table-name)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define-operations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define-sql-operation insert *accepted-insert-rules* create-rule-pipeline)
(define-sql-operation delete *accepted-delete-rules* create-rule-pipeline)
(define-sql-operation update *accepted-update-rules* create-rule-pipeline)
(define-sql-operation select *accepted-select-rules* (comp select-empty-table-pipeline-applier create-rule-pipeline))


(def ^:dynamic *accepted-ctable-rules* [:columns :foreign-keys :chaset :engine])

(create-table :table
              :columns)


(defn join-map-keyword-string [main-table [k v]]
  
  (let [[table join-column] (list (symbol k) v)]
    (format "%s ON %s.id=%s.%s" table table main-table join-column)))

(defn join-vector-keyword-string [main-table joining-table]
  (let [[table join-column] (list (symbol joining-table)
                                  (symbol (str "id_" (string/lower-case (symbol joining-table)))))]
    (format "%s ON %s.id=%s.%s" table table main-table join-column)))

(defmacro formater [k]
  (let [[sql-type n & _] (string/split (string/lower-case (name k)) #"-")]
    (condp = sql-type
      "bigint" `~(if-not n "BIGINT"
                   (format (condp = n
                             "signed" "BIGINT %s"
                             "unsigned" "BIGINT %s"
                             "zerofill" "BIGINT %s"
                             "BIGINT(%s)") (string/upper-case n)))
      "varchar" `~(format "varchar(%s)" n)
      "nnull" "NOT NULL"
      "null" "NULL"
      "auto_increment" "AUTO_INCREMENT"
      "autoincrement" "AUTO_INCREMENT"
      "auto" "AUTO_INCREMENT"
      "date" "DATE"
      "datetime" "DATETIME"
      "bool" "BOOL"
      "boolean" "BOOL"
      "blob" "BLOB"
      "json" "JSON")))




TINYINT
BOOLEAN
Synonym for TINYINT(1)
SMALLINT
Small integer from -32768 to 32767 signed
MEDIUMINT
Medium integer from -8388608 to 8388607 signed
INT
Integer from -2147483648 to 2147483647 signed
INTEGER
Synonym for INT
BIGINT
Large integer
2	
DECIMAL
A packed "exact" fixed-point number.
DEC, NUMERIC, FIXED
Synonyms for DECIMAL
FLOAT
Single-precision floating-point number
DOUBLE
Normal-size (double-precision) floating-point number
DOUBLE PRECISION
REAL and DOUBLE PRECISION are synonyms for DOUBLE.
BIT


(for [k [:bigint-20 :null :auto_increment]]
  (condp = (first (string/split (string/lower-case (name k)) #"-"))
    "bigint" (formater k)
    "cah" (formater k)))

columns [{:id "bigint(20) NOT NULL AUTO_INCREMENT" }]
;; CREATE TABLE IF NOT EXISTS `user` (
;;   `id` bigint(20) NOT NULL AUTO_INCREMENT,
;;   `login` varchar(100) NOT NULL,
;;   `password` varchar(255) NOT NULL,
;;   `first_name` varchar(100) NOT NULL,
;;   `last_name` varchar(100) NOT NULL,
;;   `id_permission` bigint(20) unsigned NOT NULL,
;;   PRIMARY KEY (`id`),
;;   KEY `user_FK` (`id_permission`),
;;   CONSTRAINT `user_FK` FOREIGN KEY (`id_permission`) REFERENCES `permission` (`id`) ON UPDATE CASCADE
;; ) ENGINE=InnoDB DEFAULT CHARSET=utf8;

;; CREATE TABLE `ekka-test`.NewTable (
;; 	id varchar(100) auto_increment NOT NULL,
;; 	blait varchar(100) NULL,
;; 	fsadfas DATE NULL
;; )
;; ENGINE=InnoDB
;; DEFAULT CHARSET=utf8
;; COLLATE=utf8_general_ci;

;; CREATE TABLE `ekka-test`.NewTable (
;; 	id varchar(100) auto_increment NOT NULL,
;; 	blait varchar(100) NULL,
;; 	fsadfas DATE NULL,
;; 	CONSTRAINT NewTable_PK PRIMARY KEY (id)
;; )
;; ENGINE=InnoDB
;; DEFAULT CHARSET=utf8
;; COLLATE=utf8_general_ci;


;;; DROP FOREING KEY
;;; ALTER TABLE `ekka-test`.`user` DROP FOREIGN KEY user_FK;

;;; ADD KEY FOREIGN
;;; ALTER TABLE `ekka-test`.`user` ADD CONSTRAINT user_FK FOREIGN KEY (id_permission) REFERENCES `ekka-test`.permission(id) ON DELETE CASCADE ON UPDATE CASCADE;


;;; ADd column to table
;; ALTER TABLE `ekka-test`.`user` ADD `temp-column` varchar(100) NULL ;

;;; DROP COLUMN
;; ALTER TABLE `ekka-test`.`user` DROP COLUMN `temp-column` ;


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



