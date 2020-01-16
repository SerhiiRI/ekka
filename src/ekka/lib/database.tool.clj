(ns ekka.database.tool
  (:require
   [clojure.string :as string]
   ;; [ekka.database.tool :as tool]
   ))



(defn pair-where-pattern
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



;;; inner join
;; (defn inner-join-string [current-string sql-dictionary table-name]
;;   (str current-string
;;        (if-let [joins (get sql-dictionary :inner-join)]
;;          (let [join-formater #(format " INNER JOIN %s ON %s.id=%s.%s" %1 %1 table-name %2)
;;                map-function (if (map? joins)
;;                           #(map symbol %)
;;                           #(list (symbol %) (symbol (str "id_" (string/lower-case (symbol %))))))]
;;            (string/join "" (map #(apply join-formater (map-function %)) joins))) "")))

;;; left join string constructor
;; (defn left-join-string [current-string sql-dictionary table-name]
;;   (str current-string
;;        (if-let [joins (get sql-dictionary :left-join)]
;;          (let [join-formater #(format " LEFT JOIN %s ON %s.id=%s.%s" %1 %1 table-name %2)
;;                map-function (if (map? joins)
;;                           #(map symbol %)
;;                           #(list (symbol %) (symbol (str "id_" (string/lower-case (symbol %))))))]
;;            (string/join "" (map #(apply join-formater (map-function %)) joins))) "")))

;;; right join
;; (defn right-join-string [current-string sql-dictionary table-name]
;;   (str current-string
;;        (if-let [joins (get sql-dictionary :right-join)]
;;          (let [join-formater #(format " RIGHT JOIN %s ON %s.id=%s.%s" %1 %1 table-name %2)
;;                map-function (if (map? joins)
;;                           #(map symbol %)
;;                           #(list (symbol %) (symbol (str "id_" (string/lower-case (symbol %))))))]
;;            (string/join "" (map #(apply join-formater (map-function %)) joins))) "")))


(defmacro defsqljoinrule [rule-name]
  (let [rule-array (string/split (str rule-name) #"\-")  rule-lenght (- (count rule-array) 1)
        rule-keyword (keyword (string/join "-"(take rule-lenght rule-array)))
        rule-string (string/join " " (map string/upper-case (take rule-lenght rule-array)))]
    `(def ~rule-name
       (join-rule-string ~rule-keyword ~rule-string))))


;; (defn join-rule-string [join-type join-string]
;;   (fn [current-string sql-dictionary table-name]
;;     (str current-string
;;          (if-let [joins (get sql-dictionary join-type)]
;;            (let [join-formater #(format " %s %s ON %s.id=%s.%s" join-string %1 %1 table-name %2)
;;                  map-function (if (map? joins)
;;                                 #(map symbol %)
;;                                 #(list (symbol %)
;;                                        (symbol (str "id_" (string/lower-case (symbol %))))))]
;;              (string/join "" (map #(apply join-formater (map-function %)) joins))) ""))))


(defn join-rule-string [join-type join-string]
  (fn [current-string sql-dictionary table-name]
    (str current-string
         (if-let [joins (get sql-dictionary join-type)]
           (if-let [join-function (get-function-by-join-type table-name joins)]
             (if (seqable? joins)
               (str join-string (join-function table-name joins))
               (reduce str (map #(str join-string (join-function table-name %1)) joins))))))))


(let [t "USER"
      j :CREDENTAIL
      f (get-function-by-join-type j)]
  (f t j))

(let [t "USER"
      j "SUKA"
      f (get-function-by-join-type j)]
  (f t j))

(let [t "USER"
      j {:CREDENTIAL :id_credential
         :META :id_metadata}
      f (get-function-by-join-type j)]
  (map #(f t %1) j))

(let [t "USER"
      j [:CREDENTIAL :OtherTable]
      f (get-function-by-join-type j)]
  (map #(f t %1) j))

(let [t "USER"
      j {:CREDENTIAL :OtherTable.id_curwa}
      f (get-function-by-join-type j)]
  (map #(f t %1) j))


(defn get-function-by-join-type [join]
 (cond
   (keyword? join) join-keyword-string
   (string? join) join-string-string
   ;;"text ON text.id...."
   (map? join) (if-let [value-of-key (second (first join))]
                 (when (keyword? value-of-key)
                   (if (and (some #(= \. %1) (str value-of-key))
                            (some #(= \. %1) (str (first (first join)))))
                     join-dot-map-string
                     join-map-keyword-string)))
   (vector? join) (if-let [first-value (first join)]
                    (cond (keyword? first-value) join-vector-keyword-string
                          ;; "[:SUKA :BLIAT]"
                          (string? first-value) join-vector-string-string))))


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
  (if-let [[[t1 id1]
            [t2 id2]] (and (some #(= \. %) (str k))
                           (some #(= \. %) (str v))
                           (list (string/split (str (symbol k)) #"\.")
                                 (string/split (str (symbol v)) #"\.")))]
    (format "%s ON %s=%s" t1 (str (symbol k)) (str(symbol v)))))



(defsqljoinrule inner-join-string)
(defsqljoinrule left-join-string)
(defsqljoinrule right-join-string)
;;; non-standart rule
;; (defsqljoinrule outer-right-join-string)
;; (defsqljoinrule outer-left-join-string)

;;; column string constructor 
(defn column-string [current-string sql-dictionary table-name]
  (str current-string
       (if-let [columns (get sql-dictionary :column)]
         (str " " (string/join ", " (map (comp str symbol) columns)))
         " *") " FROM " table-name))

;;; where string constructor 
(defn where-string [current-string sql-dictionary table-name]
  (str current-string
       (if-let [key-where (get sql-dictionary :where)]
         (if (empty? (get sql-dictionary :join-on))
           (str " WHERE " (string/join " " (map #(apply pair-where-pattern %) (seq key-where))))
           (str " WHERE " (string/join " " (map #(let [[k v] %]
                                                   (if (string/includes? k ".")
                                                     (pair-where-pattern k v)
                                                     (pair-where-pattern k v table-name)))
                                                (seq key-where))))))))



(inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                                 :suka 2
                                                 :METADATA.merried true}
                                         :column [:bliat :suka]
                                         :inner-join {:CREDENTIAL :id_credential
                                                      :METADATA :id_metadata}} "user")

(inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                                 :suka 2
                                                 :METADATA.merried true}
                                         :column [:bliat :suka]
                                         :inner-join ["INNER JOIN XXX ON user.id_columnt=XXX.id"]
                                         :METADATA } "user")



(left-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                                :suka 2
                                                :METADATA.merried true}
                                        :column [:bliat :suka]
                                        :left-join [:CREDENTIAL :METADATA]} "user")



(column-string "SELECT" {:where {:bliat "slia" :suka 2 :what? true}
                         :column [:bliat :suka]
                         :join-on {:CREDENTIAL :id_credential}} "user")


(column-string "SELECT" {} "user")



(where-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                            :suka 2
                                            :METADATA.merried true}
                                    :column [:bliat :suka]
                                    :join-on {:CREDENTIAL :id_credential
                                              :METADATA :id_metadata}} "user")

(where-string "SELECT * FROM user" {:column [:bliat :suka]
                                    :join-on {:CREDENTIAL :id_credential
                                              :METADATA :id_metadata}} "user")


(let [table-name "users"
      dick {:where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
                    :CREDENTAIL.password "Aleksandr_Bog69"
                    :name "Aleksandr"
                    :dla_mamusi "Olek"
                    :METADATA.merried false}
            :column [:name :dla_mamusi :CREDENTAIL.login]
            :inner-join {:CREDENTIAL :id_credential}
            :left-join {:METADATA :id_metadata}}]
  (-> "SELECT"
      (column-string dick table-name)
      (inner-join-string dick table-name)
      (left-join-string dick table-name)
      (right-join-string dick table-name)
      (where-string dick table-name)))



{:where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
                       :CREDENTAIL.password "Aleksandr_Bog69"
                       :name "Aleksandr"
                       :dla_mamusi "Olek"
                       :METADATA.merried false}
               :column [:name :dla_mamusi :CREDENTAIL.login]
               :inner-join {:CREDENTIAL :id_credential}
               :left-join {:METADATA :id_metadata}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro create-rule-pipeline [dicktionary]
  (if))

(reduce key-to-modificator [] (keys {:where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
                                             :CREDENTAIL.password "Aleksandr_Bog69"
                                             :name "Aleksandr"
                                             :dla_mamusi "Olek"
                                             :METADATA.merried false}
                                     :column [:name :dla_mamusi :CREDENTAIL.login]
                                     :inner-join {:CREDENTIAL :id_credential}
                                     :left-join {:METADATA :id_metadata}}))

(defn key-to-modificator [acc key]
  (if-let [f (condp = key
               :column     column-string
               :where      where-string
               :inner-join inner-join-string
               :left-join  left-join-string
               :right-join right-join-string)]
    (conj acc f)))

(keys {:where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
                                             :CREDENTAIL.password "Aleksandr_Bog69"
                                             :name "Aleksandr"
                                             :dla_mamusi "Olek"
                                             :METADATA.merried false}
                                     :column [:name :dla_mamusi :CREDENTAIL.login]
                                     :inner-join {:CREDENTIAL :id_credential}
                                     :left-join {:METADATA :id_metadata}})

(conj [1 4 3] 1)
(key-to-modificator :where)

(let [kluczy [:where :column :inner-join :left-join]]
  (if (some #(= :column %) kluczy )
    (conj [] column-string)
    []))

(get [:where :column :inner-join :left-join] :column)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro select [table-name & {:as args}]
  (let [table-name-symb (gensym 'table-name)
        dictionary-symb (gensym 'args)]
    `(let [~table-name-symb (symbol ~table-name)
           ~dictionary-symb ~args]
         (-> "SELECT"
         (column-string ~dictionary-symb ~table-name-symb)
         (inner-join-string ~dictionary-symb ~table-name-symb)
         (left-join-string ~dictionary-symb ~table-name-symb)
         (right-join-string ~dictionary-symb ~table-name-symb)
         (where-string ~dictionary-symb ~table-name-symb)))))



(select :user_table)


(select :user_table
        
        :left-join {:METADATA :id_metadata} 
        :inner-join {:CREDENTIAL :id_credential}
        :column [:name :dla_mamusi :CREDENTAIL.login]
        :where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
                :CREDENTAIL.password "Aleksandr_Bog69"
                :name "Aleksandr"
                :dla_mamusi "Olek"
                :METADATA.merried false})

(select USER_TABLE
        :left-join {METADATA :id_metadata} 
        :inner-join {CREDENTIAL :id_credential}
        :column [:name :dla_mamusi :CREDENTAIL.login]
        :where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
                :CREDENTAIL.password "Aleksandr_Bog69"
                :name "Aleksandr"
                :dla_mamusi "Olek"
                :METADATA.merried false})


