(ns ekka.database.tool
  (:require
   [clojure.string :as string]
   ;; [ekka.database.tool :as tool]
   ))


;;; PARSER RULES 
(def ^:dynamic *accepted-select-rules* [:column :inner-join :right-join :left-join :outer-left-join :outer-right-join :where])
(def ^:dynamic *accepted-select-where-rules* ['in 'less '> '< ])
(def ^:dynamic *accepted-update-rules* [])
(def ^:dynamic *where-border* false)

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


(defmacro defsqljoinrule [rule-name]
  (let [rule-array (string/split (str rule-name) #"\-")  rule-lenght (- (count rule-array) 1)
        rule-keyword (keyword (string/join "-"(take rule-lenght rule-array)))
        rule-string (string/join " " (map string/upper-case (take rule-lenght rule-array)))]
    `(def ~rule-name
       (join-rule-string ~rule-keyword ~rule-string))))



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

(defn join-rule-string [join-type join-string]
  (fn [current-string sql-dictionary table-name]
    (str current-string
         (if-let [joins (get sql-dictionary join-type)]
           (if-let [join-function (get-function-by-join-type joins)]
             (if (and (seqable? joins) (not (string? joins)))
               (str " " (string/join " " (map #(str join-string " " (join-function table-name %1)) joins)))
               (str " " join-string " " (join-function table-name joins))))))))

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
(defsqljoinrule inner-join-string)
(defsqljoinrule left-join-string)
(defsqljoinrule right-join-string)
;;; non-standart rule
(defsqljoinrule outer-right-join-string)
(defsqljoinrule outer-left-join-string)

(defn create-rule-pipeline [keys]
  (let [key-in? (fn [k col]
                  (when (some #(= k %) col)
                    (symbol (str (symbol k) "-string"))))]
    (reduce #(if-let [k (key-in? %2 keys)] (conj %1 k) %1) [] *accepted-select-rules*)))


(defmacro select [table-name & {:as args}]
  (let [table-name-symb (gensym 'table-name)
        dictionary-symb (gensym 'args)
        list-of-rules (create-rule-pipeline (keys args))]
    `(let [~table-name-symb (symbol ~table-name)
           ~dictionary-symb ~args]
       (-> "SELECT"
           ~@(for [F list-of-rules]
             `(~F ~dictionary-symb ~table-name))))))

(select :user_table
        :left-join {:METADATA :id_metadata} 
        :inner-join {:CREDENTIAL :id_credential}
        :column [:name :dla_mamusi :CREDENTAIL.login]
        :where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
                :CREDENTAIL.password "Aleksandr_Bog69"
                :name "Aleksandr"
                :dla_mamusi "Olek"
                :METADATA.merried false})


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

(let [a {:left-join {:METADATA :id_metadata} 
        :inner-join {:CREDENTIAL :id_credential}
        :column [:name :dla_mamusi :CREDENTAIL.login]
        :where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
                :CREDENTAIL.password "Aleksandr_Bog69"
                :name "Aleksandr"
                :dla_mamusi "Olek"
                :METADATA.merried false}}]
  (keys a)
  ;; (create-rule-pipeline (keys a))
  )
;; => (:left-join :inner-join :column :where)
(create-rule-pipeline '(:left-join :inner-join :column :where))
(create-rule-pipeline  [:left-join :inner-join :column :where])



(select :user-table
        :inner-join {:CREDENTIAL :is_user_metadata :METADATA :id_user_metadata}
        :right-join {:A1.id_self :user.id_user_a1 :B1.id_self :USER.id_user_b2}
        :left-join ["suka ON suka.id=user.id_suka" "dupa ON dupa.id=er.id_dupara"]
        :outer-left-join [:suka :bliat]
        :outer-right-join :credential
        :column [:name :dla_mamusi :CREDENTAIL.login]
        :where {:CREDENTAIL.login "XXXpussy_destroyer69@gmail.com"
                :CREDENTAIL.password "Aleksandr_Bog69"
                :name "Aleksandr"
                :dla_mamusi "Olek"
                :METADATA.merried false})




(outer-left-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                                 :suka 2
                                                 :METADATA.merried true}
                                         :column [:bliat :suka]
                                         :outer-left-join {:CREDENTIAL :is_user_metadata
                                                      :METADATA :id_user_metadata}} "user")




(inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                                 :suka 2
                                                 :METADATA.merried true}
                                         :column [:bliat :suka]
                                         :inner-join {:CREDENTIAL.id_self :user.id_user_credential
                                                      :METADATA.id_self :USER.id_user_metadata}} "user")

(inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                                 :suka 2
                                                 :METADATA.merried true}
                                         :column [:bliat :suka]
                                         :inner-join [:CREDENTIAL :METADATA]} "user")

(inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                                 :suka 2
                                                 :METADATA.merried true}
                                         :column [:bliat :suka]
                                         :inner-join :suka} "user")

(inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                                 :suka 2
                                                 :METADATA.merried true}
                                         :column [:bliat :suka]
                                         :inner-join "suka"} "user")

(inner-join-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                                 :suka 2
                                                 :METADATA.merried true}
                                         :column [:bliat :suka]
                                         :inner-join ["suka ON suka.id=user.id_suka"
                                                      "dupa ON dupa.id=user.id_dupara"]} "user")

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
                                         :inner-join ["INNER JOIN XXX ON user.id_columnt=XXX.id"]} "user")

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

(where-string "SELECT * FROM user" {:column [ :bliat :suka]
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

:where (and (= credential.login 1)
            (not (in 1)))

(defmacro and-ww
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))

(defmacro or-ww
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if or# or# (or ~@next)))))

(defn into-border [some-string]
  (if *where-border* 
    (format "(%s)" some-string)
    some-string))

(let [where-border true]
  (into-border "blait"))

(into-border)

(defmacro  in [])
(defmacro between [])
(defmacro like [])

(defmacro and-processor [& args]
  `(into-border (string/join " AND " (binding [*where-border* true]
                                       [~@args]))))

(defmacro or-processor [& args]
  `(into-border (string/join " OR " (binding [*where-border* true]
                                       [~@args]))))

(defmacro between-processor)

(or-processor "cuska" 123
              (and-processor "dupa"
                             (or-processor (= 1 2) 2 4) 2))

'(and (in 1 [2 3 4 5])
      (or (between :col (1 2))
          (like :col "blit")))


;; < <= <> >= >
;; WHERE City IN ('Paris','London');
;; WHERE Price BETWEEN 50 AND 60;

;; (let [t "USER"
;;       j :CREDENTAIL
;;       f (get-function-by-join-type j)]
;;   (f t j))

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
