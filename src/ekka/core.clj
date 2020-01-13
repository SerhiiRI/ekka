(ns ekka.core
  (:require
   ;; [ekka.database.tool :as tool]
   [clojure.string :as string]))

(def user-database [])

(var user-database)

(defn printuj []
  (println "chuj tam"))


(defrecord User [credential permission email name last_name])
(defrecord Credential [login password])
(defrecord Permission [id permission])

(do
  (def user-database [])
  (def user-database (conj user-database
                           (User. (Credential. "admin" "admin")
                                  (Permission. 1 "admin")
                                  "admin@admin.pl"
                                  "Admin"
                                  "Administrowicz")
                           (User. (Credential. "Tomasz" "Pilot")
                                  (Permission. 2 "user")
                                  "user@user.pl"
                                  "Tomasz"
                                  "Pilot"))))


(defn login [])
(defn register [])
(defn test-user [credential]
     (letfn [(comprt [f c1 c2] (= (f c1) (f c2)))]
       (first (filter #((and
                         (comprt credential %1)
                         (comprt credential %2)))))))


(defmacro field-comparator
  ([exld]
   `(fn [key-typed1]
      (field-comparator exld key-typed1)))
  ([exld key-typed1]
   `(fn [key-typed2]
      (field-comparator exld key-typed1 key-typed2)))
  ([exld key-typed1 key-typed2]
   (let [a (for [i (keys (eval key-typed1))
                 :when (nil? (first (filter #(= i %) exclude)))
                 :let [first-compr (i (eval key-typed1))
                       second-compr (i (eval key-typed2))]]
             `(= ~first-compr ~second-compr))]
     `(and ~@a))))

(defun field-compare [& args]
  {:pre [(if (or (= (first exld) :no-test)
                 (= (first exld) :test))
           true (keyword? (first exclude)))
         (> (count args) 0)]}
  
  (if-let [exclude-keywords (argument-tester all-args)] 
    (let [first-compr (field-comparator exclude-keywords a b)]
      (and first-compr (field-compare exclude-keywords ~a ~@all-args)))
    (let [first-compr (field-comparator [] a b)]
      (and first-compr (field-compare [] a b)))))



(field-compare :no-test)

(create-field-and :no-test [:one :two]
                  (User. (Credential. "Admin" "admin")
                         (Permission. 1 "admin")
                         "admin@admin.pl"
                         "Admin"
                         "Administrowicz")
                  (User. (Credential. "Admin" "admin")
                         (Permission. 1 "admin")
                         "admin@admin.pl"
                         "Admin"
                         "Administrowicz")
                  (User. (Credential. "Tomasz" "Pilot")
                         (Permission. 2 "user")
                         "user@user.pl"
                         "Tomasz"
                         "Pilot"))


(defn argument-tester
  "Return the `:test` and `no-test` keywords arguments if it's in
  ```
  (argument-tester obj1 obj2 obj3 :exclude [:some :123]
  ;;=> [:some :123]
  (argument-tester obj1 obj2 obj3 :exclude :some
  ;;=> [:some]
  (argument-tester obj1 obj2 obj3 :exclude
  ;;=> nil
  (argument-tester obj1 obj2 obj3
  ;;=> nil
  ```
  "
  [& args]
  (let [f (if (= (first args) :test) a b )]
    (let [number (.indexOf args :test)
          kparm (get (vec args) (+ number 1))]
      (if (seqable? kparm)
        kparm
        [kparm]))))

(do (defn argument-tester [& args]
      (let [number (.indexOf args :e)
            kparm (get (vec args) (+ number 1))]
        (if (seqable? kparm)
          kparm
          [kparm])))
    (argument-tester 1 2 3 4 :e))





(create-field-and (Credential. "admin" "admin") 
                  (Credential. "suka" "admin")
                  :exclude [:login])



(let [a (create-field-and (Credential. "admin" "admin") 
                          (Credential. "user" "user") 
                          (Credential. "admin" "admin"))]
  (println a))


(defmacro unless [pred a b]
  `(if (not ~pred) ~a ~b))

;; usage:




(let [[key-typed1 key-typed2] [(Credential. "admin" "admin") 
                               (Credential. "admin" "admin")]]
  (let [a (for [i (keys key-typed1)]
            `(= (~i ~key-typed1) (~i ~key-typed2)))]
    `(and ~@a)))


;;; sql-tool
(defmacro select [value & {:as args}]
  ;; (let [from-table (str "FROM " (symbol value))
  ;;       column (if ())]
  ;;   (println args))
  (println args))

(select :user
        :where {:bliat 1 :suka 2}
        :column [:bliat :suka])

(select user
        :join-on {credential :id_credential
               data :id_data}
        :where {:bliat 1 :suka 2})



(println (mapcat identity {:suka :fuck}))

(str "FROM " (symbol :sukas))

(get-in {:where {:bliat 1, :suka 2}, :column [:bliat :suka]} [:where :bliat])
(get {:where {:bliat "slia", :suka 2}, :column [:bliat :suka]} :where )



;;; pair where pattern
(defn pair-where-pattern
  ([k v] (format (cond
                   (string? v) "%s=\"%s\""
                   (or (boolean? v) (number? v)) "%s=%s"
                   :else "%s=%s")
                 (symbol k)
                 v))
  ([k v t] (str t "." (format (cond
                               (string? v) "%s=\"%s\""
                               (or (boolean? v) (number? v)) "%s=%s"
                               :else "%s=%s")
                             (symbol k)
                             v))))

(pair-where-pattern :suka "bliat" "user")

(defn tkey [k]
  ;; :table.value => table.value 
  (string/split (str (symbol k)) #"\."))

;;; not to use 
(defmacro defsqlrule [rule-name arguments & body]
  {:pre [(vector? arguments)]}
  (let [rule-array (string/split (str rule-name) #"\-")  rule-lenght (- (count rule-array) 1)
        rule-keyword (keyword (string/join "-"(take rule-lenght rule-array)))]
   `(defn ~rule-name ~arguments
      (str (first ~arguments) " "
           (if-let [dictionary (get (second ~arguments) ~rule-keyword)]
             ~body
             "")))))

(defsqlrule inner-join-string [1 2 3])

(defsqlrule inner-join-string [current-string sql-dictionary table-name]
  (let [join-formater #(format "INNER JOIN %s ON %s.id=%s.%s" %1 %1 table-name %2)
        map-function (if (map? joins)
                       #(map symbol %)
                       #(list (symbol %) (symbol (str "id_" (string/lower-case (symbol %))))))]
    (string/join " " (map #(apply join-formater (map-function %)) joins))))


;;; inner join
(defn inner-join-string [current-string sql-dictionary table-name]
  (str current-string " "
       (if-let [joins (get sql-dictionary :inner-join)]
         (let [join-formater #(format "INNER JOIN %s ON %s.id=%s.%s" %1 %1 table-name %2)
               map-function (if (map? joins)
                          #(map symbol %)
                          #(list (symbol %) (symbol (str "id_" (string/lower-case (symbol %))))))]
           (string/join " " (map #(apply join-formater (map-function %)) joins))) "")))


;;; left join string constructor
(defn left-join-string [current-string sql-dictionary table-name]
  (str current-string " "
       (if-let [joins (get sql-dictionary :left-join)]
         (let [join-formater #(format "LEFT JOIN %s ON %s.id=%s.%s" %1 %1 table-name %2)
               map-function (if (map? joins)
                          #(map symbol %)
                          #(list (symbol %) (symbol (str "id_" (string/lower-case (symbol %))))))]
           (string/join " " (map #(apply join-formater (map-function %)) joins))) "")))

;;; right join
(defn right-join-string [current-string sql-dictionary table-name]
  (str current-string " "
       (if-let [joins (get sql-dictionary :right-join)]
         (let [join-formater #(format "RIGHT JOIN %s ON %s.id=%s.%s" %1 %1 table-name %2)
               map-function (if (map? joins)
                          #(map symbol %)
                          #(list (symbol %) (symbol (str "id_" (string/lower-case (symbol %))))))]
           (string/join " " (map #(apply join-formater (map-function %)) joins))) "")))


;;; column string constructor 
(defn column-string [current-string sql-dictionary table-name]
  (str current-string " "
       (if-let [columns (get sql-dictionary :column)]
         (string/join " " (map (comp str symbol) columns))
         "*") " FROM " table-name))


;;; where string constructor 
(defn where-string [current-string sql-dictionary table-name]
  (str current-string
       (if-let [key-where (get sql-dictionary :where)]
         (if (empty? (get sql-dictionary :join-on))
           (str " WHERE " (string/join " " (map #(apply pair-where-pattern %) (seq key-where))))
           (str " WHERE " (string/join " " (map #(let [[k v] %]
                                                   (if (string/includes? k ".")
                                                     (pair-where-pattern k v)
                                                     (pair-where-pattern k v table-name))) (seq key-where)))))
         "")))





(join-on-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                              :suka 2
                                              :METADATA.merried true}
                                      :column [:bliat :suka]
                                      :join-on {:CREDENTIAL :id_credential
                                                :METADATA :id_metadata}} "user")

(join-on-string "SELECT * FROM user" {:where {:CREDENTAIL.login "anatoli"
                                              :suka 2
                                              :METADATA.merried true}
                                      :column [:bliat :suka]
                                      :join-on [:CREDENTIAL :METADATA]} "user")



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



