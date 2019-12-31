(ns ekka.core)

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



(defmacro create-field-and
  ([a] false)
  ([key-typed]
   (partial create-field-and key-typed))
  ([key-typed1 key-typed2 & {:keys [exclude] :or {exclude []}}]
   {:pre [(if (empty? exclude) true (keyword? (first exclude)))]}
   (let [a (for [i (keys (eval key-typed1))
                 :when (nil? (first (filter #(= i %) exclude)))]
             `(= (~i ~key-typed1) (~i ~key-typed2)))]
     `(and ~@a)))
  ([a b & c {:keys [exclude] :or {exclude []}}]
   {:pre [(if (empty? exclude) true (keyword? (first exclude)))]}
   (let [a (for [i (keys (eval key-typed1))
                 :when (nil? (first (filter #(= i %) exclude)))]
             `(= (~i ~key-typed1) (~i ~key-typed2)))]
     `(and ~@a))))

(do (defn argument-tester [& args]
      (let [number (.indexOf args :e)
            kparm (get (vec args) (+ number 1))]
        (if (seqable? kparm)
          kparm
          [kparm])))
    (chuj 1 2 3 4 :e ['kparm] ))


(or nil 23 )


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
