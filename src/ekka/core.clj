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
  ([] false)
  ([key-typed]
   (fn [object-to-compare]
     `(create-field-and key-typed object-to-compare)))
  ([key-typed1 key-typed2]
   (let [a (for [i (keys key-typed1)]
            `(= (~i ~key-typed1) (~i ~key-typed2)))]
     `(and ~@a))))

(defmacro create-field-and
  ([] false)
  ([key-typed]
   (fn [object-to-compare]
     `(create-field-and key-typed object-to-compare)))
  ([key-typed1 key-typed2]
   (let [a (for [i (keys key-typed1)]
            `(= (~i ~key-typed1) (~i ~key-typed2)))]
     `(and ~@a))))


(let [[key-typed1 key-typed2] [(Credential. "admin" "admin")
                               (Credential. "1sa" "1231")
                               ]]
  (let [key-list [key-typed1 key-typed2]
        a (for [i (keys key-typed1)]
            (let [cmpr (map (fn [kt] (i kt)) key-list)]
              `(= ~@cmpr)))]
    `(and ~@a)))



(create-field-and (User. (Credential. "admin" "admin")
                         (Permission. 1 "admin")
                         "admin@admin.pl"
                         "Admin"
                         "Administrowicz")
                  (User. (Credential. "admin" "admin")
                         (Permission. 1 "admin")
                         "admin@admin.pl"
                         "Admin"
                         "Administrowicz"))


