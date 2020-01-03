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
