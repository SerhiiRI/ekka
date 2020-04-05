(ns hrtime.database-manager
  (:gen-class)
  (:use
   seesaw.chooser)
  (:require
   [hrtime.sql-tool :as toolbox :include-macros true :refer :all]
   [hrtime.dev-tools :refer [image-scale]]
   [hrtime.config-manager :refer :all]
   [hrtime.icon-library :as icon]
   [excel-clj.core :as excel]
   [clojure.string :as string]
   [clojure.java.jdbc :as jdbc]
   [clojure.spec.alpha :as spec]))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DB configurations ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; (def ^:dynamic sqlite {:classname "org.sqlite.JDBC" :subprotocol "sqlite" :subname "hrtime-test"})

(def config (config-file "database.edn"))


;; (def ^:dynamic sqlite {:classname    (config [:JDBC-sqlite-configuration :classname])
;;                        :subprotocol  (config [:JDBC-sqlite-configuration :subprotocol])
;;                        :subname      (config [:JDBC-sqlite-configuration :subname])})

(def ^:dynamic *table-default-header* (config [:View-table-name]))

;; (def ^:dynamic sql-connection {:dbtype "mysql" :host "192.168.1.31" :port 3306 :dbname "db" :user "hrtime" :password "dupa"})
;; (def ^:dynamic sql-connection {:dbtype "mysql" :host "127.0.0.1" :port 3306 :dbname "hrtime-first" :user "root" :password "123"})
;; (def ^:dynamic sql-connection {:dbtype "mysql" :host "192.168.1.31" :port 3306 :dbname "db" :user "hrtime" :password "dupa"})
(def ^:dynamic sql-connection (config [:JDBC-mariadb-configuration-database]))
;; (def ^:dynamic db-test {:dbtype "mysql" :dbname "hrtime-test" :user "root" :password "1234"})



;; (spec/def :db/host (every-pred string? not-empty))
;; (spec/def :db/dbtype (every-pred string? not-empty))
;; (spec/def :db/port #(<= 0 % 65535))
;; (spec/def ::db-connector-scheme (spec/keys :req-un [:db/host :db/dbtype :db/port]))
(defn test-connection [db-spec]
  ;; {:pre [(spec/valid? ::db-connector-scheme db-spec)]}
  (let [subprotocols {"hsql"       "hsqldb"
                      "jtds"       "jtds:sqlserver"
                      "mssql"      "sqlserver"
                      "oracle"     "oracle:thin"
                      "oracle:sid" "oracle:thin"
                      "postgres"   "postgresql"}
        host-prefixes {"oracle:oci"  "@"
                       "oracle:thin" "@"}
        dbname-separators {"mssql"      ";DATABASENAME="
                           "sqlserver"  ";DATABASENAME="
                           "oracle:sid" ":"}
        dbtype (:dbtype db-spec)
        port (:port db-spec)
        host (:host db-spec)
        subprotocol (subprotocols dbtype dbtype)
        port (when port (str ":" port))
        db-sep (dbname-separators dbtype "/")]
    (java.sql.DriverManager/setLoginTimeout 1) 
    (try
      (let [connector (java.sql.DriverManager/getConnection (do (println (str "jdbc:" subprotocol "://" host port))
                                                                (str "jdbc:" subprotocol "://" host port db-sep "?socketTimeout=4000&loginTimeout=4000&connectTimeout=4000"
                                                                     )) (:user db-spec) (:password db-spec))]
        (jdbc/query connector (jdbc/prepare-statement connector "SHOW DATABASES"
                                                      {
                                                       :timeout 4})))
      (catch com.mysql.jdbc.exceptions.jdbc4.CommunicationsException _ nil) 
      (catch java.net.ConnectException _ nil)
      (catch Exception _ nil))))

;; (test-connection {:dbtype "mysql" :host "192.168.1.31" :port "3306" :dbname "db" :user "hrtime" :password "dupa"})
;; (jdbc/query sql-connection "SHOW TABLES")
(def UI-combo-column {:work_type ["" "Umysłowy" "Fizyczny"] :section ["" "HS" "HMS" "TME" "EPE" "TKT"]})

;;;;;;;;;;;;;;
;;; config ;;;
;;;;;;;;;;;;;;

(def user
  (create-table :user
                :columns [{:first_name [:varchar-100 :default :null]}
                          {:last_name [:varchar-100 :default :null]}
                          {:work_type [:varchar-100 :default :null]}
                          {:section [:varchar-100 :default :null]}
                          {:teta_nr [:varchar-100 :default :null]}]))

(def card
  (create-table :card
                :columns [{:rfid [:varchar-100 :default :null]}
                          {:id_user [:bigint-120-unsigned :null]}]
                :foreign-keys [{:id_user :user} {:delete :null :update :null}]))


(def registration
  (create-table :registration
                :columns [{:datetime [:datetime :default :null]}
                          {:direction [:varchar-100 :default :null]}
                          {:id_user [:bigint-120-unsigned :null]}
                          {:id_card [:bigint-120-unsigned :null]}]
                :foreign-keys [{:id_user :user} {:delete :null :update :null}]
                :foreign-keys [{:id_card :card} {:delete :null :update :null}]))





(defn drop-all-tables []
  (jdbc/execute! sql-connection (drop-table :registration))
  (jdbc/execute! sql-connection (drop-table :card))
  (jdbc/execute! sql-connection (drop-table :user)))

(defn create-all-tables []
  (jdbc/execute! sql-connection user)
  (jdbc/execute! sql-connection card)
  (jdbc/execute! sql-connection registration))
;; (create-all-tables)

;;; SQL-lite style
;; (defn create-table-user []
;;   (jdbc/execute! sql-connection "CREATE TABLE IF NOT EXISTS `user` (
;;    id INTEGER PRIMARY KEY AUTOINCREMENT, 
;;    first_name CHAR(100) NULL, 
;;    last_name CHAR(100) NULL, 
;;    work_type CHAR(100) NULL, 
;;    section CHAR(100) NULL, 
;;    teta_nr CHAR(100) NULL)"))
;; (defn create-table-card []
;;   (jdbc/execute! sql-connection "CREATE TABLE IF NOT EXISTS `card` (
;;    id INTEGER PRIMARY KEY AUTOINCREMENT, 
;;    rfid CHAR(100) NULL, 
;;    id_user INTEGER NULL, 
;;    FOREIGN KEY (id_user) REFERENCES user (id))"))
;; (defn create-table-registration []
;;   (jdbc/execute! sql-connection "CREATE TABLE IF NOT EXISTS `registration` (
;;    id INTEGER PRIMARY KEY AUTOINCREMENT,
;;    datetime DATETIME NULL,
;;    direction INTEGER NULL, 
;;    id_user INTEGER NULL,
;;    id_card INTEGER NULL,
;;    FOREIGN KEY (id_user) REFERENCES user (id),
;;    FOREIGN KEY (id_card) REFERENCES card (id))"))


;;;;;;;;;;;;;;;;;;;;;;
;;; Data generator ;;;
;;;;;;;;;;;;;;;;;;;;;;
(defn generate-dummy-data-card []
  (let[already-used (reduce (fn [a x] (if-let [u_id (:id_user x)] (conj a u_id) a)) [] (jdbc/query sql-connection (select :card)))]
    (map #(jdbc/execute! sql-connection (toolbox/insert :card :values [nil (format "rfid%s" (rand-int 9898859)) (:id %) ]))
         (filter (fn [u] (not (some #(= % (:id u)) already-used))) (jdbc/query sql-connection (select :user))))))

(defn generate-dummy-data-user []
  (map #(jdbc/execute! sql-connection (toolbox/insert :user :values [nil (format "f%s" %) (format "l%s" %) (format "w%s" %) (format "s%s" %) (format "s%s" %)]))
       (range 1 30)))

(defn generate-dummy-data-registration []
  (map #(if (nil? (:id_user %)) nil
            (jdbc/execute! sql-connection (toolbox/insert :registration
                                                          :values [nil (date) (if (= 0 (rand-int 2)) "we" "wy") (:id %) (:id_user %)])))
       (jdbc/query sql-connection (select :card))))

(defn refresh-test-bd []
  (do
    (drop-all-tables)
    (create-all-tables)
    (do (generate-dummy-data-user))
    (do (generate-dummy-data-card))
    (do (generate-dummy-data-registration))))


;;;;;;;;;;;;;;;;;;;;;;
;;; Data managment ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defrecord Card [id rfid id_user])
(defrecord User [id first_name last_name work_type section teta_nr])
(defrecord Registration [id datetime direction id_user id_card])

(defn ^clojure.lang.PersistentList get-sql-by-id
  "Example: 
   (get-sql-by-id :card 4 map->Card)
   (get-sql-by-id :user 4 map->User)
   (get-sql-by-id :registration 4 map->Registration)"
  ([table id]
   (jdbc/query sql-connection (select table :where (= :id id))))
  ([table id mapper]
   (map mapper (get-sql-by-id table id))))

(defn ^clojure.lang.PersistentList update-sql-by-id
  "Description
  Do insert or update do DB, depended on `m` data `:id`
  keyword data. If id is nil, than do insert. 
  
  Example:
  (update-sql-by-id :card
     (map->Card {:id 4, :rfid \"sukabliat\", :id_user 4}))
  (update-sql-by-id :user
     (map->User {:id 5, :first_name \"Adam\",
                 :last_name \"Nowak\", :work_type \"w5\",
                 :section \"s5\", :teta_nr \"s5\"}))
  (update-sql-by-id :registration
      (map->Registration {:id 5, :datetime (.format (java.text.SimpleDateFormat. \"YYYY-MM-dd HH:MM:ss\") (java.util.Date. ))
                          :direction \"wyjscie\",
                          :id_user 6, :id_card 6}))"  
  ([table m]
   (if (:id m)
     (jdbc/execute! sql-connection (update table :set m :where (= :id (:id m))))
     (jdbc/execute! sql-connection (insert table :values (vals m))))))

(defn ^clojure.lang.PersistentList update-sql-by-id-template
  "Description
  Do insert or update do DB, depended on `m` data `:id`
  keyword data. If id is nil, than do insert. 
  
  Example:
  (update-sql-by-id :card
     (map->Card {:id 4, :rfid \"sukabliat\", :id_user 4}))
  (update-sql-by-id :user
     (map->User {:id 5, :first_name \"Adam\",
                 :last_name \"Nowak\", :work_type \"w5\",
                 :section \"s5\", :teta_nr \"s5\"}))
  (update-sql-by-id :registration
      (map->Registration {:id 5, :datetime (.format (java.text.SimpleDateFormat. \"YYYY-MM-dd HH:MM:ss\") (java.util.Date. ))
                          :direction \"wyjscie\",
                          :id_user 6, :id_card 6}))"  
  ([table m]
   (if (:id m)
     (update table :set m :where (= :id (:id m)))
     (insert table :values (vals m)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; `Seesaw.core/table` modele generator ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn addTableSorter
  "Something to creating table"
  [^javax.swing.JTable T point-lambda]
  (doto (.getTableHeader T)
    (.addMouseListener
     (proxy [java.awt.event.MouseAdapter] []
       (^void mouseClicked [^java.awt.event.MouseEvent e]
        (point-lambda (.getPoint e)))))) T)


(defmulti filter-col-by
  "Describe:
  Return function which do filter on Map Data
  by column(-s), using re-gex-pattern for matching
  
  If argument is RegularExpr patter, than match
  by values of all columns
  If argument is {keyword RegularExpr}, then match
  only by column, choosed by 'keyword' map key.

  Return:
    (fn [col-map]) => col-map 
  
  Example:
    (filter-col-by #\"DUPA\")
    (filter-col-by {:first_name #\"DUPA\"})"
  class)
(defmethod filter-col-by nil [pattern] (fn [col] nil))
(defmethod filter-col-by :default [pattern] (fn [col] col))
(defmethod filter-col-by java.util.regex.Pattern [pattern]
  (fn [col] (filter (fn [item] (not (empty? (re-seq pattern (string/join "" (vals item)))))) col)))
(defmethod filter-col-by clojure.lang.PersistentArrayMap [pattern]
  (let [[k v] (first pattern)]
   (fn [col] (filter (fn [item] (not (empty? (re-seq v (k item))))) col))))


(defn sort-by-column
  "Describe:re
  Return function which to sorting on Map Data
  by column. Defacto wraper, which apply list to
  sort+reverse functionality.
  First argument is column for sorting, and second
  optinal argument for selection direction

  Return:
    (fn [col-map]) => col-map 
  
  Example:
    (do-sort-view :first_name)
    (do-sort-view :first_name :dec)
    (do-sort-view :first_name :inc)"
  [k & [dec-inc]] {:pre [(keyword? k)]}
  (fn [col-map] (let [sorted (sort-by k col-map)]
                 (if (= :desc dec-inc) (reverse sorted) sorted))))

(defn do-clear-keys [keymap]
  (reduce (fn [acc kv] (let [[k v] (apply (fn [a b] [(name a) b]) kv)]
                        (if (or (= k "id") (= "id" (apply str (take 2 k)))) acc
                          (into acc {(keyword k) v})))) {}
          (seq keymap)))



;; (do-clear-keys {:datetime "Data/Czas" :direction "Kierunek" :rfid "RFID Karty" :teta_nr "Nr. Teta" :first_name "Imie" :last_name "Nazwisko" :section "Sekcja" :id_user "Użytkownik" :id_card "Karta"})
(defn add-header-text [vec-symb-list]
  (let [f (fn [acc x] (if-let [text (x *table-default-header*)] (into acc {x text}) acc))]
    (reduce f {} (map keyword vec-symb-list))))

(defmacro defview*
    "Short description:
  The macro (defview* A {:b \"1\"} (select :user))
  generate a datatype with `A` name
     (defrecord A [b])
  and two function:
     (table-model->A [m])
     (getA [sortedFunction] (select :user))

  Example generated Functions:
     `TableCard`
     `data<<-TableCard`
     `model<<-TableCard`
     `val-key-col<<-TableCard`
     `inc-dec-col<<-TableCard`
     `excel<<-TableCard`
     `table<<-TableCard`

  
  Description:
   The macro `defview` generate View module for seesaw table representation.
  Defmacro in first step create defrecord with name of first parameter `s`,
  which get keys from (keys `m`) evaluation.
   By the `m` param would be generated function, which generate table-model
  builder function. The function name start on `model<<-` symbol prefix.
   Third function is function selector. It's only wraper for SQL query, that
  return data in type declarated `s` record.
   Fourth and Fife functoin is helper function for Table component.
  `val-key-col<<-*` Return reversed key-value `m`.
  `inc-dec-col<<-*` Return map of `m` keys with deafult `:inc` values

  See also:
     (`hrtime.user-module/sort-by-column`)
     (`seesaw.table/table-model`)

  How To:
    (defview* TableCard {:rfid \"RFID\" teta_nr \"Nr. Teta\" first_name \"Imie\" last_name \"Nazwisko\"}
       \"SELECT rfid, teta_nr, first_name, last_name FROM card LEFT JOIN user ON user.id=card.id_user\")

    (defview* TableCard {:rfid \"RFID\" teta_nr \"Nr. Teta\" first_name \"Imie\" last_name \"Nazwisko\"}
       (select :card :column [:rfid :teta_nr :first_name :last_name] :left-join :user))

    (inc-dec-col<<-TableCard)
  
    (model<<-TableCard (data<<-TableCard (sort-by-column :last_name :inc))
    (model<<-TableCard (data<<-TableCard (sort-by-column :first_name :dec)))

    (data<<-TableCard (sort-by-column :first_name :dec))
    (data<<-TableCard (sort-by-column :first_name))
    (data<<-TableCard)

    (val-key-col<<-TableCard)
    (val-key-col<<-TableCard \"RF-ID\")

    (seesaw.core/frame :content (table<<-TableCard)) 
    (seesaw.core/frame :content (table<<-TableCard #'println)) 
    (seesaw.core/frame :content (table<<-TableCard (fn [x]....)))


  Explanation:
  (defview* TableCard {:rfid \"RFID\" teta_nr \"Nr. Teta\" first_name \"Imie\" last_name \"Nazwisko\"}
     (select :card :column [:rfid :teta_nr :first_name :last_name] :left-join :user))

  ;; expands to ;;
  
  (defrecord `TableCard` [rfid teta_nr first_name last_name])

  (defn `model<<-TableCard`
    [map-like-structure]
    [:columns [{:key :rfid, :text \"RF-ID\"} {:key :teta_nr, :text \"Teta NUM\"} {:key :first_name, :text \"Imie\"} {:key :last_name, :text \"Nazw.\"}]
     :rows (vec (concat map-like-structure))])

  (defn `val-key-col<<-TableCard`
    ([] {\"RF-ID\" :rfid, \"Teta NUM\" :teta_nr, \"Imie\" :first_name, \"Nazw.\" :last_name})
    ([k] (get-in {\"RF-ID\" :rfid, \"Teta NUM\" :teta_nr, \"Imie\" :first_name, \"Nazw.\" :last_name} [k])))

  (defn `inc-dec-col<<-TableCard`
    [] {:rfid :inc, :teta_nr :inc, :first_name :inc, :last_name :inc})

  (defn `data<<-TableCard`
    \"Wrapper for geting map from database\"
    [& [sorter-function]]
    (let [r (jdbc/query {:classname \"org.sql-connection.JDBC\", :subprotocol \"sql-connection\", :subname \"hrtime-test\"}
              (select :card :column [:rfid :teta_nr :first_name :last_name] :left-join :user))]
      (if (and sorter-function (fn? sorter-function)) (sorter-function r) r)))
  ...
  (defn `excel<<-TableCard` ...)
  (defn `tabel<<-TableCard` ...)"
  [s record-key cmap select-sql]
  (let [k (map symbol (keys (do-clear-keys cmap)))
        m (do-clear-keys cmap)
        
        allkeys (map symbol (keys cmap))
        
        --val-key (reduce (fn [a [k v] ] (into a {v k})) {} m)
        --model-columns (map (fn [[k v]] {:key k :text v}) m)
        --inc-dec-columns (reduce (fn [a [k v]] (into a {k :inc})) {} m)
        -f-info (str "info<<-" (name s))
        -f-record-header (str "record-header<<-" (name s))
        -f-model (str "model<<-" (name s))
        -f-data  (str "data<<-" (name s))
        ;; -f-data-sql  (str "data-sql<<-" (name s))
        -f-val-key-col (str "val-key-col<<-" (name s))
        -f-inc-dec-col (str "inc-dec-col<<-" (name s))
        -f-excel-export (str "excel<<-" (name s))
        -f-pdf-export (str "pdf<<-" (name s))
        -f-table-generate (str "table<<-" (name s))
        -f-scroll-table-generate (str "scroll-table<<-" (name s))
        -f-temporary-data (str "dump-data-" (gensym s))
        -f-dump-temporary-data (str "dump-data<<-" (name s))]
    `(do
       (defrecord ~s [~@record-key])
       (defn ~(symbol -f-record-header) []
         (add-header-text '[~@record-key]))
       (defn ~(symbol -f-info) []
         (println (clojure.string/join "\n" [(format "\nFor view %s:" ~(name s))
                                             "Available records:"
                                             (clojure.string/join "\n" (map #(format "\t(%s)" %)
                                                                            [(str ~(name s) " " (str '~k))]))
                                             "Available functions:"
                                             (clojure.string/join "\n"
                                                                  (map #(format "\t(%s)" %)
                                                                       [~-f-info 
                                                                        (str ~-f-model " data") 
                                                                        (str ~-f-data  " [:filter-lambda f, :sql s]")  
                                                                        ~-f-inc-dec-col
                                                                        (str ~-f-val-key-col " [k]")
                                                                        (str ~-f-excel-export " [data [sheet [path]]]")
                                                                        (str ~-f-pdf-export " [data [sheet [path]]]") 
                                                                        (str ~-f-table-generate " [f [data]")
                                                                        (str ~-f-scroll-table-generate " [f [data]]")
                                                                        (str ~-f-dump-temporary-data " [:filter-lambda f]")]))
                                             "Available Variables:"
                                             (clojure.string/join "\n" (map #(format "\t(%s)" %) [~-f-temporary-data]))])))
       (def ~(symbol -f-temporary-data) (ref nil))
       ;; (defrecord ~s [~@k])
       (defn ~(symbol -f-model)
         "Function build table-model"
         [~'map-like-structure]
         [:columns [~@--model-columns]
          :rows (vec (concat ~'map-like-structure))])
       (defn ~(symbol -f-val-key-col)
         ([] ~--val-key)
         ([~'k] (get-in ~--val-key [~'k])))
       (defn ~(symbol -f-inc-dec-col) [] ~--inc-dec-columns)
       (defn ~(symbol -f-data)
         "Wrapper for geting map from database"
         ([& {:keys [~'filter-lambda ~'sql] :or {~'filter-lambda nil ~'sql ~select-sql}}]
          (let [~'r (clojure.java.jdbc/query ~sql-connection (if (not (string? ~'sql)) (eval ~'sql) ~'sql))
                ~'db-data (if (and ~'filter-lambda (fn? ~'filter-lambda))
                            (~'filter-lambda ~'r) ~'r)]
            (dosync (ref-set ~(symbol -f-temporary-data) ~'db-data))
            ~'db-data)))
       (defn ~(symbol -f-dump-temporary-data) [& {:keys [~'filter-lambda] :or {~'filter-lambda nil}}]
         (if (and ~'filter-lambda (fn? ~'filter-lambda))
           (~'filter-lambda (deref ~(symbol -f-temporary-data)))
           (deref ~(symbol -f-temporary-data))))
       (defn ~(symbol -f-excel-export)
         ([]
          (let [[~'d & ~'t] (string/split (.format (java.text.SimpleDateFormat. "YYYY-MM-dd HH MM ss") (java.util.Date. )) #" ")]
            (~(symbol -f-excel-export) (deref ~(symbol -f-temporary-data)) (format "sheet") (str (if-let [~'path (str (~'seesaw.chooser/choose-file :type :save :selection-mode :dirs-only))]
                                                                         (str (string/trim ~'path) "\\"))
                                                                       (format "(%s)%s.xlsx" ~'d (string/join "" ~'t))))))
         ([~'data]
          (let [[~'d & ~'t] (string/split (.format (java.text.SimpleDateFormat. "YYYY-MM-dd HH MM ss") (java.util.Date. )) #" ")]
            (~(symbol -f-excel-export) ~'data (format "sheet") (str (if-let [~'path (str (~'seesaw.chooser/choose-file :type :save :selection-mode :dirs-only))]
                                                                  (str (string/trim ~'path) "\\")) 
                                                                (format "(%s)%s.xlsx" ~'d (string/join "" ~'t))))))
         ([~'data ~'sheet-name]
          (let [[~'d & ~'t] (string/split (.format (java.text.SimpleDateFormat. "YYYY-MM-dd HH MM ss") (java.util.Date. )) #" ")]
            (~(symbol -f-excel-export) ~'data ~'sheet-name (str (if-let [~'path (str (~'seesaw.chooser/choose-file :type :save :selection-mode :dirs-only))]
                                                                  (str (string/trim ~'path) "\\")) 
                                                                (format "(%s)%s.xlsx" ~'d (string/join "" ~'t))))))
         ([~'data ~'sheet-name ~'file-path]
          (let [~'workbook {~'sheet-name (excel/table (for [~'row ~'data]
                                                        (into {} (map (fn [~'tkey] {(~'tkey ~m) (~'tkey ~'row)})
                                                                      ~(vec (keys m))))))}]
            (excel/write! ~'workbook ~'file-path)
            (excel/open ~'file-path))))
       (defn ~(symbol -f-table-generate)
         ([]              (~(symbol -f-table-generate) #'identity    (~(symbol -f-data))))
         ([~'listener-fn] (~(symbol -f-table-generate) ~'listener-fn (~(symbol -f-data))))
         ([~'listener-fn ~'data]
          (let [~'T (seesaw.core/table  :model (~(symbol -f-model) ~'data))]
            (seesaw.core/listen ~'T :selection (fn [~'e] (~'listener-fn (seesaw.table/value-at ~'T (seesaw.core/selection ~'T)))))
            (let [~'inc-dec (ref (~(symbol -f-inc-dec-col)))
                  ~'val-key (~(symbol -f-val-key-col))]
              ;; (println ~'inc-dec ~'val-key)
              (addTableSorter ~'T
                              (fn [^java.awt.Point ~'point]
                                (let [~'k-to-sort (get-in ~'val-key [(.getColumnName ~'T (.columnAtPoint ~'T ~'point))])]
                                  (if (= (get-in (deref ~'inc-dec) [~'k-to-sort]) :asc)
                                    (do (dosync (ref-set ~'inc-dec (reduce (fn [~'x [~'a ~'b]] (into ~'x {~'a :desc})) {} (deref ~'inc-dec))))
                                        (dosync (ref-set ~'inc-dec (assoc-in (deref ~'inc-dec) [~'k-to-sort] :desc))))
                                    (do (dosync (ref-set ~'inc-dec (reduce (fn [~'x [~'a ~'b]] (into ~'x {~'a :desc})) {} (deref ~'inc-dec))))
                                        (dosync (ref-set ~'inc-dec (assoc-in (deref ~'inc-dec) [~'k-to-sort] :asc)))))
                                  ;; (seesaw.core/config! ~'T :model (~(symbol -f-model) (~(symbol -f-data) :sql (hrtime.sql-tool/change-expression '~select-sql :order [~'k-to-sort (get-in (deref ~'inc-dec) [~'k-to-sort])]))))
                                  (seesaw.core/config! ~'T :model (~(symbol -f-model) (~(symbol -f-dump-temporary-data) :filter-lambda (~'sort-by-column ~'k-to-sort (get-in (deref ~'inc-dec) [~'k-to-sort]))))))))))))
       (defn ~(symbol -f-scroll-table-generate)
         ([]              (seesaw.core/scrollable (~(symbol -f-table-generate) #'identity    (~(symbol -f-data))) :hscroll :as-needed :vscroll :as-needed))
         ([~'listener-fn] (seesaw.core/scrollable (~(symbol -f-table-generate) ~'listener-fn (~(symbol -f-data))) :hscroll :as-needed :vscroll :as-needed))
         ([~'listener-fn ~'data]
          (seesaw.core/scrollable (~(symbol -f-table-generate) ~'listener-fn ~'data) :hscroll :as-needed :vscroll :as-needed))))))

(defmacro defview [rec-name rec-keys & {:keys [view]}]
  (let [[table-representation build-in-sql] view
        into-view-map #(let [f (fn [acc x] (if-let [text (x *table-default-header*)] (into acc {x text}) acc))]
                         (reduce f {} (map keyword %)))]
    (cond
      (map?    table-representation) `(defview* ~rec-name ~rec-keys ~table-representation ~build-in-sql)
      (vector? table-representation) `(defview* ~rec-name ~rec-keys ~(into-view-map table-representation) ~build-in-sql))))


(defview User [id first_name last_name work_type section teta_nr]
  :view [[first_name last_name section work_type rfid teta_nr]
         (hrtime.sql-tool/select :user
                 :column [{:user.id :id} :first_name :last_name :section :work_type :rfid :teta_nr]
                 :left-join {:card.id_user :user.id})])


(defview Card [id rfid id_user]
  :view [[rfid teta_nr first_name last_name id_user]
         (hrtime.sql-tool/select :card
                 :column [{:card.id :id} :rfid :teta_nr :first_name :last_name]
                 :left-join :user)])

(defview Registration [id datetime direction id_user id_card]
  :view [[first_name teta_nr id_card section id rfid datetime last_name id_user direction]
         (hrtime.sql-tool/select :registration
                 :column [{:registration.id :id} :datetime :direction :rfid :teta_nr :first_name :last_name :section]
                 :left-join [:user :card])])


;; (selection-dialog "User")
(defn selection-dialog [table-name]
  (let [;; Generated table creator macros
        table<<- (eval `~(symbol (str "hrtime.database-manager/table<<-" table-name)))
        model<<- (eval `~(symbol (str "hrtime.database-manager/model<<-" table-name)))
        ddata<<- (eval `~(symbol (str "hrtime.database-manager/dump-data<<-" table-name)))
        
        ;; UI components 
        dialog   (seesaw.core/custom-dialog :modal? true :width 400 :height 500 :title "Wyszukiwarka")
        table    (table<<- #(seesaw.core/return-from-dialog dialog %))
        content  (hrtime.layout-lib/mig [[(seesaw.core/label :text "Wpisz frazę i kliknij - ENTER - by szukać w tabeli." :halign :center :icon (image-scale icon/loupe-blue-64-png 30))]
                                         [(seesaw.core/text :text ""
                                                            :halign :center
                                                            :border (seesaw.border/compound-border (seesaw.border/empty-border :thickness 5) (seesaw.border/line-border :bottom 1 :color (hrtime.layout-lib/style :color_bg_btn_hover)))
                                                            :listen [:action
                                                                     (fn [e]
                                                                       (let [input-text (seesaw.core/text e)]
                                                                         (if (= "" (clojure.string/trim input-text))
                                                                           (seesaw.core/config! table :model (ddata<<-)))
                                                                         (if (not (some #(= " " (str %)) input-text))
                                                                           (seesaw.core/config! table :model (model<<- (ddata<<- :filter-lambda (filter-col-by (re-pattern (clojure.string/trim input-text)))))))))])]
                                         [(seesaw.core/scrollable table :hscroll :as-needed :vscroll :as-needed)]])]
    (seesaw.core/config! dialog :content content)
    (seesaw.core/show! (doto dialog
                         (.setLocationRelativeTo nil)))))

;; (data<<-Card)
;; (excel<<-Card)
;; 
;; 
;; (data<<-Registration)
;; (excel<<-Registration)
;; 
;; 
;; (data<<-User)
;; (excel<<-User)

(defn get-human-readable-data 
  "(get-human-readable-data  {:datetime \"Data/Czas\"
                             :suka \"bliat\"
                             :direction \"Kierunek\"
                             :id_card \"Karta\"
                             :id_user \"Użytkownik\"})" [d]
  (clojure.string/join " " (reduce #(let [[kk v] %2 k (name kk)]
              (if (or (= k "id")
                      (= k "work_type")
                      (= "id" (apply str (take 2 k)))
                      (not= 0 (count (re-seq #"date" k))))
                %1 (conj %1 v))) [] (seq d))))



(defn construct-dialog [k v reference-value]
  (let [[_ related-table] (string/split k #"_")
        [h & r] related-table
        table-name (str (clojure.string/upper-case h) (apply str r))
        get-text (fn [] (if (nil? (get-in @reference-value [(keyword k)])) "Kliknij by wybrać"
                            (get-human-readable-data
                             (first (jdbc/query sql-connection (select (keyword related-table)
                                                                       :where (= :id ((keyword k) @reference-value))))))))
        selected-value (fn [e] (let [m (selection-dialog table-name)]
                                 (dosync (ref-set reference-value (clojure.core/assoc @reference-value (keyword k) (:id m))))
                                 (seesaw.core/config! e :text (get-text))))
        text-label (seesaw.core/label
                    :cursor :hand
                    :font (hrtime.layout-lib/style :font_regular)
                    :border (seesaw.border/compound-border (seesaw.border/empty-border :thickness 5) (seesaw.border/line-border :bottom 1 :color (hrtime.layout-lib/style :color_bg_btn_hover)))
                    :listen [:mouse-clicked selected-value]
                    :text (get-text))
        dialog-label (hrtime.layout-lib/mig [[text-label]
                                             [(seesaw.core/label :icon (image-scale icon/basket-grey1-64x64-png 40)
                                                                 :listen [:mouse-entered (fn [e] (seesaw.core/config! e :cursor :hand :icon (image-scale icon/basket-blue1-64x64-png 40)))
                                                                          :mouse-exited  (fn [e] (seesaw.core/config! e :cursor :default :icon (image-scale icon/basket-grey1-64x64-png 40)))
                                                                          :mouse-clicked (fn [e] (if ((keyword k) @reference-value) 
                                                                                                  (do (dosync (ref-set reference-value (assoc @reference-value (keyword k) nil)))
                                                                                                      (seesaw.core/config! text-label :text (get-text)))))])]]
                                            :args [:constraints ["" "0px[grow, fill]10px[20, fill]0px" "0px[grow, fill]0px"] :border v])]
    (seesaw.core/grid-panel :rows 1 :columns 3 :items [dialog-label])))

;; (seesaw.core/config! f :content (construct-datapicker "sdate" "dupa" (ref {:sdate "1998-10-1 00:00:00"})))

(defn construct-datapicker [k v reference-value]
  ;; setting default value for datapicker component
  (if (nil? (get-in @reference-value [(keyword k)])) (dosync (ref-set reference-value (clojure.core/assoc @reference-value (keyword k) (hrtime.sql-tool/date-format)))))
  (let [datapicker (if (nil? (get-in @reference-value [(keyword k)])) (hrtime.layout-lib/DataPicker)
                       (hrtime.layout-lib/DataPicker (get-in @reference-value [(keyword k)])))]
    (doto datapicker
      (seesaw.core/config! :border (seesaw.border/compound-border (seesaw.border/empty-border :thickness 5)
                                                                  (seesaw.border/line-border :thickness 1 :color "#fff")
                                                                  (seesaw.border/line-border :bottom 1 :color (hrtime.layout-lib/style :color_bg_btn_hover))))
      (seesaw.core/listen :state-changed
                          (fn [e]
                            (dosync (ref-set reference-value
                                             (clojure.core/assoc @reference-value (keyword k)
                                                                 (hrtime.sql-tool/date-format (seesaw.core/selection e))))))))
    (hrtime.layout-lib/mig 
     [[datapicker]] :args [ :border v])))


(defn construct-textinput [k v reference-value]
  (let [lambda (fn [e] (dosync (ref-set reference-value (clojure.core/assoc @reference-value (keyword k) (seesaw.core/text e)))))]
    (hrtime.layout-lib/mig [[(hrtime.layout-lib/mig 
                              [[(seesaw.core/text :text (get-in @reference-value [(keyword k)])
                                                  :font (hrtime.layout-lib/style :font_regular)
                                                  :border (seesaw.border/compound-border (seesaw.border/empty-border :thickness 5)
                                                                                         (seesaw.border/line-border :bottom 1 :color (hrtime.layout-lib/style :color_bg_btn_hover)))
                                                  :listen [:caret-update lambda])]]
                              :args [:border v])]])))

;; Combobox
(defn construct-combobox [k v reference-value]
  (let [lambda (fn [e] (dosync (ref-set reference-value (clojure.core/assoc @reference-value (keyword k) (seesaw.core/selection e)))))
        combo (seesaw.core/combobox :model (get-in UI-combo-column [(keyword k)])
                                    :font (hrtime.layout-lib/style :font_regular)
                                    ;; :background "#fff"
                                    :border (seesaw.border/compound-border (seesaw.border/empty-border :thickness 5)
                                                                           (seesaw.border/line-border :bottom 1 :color (hrtime.layout-lib/style :color_bg_btn_hover)))
                                    :listen [:action-performed lambda])]
    (if-let [selected-value ((keyword k) @reference-value)] 
      (seesaw.core/selection! combo selected-value)
      (seesaw.core/selection! combo nil))
    (hrtime.layout-lib/mig [[(hrtime.layout-lib/mig
                              [[combo]]
                              :args [:border v])]])))


(defn to-arrow [a-func table]
  (symbol (str "hrtime.database-manager/" a-func table)))


(defn construct-UI [^clojure.lang.Ref reference-value table-name kv-array to-table to-ui]
  (let [;; do local-backup
        [h & r] (name table-name)
        upper-case-table-name (symbol (str (clojure.string/upper-case h) (apply str r)))
        reference-value-back @reference-value
        search-component (eval (to-arrow 'search-UI- (symbol (name table-name))))
        ;; compontn-list (map vec (eval (search-component (ref {}) )))
        compontn-list (map vector (search-component (ref {:date-from (date 1998)
                                                          :date-to (date)}) kv-array to-table to-ui))
        ;; k (println compontn-list)
        ui-list (vec (map vector (filter #(not (nil? %))
                                         (for [[k v] (map #(let [[k v] %] [(name k) v]) (seq kv-array))]
                                           (cond
                                             (= k "id")                               nil
                                             (= "id" (apply str (take 2 k)))          (construct-dialog     k v reference-value)
                                             (not= 0 (count (re-seq #"date" k)))      (construct-datapicker k v reference-value)
                                             (in? (keys UI-combo-column) (keyword k)) (construct-combobox   k v reference-value)
                                             :else                                    (construct-textinput  k v reference-value))))))
        
        buttons-list [(hrtime.layout-lib/mig [[(hrtime.layout-lib/btn :text (hrtime.layout-lib/lang :btn_save)
                                                                      :user-data {:mode "s"}
                                                                      :halign :center
                                                                      :icon (image-scale icon/agree-64-png 30)
                                                                      :onClick (fn [x]
                                                                                 (if (not-empty (keys @reference-value))
                                                                                   (if-not (:id @reference-value)
                                                                                     (jdbc/execute! sql-connection (insert table-name :set @reference-value))
                                                                                     (jdbc/execute! sql-connection (update table-name :set (dissoc @reference-value :id) :where (= :id (:id @reference-value))))))
                                                                                 (do (dosync (ref-set reference-value {}))
                                                                                     (to-ui (hrtime.database-manager/construct-UI reference-value table-name kv-array to-table to-ui))
                                                                                     (to-table ((eval (to-arrow 'scroll-table<<- upper-case-table-name))
                                                                                                (fn [x] (if-let [ref-value (ref (first (get-sql-by-id table-name (:id x) (eval (to-arrow 'map-> upper-case-table-name)))))]
                                                                                                          (to-ui (hrtime.database-manager/construct-UI ref-value table-name kv-array to-table to-ui))))))
                                                                                     )))]
                                              [(hrtime.layout-lib/btn :text (hrtime.layout-lib/lang :btn_clean_inputs)
                                                                      :user-data {:mode "s"}
                                                                      :halign :center
                                                                      :icon (image-scale icon/ban-blue-64-png 30)
                                                                      :onClick (fn [x]
                                                                                 (dosync (ref-set reference-value {}))
                                                                                 (to-ui (hrtime.database-manager/construct-UI reference-value table-name kv-array to-table to-ui))
                                                                                 (to-table ((eval (to-arrow 'scroll-table<<- upper-case-table-name))
                                                                                            (fn [x] (if-let [ref-value (ref (first (get-sql-by-id table-name (:id x) (eval (to-arrow 'map-> upper-case-table-name)))))]
                                                                                                      (to-ui (hrtime.database-manager/construct-UI ref-value table-name kv-array to-table to-ui))))))))]
                                              
                                              [(hrtime.layout-lib/btn :text (hrtime.layout-lib/lang :btn_remove)
                                                                      :user-data {:mode "s"}
                                                                      :halign :center
                                                                      :icon (image-scale icon/x-blue1-64-png 30)
                                                                      :onClick (fn [x] (if-let [id (:id (deref reference-value))]
                                                                                        (if (hrtime.layout-lib/dlg "Czy napewno chcesz usunąć element?" "Informacja")
                                                                                            (do (jdbc/execute! sql-connection (delete table-name :where (= :id id)))
                                                                                              (dosync (ref-set reference-value {}))
                                                                                              (to-ui (hrtime.database-manager/construct-UI reference-value table-name kv-array to-table to-ui))
                                                                                              (to-table ((eval (to-arrow 'scroll-table<<- upper-case-table-name))
                                                                                                         (fn [x] (if-let [ref-value (ref (first (get-sql-by-id table-name (:id x) (eval (to-arrow 'map-> upper-case-table-name)))))]
                                                                                                                  (to-ui (hrtime.database-manager/construct-UI ref-value table-name kv-array to-table to-ui)))))))))))]
                                              
                                              ;; [(hrtime.layout-lib/btn :text (hrtime.layout-lib/lang :btn_back)
                                              ;;                         :user-data {:mode "s"}
                                              ;;                         :halign :center
                                              ;;                         :icon (image-scale icon/arrow-blue-grey-left-64-png 30)
                                              ;;                         :onClick (fn [x] (do (dosync (ref-set reference-value reference-value-back))
                                              ;;                                              (to-ui (hrtime.database-manager/construct-UI reference-value table-name kv-array to-table to-ui))))) "span"]
                                              ]
                                             :args [:constraints ["wrap 3" "0px[grow, fill]0px" "0px[grow, fill]0px"]])]
        
        panel-component-list (if (= :registration table-name) compontn-list
                                 (concat compontn-list ui-list [buttons-list]))]
    
    (hrtime.layout-lib/mig panel-component-list :args[:constraints ["wrap 1" "0px[grow, fill]0px" "10px[grow, fill]0px"]])))



(defn search-UI-registration [^clojure.lang.Ref reference-value kv-array to-table to-ui]
  (let [;; do local-backup
        reference-value-back @reference-value
        ;; editable UI lists
        ui-list [(construct-datapicker "date-from" "Data od" reference-value)
                 (construct-datapicker "date-to"  "Data do" reference-value)
                 (construct-textinput  "in-all-column" "Wyszukaj w tekscie" reference-value)]
        buttons-list [(hrtime.layout-lib/mig [[(hrtime.layout-lib/btn :text (hrtime.layout-lib/lang :btn_clean_filter)
                                                                       :user-data {:mode "s"}
                                                                       :halign :center
                                                                       :icon (image-scale icon/ban-blue-64-png 30)
                                                                       :onClick (fn [x]
                                                                                 (to-table (scroll-table<<-Registration (fn [x]
                                                                                                                          (if-let [ref-value (ref (first (get-sql-by-id :registration (:id x) map->Registration)))]
                                                                                                                            (to-ui (hrtime.database-manager/construct-UI ref-value :registration kv-array to-table to-ui))))))))]
                                               [(hrtime.layout-lib/btn :text (hrtime.layout-lib/lang :btn_search)
                                                                       :user-data {:mode "s"}
                                                                       :halign :center
                                                                       :icon (image-scale icon/loupe-blue1-64-png 30)
                                                                       :onClick (fn [x]
                                                                                  (let [_TMP1 (hrtime.sql-tool/select :registration
                                                                                                                      :column [{:registration.id :id} :datetime :direction :rfid :teta_nr :first_name :last_name :section]
                                                                                                                      :left-join [:user :card] :where (between :datetime (:date-from @reference-value) (:date-to @reference-value)))]
                                                                                    (let [d<<- (if (or (nil? (:in-all-column @reference-value)) (= "" (clojure.string/trim (:in-all-column @reference-value))))
                                                                                                 (data<<-Registration :sql _TMP1)
                                                                                                 (data<<-Registration :sql _TMP1 :filter-lambda (filter-col-by (re-pattern (:in-all-column @reference-value)))))]
                                                                                      (to-table (scroll-table<<-Registration (fn [x]
                                                                                                                               (if-let [ref-value (ref (first (get-sql-by-id :registration (:id x) map->Registration)))]
                                                                                                                                 (to-ui (hrtime.database-manager/construct-UI ref-value :registration kv-array to-table to-ui)))) d<<-)))
                                                                                    )))]]
                                             :args [:constraints ["wrap 2" "0px[grow, fill]0px" "0px[grow, fill]0px"]])]]
    (concat ui-list buttons-list)))

;; (data<<-Registration :filter-lambda (filter-col-by (re-pattern "we")))

(defn search-UI-user [^clojure.lang.Ref reference-value kv-array to-table to-ui]
  (let [;; do local-backup
        reference-value-back @reference-value
        ;; editable UI lists
        ui-list [(construct-textinput "in-all-column" "Wyszukaj" reference-value)]
        buttons-list [(hrtime.layout-lib/mig [[(hrtime.layout-lib/btn :text (hrtime.layout-lib/lang :btn_clean_filter)
                                                                      :user-data {:mode "s"}
                                                                      :halign :center
                                                                      :icon (image-scale icon/ban-blue-64-png 30)
                                                                      :onClick (fn [x]
                                                                                 (to-table (scroll-table<<-User (fn [x]
                                                                                                           (if-let [ref-value (ref (first (get-sql-by-id :user (:id x) map->User)))]
                                                                                                             (to-ui (hrtime.database-manager/construct-UI ref-value :user kv-array to-table to-ui))))))))]
                                              [(hrtime.layout-lib/btn :text (hrtime.layout-lib/lang :btn_search)
                                                                      :user-data {:mode "s"}
                                                                      :halign :center
                                                                      :icon (image-scale icon/loupe-blue1-64-png 30)
                                                                      :onClick (fn [x] (let [d<<- (if (or (nil? (:in-all-column @reference-value)) (= "" (clojure.string/trim (:in-all-column @reference-value))))
                                                                                                   (data<<-User)
                                                                                                   (data<<-User :filter-lambda (filter-col-by (re-pattern (:in-all-column @reference-value)))))]
                                                                                        (to-table (scroll-table<<-User (fn [x]
                                                                                                                  (if-let [ref-value (ref (first (get-sql-by-id :user (:id x) map->User)))]
                                                                                                                    (to-ui (hrtime.database-manager/construct-UI ref-value :user kv-array to-table to-ui)))) d<<-)))))]] :args [:constraints ["wrap 2" "0px[grow, fill]0px" "0px[grow, fill]0px"]])]]
    (concat ui-list buttons-list)))

(defn search-UI-card [^clojure.lang.Ref reference-value kv-array to-table to-ui]
  (let [;; do local-backup
        reference-value-back @reference-value
        ;; editable UI lists
        ui-list [(construct-textinput "in-all-column" "Wyszukaj" reference-value)]
        buttons-list [(hrtime.layout-lib/mig [[(hrtime.layout-lib/btn :text (hrtime.layout-lib/lang :btn_clean_filter)
                                                                      :user-data {:mode "s"}
                                                                      :halign :center
                                                                      :icon (image-scale icon/x-blue1-64-png 30)
                                                                      :onClick (fn [x]
                                                                                 (to-table (scroll-table<<-Card (fn [x]
                                                                                                                  (if-let [ref-value (ref (first (get-sql-by-id :card (:id x) map->Card)))]
                                                                                                                    (to-ui (hrtime.database-manager/construct-UI ref-value :card kv-array to-table to-ui))))))))]
                                              [(hrtime.layout-lib/btn :text (hrtime.layout-lib/lang :btn_search)
                                                                      :user-data {:mode "s"}
                                                                      :halign :center
                                                                      :icon (image-scale icon/loupe-blue-64-png 30)
                                                                      :onClick (fn [x] (let [d<<- (if (or (nil? (:in-all-column @reference-value)) (= "" (clojure.string/trim (:in-all-column @reference-value))))
                                                                                                   (data<<-Card)
                                                                                                   (data<<-Card :filter-lambda (filter-col-by (re-pattern (:in-all-column @reference-value)))))]
                                                                                        (to-table (scroll-table<<-Card (fn [x]
                                                                                                                  (if-let [ref-value (ref (first (get-sql-by-id :card (:id x) map->Card)))]
                                                                                                                    (to-ui (hrtime.database-manager/construct-UI ref-value :card kv-array to-table to-ui)))) d<<-)))))]]
                                              :args [:constraints ["wrap 2" "0px[grow, fill]0px" "0px[grow, fill]0px"]])]

        ]
    (concat ui-list buttons-list)))

;; (construct-UI (ref #hrtime.database_manager.User{:id nil, :first_name nil, :last_name nil, :work_type nil, :section nil, :teta_nr nil})  :user {:first_name "Imie", :last_name "Nazwisko", :work_type "Typ pracownika", :section "Dział", :teta_nr "Nr w systemie"})

;; (-> (seesaw.core/frame :content (seesaw.core/horizontal-panel :items (search-UI-Card (ref {}) {})))
;;     seesaw.core/pack!
;;     seesaw.core/show!)


(defmacro def-view-component [table-kwd  & {:keys [is-editable?] :or {is-editable? true}}]
  (let [[h & r] (name table-kwd)
        table-name (symbol (str (clojure.string/upper-case h) (apply str r)))
        function-name (symbol (str "view-" (name table-kwd)))
        ;; declare row function
        map->             (to-arrow 'map->            table-name)
        excel<<-          (to-arrow 'excel<<-         table-name)
        record-header<<-  (to-arrow 'record-header<<- table-name)
        scroll-table<<-   (to-arrow 'scroll-table<<-  table-name)]
    `(defn ~function-name [~'to-ui ~'to-table ~'excel-exporter-lambda]
      (let [~'ref-value (ref (~map-> {}))
            ~'struct-hint (~record-header<<-)]
        (~'to-ui (construct-UI ~'ref-value ~table-kwd ~'struct-hint ~'to-table ~'to-ui))
        (~'to-table (~scroll-table<<- (fn [~'x]
                                        (if-let [~'ref-value (ref (first (get-sql-by-id ~table-kwd (:id ~'x) ~map->)))]
                                          (~'to-ui (hrtime.database-manager/construct-UI ~'ref-value ~table-kwd ~'struct-hint ~'to-table ~'to-ui))))))
        ;; (~'excel-exporter-lambda (fn [e] ((eval excel<<-))))
        (~'excel-exporter-lambda [:mouse-clicked (fn [~'e] (~excel<<-))])))))


;; (jdbc/query sql-connection "SELECT registration.id AS id, datetime, direction, rfid, teta_nr, first_name, last_name, section FROM registration LEFT JOIN user ON user.id=registration.id_user LEFT JOIN card ON card.id=registration.id_card WHERE datetime BETWEEN \"1998-01-01 00:00:00\" AND \"2014-03-13 17:07:15\" ")


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Declare table view ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(do
  (def-view-component :user)
  (def-view-component :card)
  (def-view-component :registration))

;; (def f (seesaw.core/frame :content (seesaw.core/label)))
;; (def t (seesaw.core/frame :content (seesaw.core/label)))
;; (-> f seesaw.core/pack! seesaw.core/show!)
;; (-> t seesaw.core/pack! seesaw.core/show!)
;; (view-registration #(seesaw.core/config! f :content (seesaw.core/scrollable % :hscroll :as-needed :vscroll :as-needed))
;;                    #(seesaw.core/config! t :content %)
;;                    #(identity %))
;; (view-card #(seesaw.core/config! f :content (seesaw.core/scrollable % :hscroll :as-needed :vscroll :as-needed))
;;            #(seesaw.core/config! t :content %)
;;            #(identity %))
;; (view-user #(seesaw.core/config! f :content (seesaw.core/scrollable % :hscroll :as-needed :vscroll :as-needed))
;;            #(seesaw.core/config! t :content %)
;;            #(identity %))







