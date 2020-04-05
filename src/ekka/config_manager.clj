;; How to use it?
;; config-manager is simply function for managing configuration of application.
;;
;; <problem description>
;;    <problem code solution>
;;
;; Print all configuration tree for debbugin aims
;;    (print-config-tree)
;;    (print-config-tree "./config")
;;
;; Get configuration getter and setter
;;    (def configuration (config-file \"system.clj\")
;;    (configuration) 
;;    (configuration [:blue]) 
;;    (configuration [:blue] "#00A")
;; 

(ns hrtime.config-manager
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.java.io :as io]))

(defn- separator []
  (if (= (java.io.File/separator) "\\") #"\\" #"/"))

(def ^:dynamic *configuration-path* "configuration path directory" "config")
(def ^:dynamic *markdown-files* ["md" "markdown"])
(def ^:dynamic *xml-files*      ["html" "xhtml" "xml"])
(def ^:dynamic *text-files*     ["txt" "text" "org"])
(def ^:dynamic *json-files*     ["json"])
(def ^:dynamic *clojure-files*  ["clj" "clojure"])
(def ^:dynamic *config-files*   ["edn"])
(def ^:dynamic *csv-files*      ["csv"])
(def ^:dynamic *shell-files*    ["sh" "bash" "fish"])
(def ^:dynamic *prop-files*     ["properties" "prop" "cfg"])
(def ^:dynamic *supported-config-files* "list of all supported files"
  (concat *markdown-files*
          *xml-files*
          *text-files*
          *clojure-files*
          *config-files*
          *json-files*
          *csv-files*  
          *shell-files*
          *prop-files*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize config folder ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let [file*configuration-root* (java.io.File. *configuration-path*)]
  (if-not file*configuration-root*
    (.mkdir file*configuration-root*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper test functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn in? [col x ]
  (if (and (not (string? col)) (seqable? col))
    (some #(= x %) col)
    (= x col)))

(defn is-supported?
  "Example:
     (is-supported? \"temp.clj\") ;=> true
     (is-supported? \"temp.Kt1\") ;=> nil"
  [file-name]
  {:pre (string? file-name)}
  (if-not (some #(= \. %) (seq file-name)) nil
          (let [frmt (last (clojure.string/split file-name #"\."))]
            (some #(= frmt %) *supported-config-files*))))

(defn is-file? [file-name]
  {:pre (string? file-name)}
  (.isFile (clojure.java.io/file file-name)))


;;;;;;;;;;;;;;;;;;;;;;
;;; debug function ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defn file-extension
  "Example:
     (is-supported? \"temp.clj\") ;=> true
     (is-supported? \"temp.Kt1\") ;=> nil"
  [file-name]
  {:pre (string? file-name)}
  (if-not (some #(= \. %) (seq file-name)) nil
          (let [frmt (last (clojure.string/split file-name #"\."))]
            (condp in? frmt
              *markdown-files*  "markdown"
              *xml-files*       "xml"
              *text-files*      "text"
              *clojure-files*   "clojure"
              *json-files*      "json"
              *config-files*    "sys-conf"
              *csv-files*       "csv"
              *shell-files*     "script"
              *prop-files*      "shell"
              "unsupported file"))))

(defn- debug-tree [str-file-name n]
  (let [path (clojure.string/split str-file-name (separator))
        node-name (fn [str-file-name]
                    (if (and (is-file? str-file-name))
                      (format "%s [%s]" (.getName (clojure.java.io/file str-file-name)) (file-extension str-file-name))
                      (format "%s" (.getName (clojure.java.io/file str-file-name)))))]
    (println (str (last path)))
    (doseq [f-d (sort-by #(.isDirectory %) (.listFiles (clojure.java.io/file str-file-name)))]
      (if (.isFile f-d) (println (str (apply str (repeat n "| ")) "| " (node-name (str f-d))))
          (do (print (str (apply str (repeat n "| ")) "+-")) (debug-tree (str f-d) (inc n)))))))


(defn print-config-tree
  "Example:
    (print-config-tree)
    (print-config-tree \"./config\")"
  ([] (print-config-tree *configuration-path*))
  ([configuration-path] (let [_tmp_01(apply str (repeat (+ 1 (count configuration-path)) "="))]
        (println "\nConfiguration tree: |" configuration-path)
        (println (str "====================|" _tmp_01))
        (debug-tree configuration-path 0)
        (println (str "=====================" _tmp_01)))))

(defn config-map "(config-map \"system.clj\")" [^java.lang.String path]
  (read-string (slurp (java.io.File. (clojure.string/join (java.io.File/separator) [*configuration-path* path])))))

(defn create-if-not-exist [^String path]
  {:pre [((every-pred string? not-empty) path)]}
  (let [p (clojure.string/join (java.io.File/separator) [*configuration-path* path])]
    (if-not (.exists (io/file p))
      (do (spit p (prn-str {})) p)
      p)))

;;; DEPRECATED
;; (defn get-theme
;;   "Example: 
;;      ((config-file-getter \"system.clj\") [:blue])"
;;   [pcf]
;;   (fn [keyword-config-path]
;;     (let [configuration pcf]
;;       (if (map? configuration)
;;         (get-in configuration keyword-config-path nil)
;;         nil))))


(defn config-file
  "Example: 
     ((config-file \"system.clj\") [:blue])"
  [^java.lang.String path]
  (fn([] (let [configuration (read-string (slurp (java.io.File. (create-if-not-exist path))))]
          (if (map? configuration)
            configuration nil)))
    ([keyword-config-path] (let [configuration (read-string (slurp (java.io.File. (create-if-not-exist path))))]
                             (if (map? configuration)
                               (get-in configuration keyword-config-path nil) nil)))
    ([keyword-config-path value] (let [configuration (read-string (slurp (java.io.File. (create-if-not-exist path))))]
                                   (if (map? configuration)
                                     (spit (create-if-not-exist path)
                                           (prn-str (assoc-in configuration keyword-config-path value))))))))



