(ns hrtime.layout-lib
  (:gen-class)
  (:use seesaw.dev
        seesaw.core
        seesaw.mig
        seesaw.border
        seesaw.make-widget
        seesaw.to-widget)
  (:require [clojure.string :as string]
            [hrtime.sql-tool :as sql]
            [hrtime.dev-tools :as tools]
            [hrtime.icon-library :as icons]
            [hrtime.config-manager :as cfg]))


;;;;;;;;;;;;;;;;;;
;;; LOAD THEME ;;;
;;;;;;;;;;;;;;;;;;

(def theme-fn (cfg/config-file (format "themes/%s.edn" (first ((cfg/config-file "themes/theme_config.edn") [:selected-theme-select])))))

(defn update-theme []
  (let [loaded-map (ref (theme-fn))]
    (fn [col-key] (get-in @loaded-map col-key))))

(def theme (ref (update-theme)))

(defn style-fn [] {:color_jarman_bar     (theme [:color :jarman :bar])
                   :color_jarman         (theme [:color :jarman :jarman])
                   :color_jarman_light   (theme [:color :jarman :light])
                   :color_jarman_dark    (theme [:color :jarman :dark])

                   :color_bg                   (theme [:color :background :main])
                   :color_bg_btn               (theme [:color :background :button])
                   :color_bg_btn_hover         (theme [:color :background :button_hover])
                   :color_bg_btn_selected      (theme [:color :background :button_selected])
                   :color_bg_btn_config        (theme [:color :background :button_config])

                   :color_bg_btn_main          (theme [:color :main-button :background])
                   :color_bg_btn_main_clicked  (theme [:color :main-button :clicked])

                   :color_fg                   (theme [:color :foreground :main])
                   :color_fg_btn               (theme [:color :foreground :button])
                   :color_fg_btn_hover         (theme [:color :foreground :button_hover])
                   :color_fg_btn_selected      (theme [:color :foreground :button_selected])
                   :color_border               (theme [:color :border])

                   :color_font_doc             (theme [:color :foreground :doc-font-color])
                   :color_font_.txt            (theme [:color :foreground :.txt-font-color])

                   :frame_width                (theme [:frame :width])
                   :frame_heigh                (- (theme [:frame :width]) 500)

                   :font_size_l         (theme [:font :size-large])
                   :font_size_m         (theme [:font :size-medium])
                   :font_size_n         (theme [:font :size-normal])
                   :font_size_s         (theme [:font :size-small])
                   :font_style          (theme [:font :style])

                   :font_bold           {:name (theme [:font :bold]) :size (theme [:font :size-normal])}
                   :font_light          {:name (theme [:font :light]) :size (theme [:font :size-normal])}
                   :font_medium         {:name (theme [:font :medium]) :size (theme [:font :size-normal])}
                   :font_regular        {:name (theme [:font :regular]) :size (theme [:font :size-normal])}

                   :font_name           {:name (theme [:font :regular])}
                   :font_name_bold      {:name (theme [:font :bold])}

                   :mini_btn            [:bg          (theme [:ui :special-button :background])
                                         :bg-hover    (theme [:ui :special-button :background-hover])
                                         :fg          (theme [:ui :special-button :foreground])
                                         :fg-hover    (theme [:ui :special-button :foreground-hover])
                                         :halign      (theme [:ui :special-button :horizontal-align])
                                         :cursor      (theme [:ui :special-button :cursor])
                                         :font-style  (theme [:ui :special-button :font-style])]})
(def app-style (ref (style-fn)))
(defn style
  ([key] (get @app-style key))
  ([key add] (conj (get @app-style key) add)))
;; (style :color_bg_btn)

(defn lang-fn [] {:btn_save            (theme [:language :buttons :save])
                  :btn_save_and_reload (theme [:language :buttons :save-and-reload])
                  :btn_register        (theme [:language :buttons :register])
                  :btn_workers         (theme [:language :buttons :workers])
                  :btn_rfid            (theme [:language :buttons :rfid])
                  :btn_settings        (theme [:language :buttons :settings])
                  :btn_search          (theme [:language :buttons :search])
                  :btn_clean_filter    (theme [:language :buttons :filter-clean])
                  :btn_clean_inputs    (theme [:language :buttons :inputs-clean])
                  :btn_remove          (theme [:language :buttons :remove])
                  :btn_disconnect_rfid (theme [:language :buttons :disconnect-RFID])
                  :btn_back            (theme [:language :buttons :back])
                  :btn_run_app         (theme [:language :buttons :run-app])
                  :btn_excel_export    (theme [:language :buttons :excel-export])
                  ;; database information
                  :btn_test_connection (theme [:language :buttons :test-connection])
                  :info_connection_suc (theme [:language :buttons :test-connection-success])
                  :info_connection_fal (theme [:language :buttons :test-connection-fail])
                  :info_testing_connection       (theme [:language :alerts  :testing-connection])

                  :info_changes_saved            (theme [:language :alerts  :changes-saved])
                  :info_changes_saved_and_reload (theme [:language :alerts  :changes-saved-but-need-reload])
                  :no  (theme [:language :basic :no])
                  :yes (theme [:language :basic :yes])
                  })

(def app-lang (ref (lang-fn)))
(defn lang ([key] (get @app-lang key)))

(defn reload-config []
  (do
    (dosync (ref-set theme (update-theme)))
    (dosync (ref-set app-style (style-fn)))
    (dosync (ref-set app-lang (lang-fn)))))


;; (def .lang_save_button     (theme [:language :save-button]))
;; (def .lang_register_button (theme [:language :register-button]))
;; (def .lang_workers_button  (theme [:language :workers-button]))
;; (def .lang_rfid_button     (theme [:language :rfid-button]))
;; (def .lang_settings_button (theme [:language :settings-button]))


; Imports for data piceker
(import java.util.Calendar)
(import javax.swing.SpinnerDateModel)
(import javax.swing.JSpinner$DateEditor)
(import javax.swing.text.DateFormatter)


(defn DataPicker
  "Create data spiner.
  The argument `some-date` set 
  a default date for spinner element
  *data-format* Is a standart parser pattern in variable
  Parsing pattern 'yyyy-MM-dd hh:mm:ss' locate
  in *data-format* dynamic variable, if you
  want to change that, just do binding

  Example:
    (DataPicker)
    (DataPicker \"1998-10-23 10:10:11\")
    (DataPicker (java.util.Date.))
    (binding [sql/*data-format* \"yyyy-MM-dd\"]
      (DataPicker \"1998-10-29\"))"
  ([] (DataPicker (sql/date)))
  ([some-date]
   (let [date-obj (cond (string? some-date) (sql/date-to-obj some-date)
                        (= java.util.Date (class some-date)) some-date :else (sql/date))
         spinner (javax.swing.JSpinner. (javax.swing.SpinnerDateModel.))
         date-editor (javax.swing.JSpinner$DateEditor. spinner "dd-MM-yyyy")]
     (.setAllowsInvalid (cast javax.swing.text.DateFormatter (.getFormatter (.getTextField date-editor))) false)
     (doto spinner
       (.setEditor date-editor)
       (.setValue  date-obj)))))





(defn mig
  [items & {:keys [args] :or {args []}}]
  (apply mig-panel
         :items items
         :background (style :color_bg)
         :constraints ["wrap 1" "0px[grow, fill]0px" "5px[grow, fill]0px"]
         args))

(defn btn "Buttons with pre-style"
  [& {:keys [text
             id
             onClick
             icon
             halign
             hover-bg-color
             hover-fg-color
             hover-bold?
             background
             underline
             fg-color
             args]
      :or {text ""
           id nil
           onClick (fn [x] (identity x))
           icon nil
           halign :left
           hover-bg-color (style :color_bg_btn_hover)
           hover-fg-color (style :color_fg_btn_hover)
           hover-bold? false
           background (style :color_bg_btn)
           underline nil
           fg-color (style :color_fg_btn)
           args []}}]
  (let [btn (apply label 
                   :text (string/join "" [(if-not (= halign :left) " ") text]) 
                   :id id 
                   :user-data {:clicked false} 
                   :cursor :hand 
                   :listen [:mouse-clicked onClick] 
                   :icon icon
                   :halign halign
                   :border (if (= (get :user-data :mode) "s") nil
                               (compound-border  (empty-border :left 10 :top 6 :bottom 6 :right 6) (line-border :bottom 2 :color (if-not (= underline nil) underline background))))
                   :background background
                   :font (style :font_regular {:color fg-color})
                   args)]
    (doto btn
      (listen :mouse-entered (fn [e] (if (= (get (config e :user-data) :clicked) false) (config! e
                                                                                                 :background hover-bg-color
                                                                                                 :foreground hover-fg-color
                                                                                                 :font (if (= hover-bold? true) (style :font_bold) (style :font_regular))
                                                                                                 :border  (cond 
                                                                                                            (= (get (config e :user-data) :mode) "s") nil
                                                                                                            :else (compound-border  (empty-border :left 10 :top 6 :bottom 6 :right 6) (line-border :bottom 2 :color hover-bg-color)))))))
      (listen :mouse-exited (fn [e] (if (= (get (config e :user-data) :clicked) false) (config! e
                                                                                                :background background
                                                                                                :foreground fg-color
                                                                                                :font (style :font_regular)
                                                                                                :border  (compound-border  (empty-border :left 10 :top 6 :bottom 6 :right 6) (line-border :bottom 2 :color (if-not (= underline nil) underline background))))))))))


(defn icon-btn [& {:keys [icon
                          args]
                   :or {text ""
                        id nil
                        onClick (fn [x] (identity x))
                        icon nil
                        args []}}]
  (let [btn (apply label
                   :cursor :hand
                   :icon icon
                   :halign :center
                   :background (style :color_bg)
                   args)] 
    btn))



(defn btn-start [& {:keys [text
                           icon
                           halign
                           hover-bg-color
                           hover-fg-color
                           hover-bold?
                           background
                           underline
                           fg-color
                           args]
                   :or {text ""
                        id nil
                        onClick (fn [x] (identity x))
                        icon nil
                        halign :center
                        hover-bg-color (style :color_bg_btn_hover)
                        hover-fg-color (style :color_fg_btn_hover)
                        hover-bold? false
                        background (style :color_bg_btn)
                        underline nil
                        fg-color (style :color_fg_btn)
                        args []}}]
  (let [btn (apply label
                   :text text
                   :user-data {:clicked false}
                   :cursor :hand
                   :icon icon
                   :halign halign
                   :border (empty-border :thickness 20)
                   :background background
                   :font (style :font_name {:color fg-color :size (style :font_size_m)})
                   args)]
    (doto btn
      (listen :mouse-entered (fn [e] (config! e
                                              :background hover-bg-color
                                              :foreground hover-fg-color
                                              :font (if (= hover-bold? true)
                                                      (style :font_name_bold {:size (style :font_size_m)})
                                                      (style :font_name {:size (style :font_size_m)})))))
      (listen :mouse-exited (fn [e] (config! e
                                             :background background
                                             :foreground fg-color
                                             :font (style :font_name {:size (style :font_size_m)})))))))


(defn msg-error
  ([text title]
   (let [;; Generated table creator macros
         dialog   (seesaw.core/custom-dialog :modal? true :width 320 :height 150 :title title)
         content  (hrtime.layout-lib/mig [[(seesaw.core/label :text (string/join ["  " text]) 
                                                              :halign :center 
                                                              :font {:name (style :font-name) :size (- (style :font_size_m) 1)}
                                                              :icon (tools/image-scale icons/x-blue1-64-png 30))]
                                          [(hrtime.layout-lib/mig [[(hrtime.layout-lib/btn :text "OK"
                                                                                           :user-data {:mode "s"}
                                                                                           :halign :center
                                                                                           :underline (style :color_bg_btn_hover)
                                                                                           :onClick (fn [e] (dispose! e)))]]
                                                                  :args [:constraints ["" "0px[grow,fill]0px" "0px[grow, fill]0px"]])]]
                                         :args [:constraints ["wrap 1" "10px[grow,fill]10px" "0px[grow]0px"]])]
     (seesaw.core/config! dialog :content content)
     (seesaw.core/show! (doto dialog
                          (.setLocationRelativeTo nil))))))

;; (msg-error (lang :info_connection_fal) "Błąd połączenia")

(defn dlg
  ([text]
   (dlg text "Dialog" (fn [_] true) (fn [_] nil)))
  ([text title]
   (dlg text title (fn [_] true) (fn [_] nil)))
  ([text title ok-lambda no-lambda]
   {:pre [(fn? ok-lambda) (fn? no-lambda)]}
   (let [;; Generated table creator macros
         dialog   (seesaw.core/custom-dialog :modal? true :width 320 :height 150 :title title)
         content  (hrtime.layout-lib/mig [[(seesaw.core/label :text text :halign :center :font {:name (style :font-name) :size (- (style :font_size_m) 1)})]
                                          [(hrtime.layout-lib/mig [[(hrtime.layout-lib/btn :text (lang :no)
                                                                                           :user-data {:mode "s"}
                                                                                           :halign :center
                                                                                           :underline (style :color_bg_btn_hover)
                                                                                           :icon (tools/image-scale icons/X-64x64-png 30)
                                                                                           :onClick #(return-from-dialog dialog (no-lambda %)))]
                                                                   [(hrtime.layout-lib/btn :text (lang :yes)
                                                                                           :user-data {:mode "s"}
                                                                                           :halign :center
                                                                                           :underline (style :color_bg_btn_hover)
                                                                                           :icon (tools/image-scale icons/agree-64-png 30)
                                                                                           :onClick #(return-from-dialog dialog (ok-lambda %)))]]
                                                                  :args [:constraints ["" "5px[grow,fill]5px" "0px[grow, fill]0px"]])]]
                                         :args [:constraints ["wrap 1" "10px[grow,fill]10px" "0px[grow]0px"]])]
     (seesaw.core/config! dialog :content content)
     (seesaw.core/show! (doto dialog
                          (.setLocationRelativeTo nil))))))


