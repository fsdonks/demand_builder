;;migrated from: https://github.com/fsdonks/Phase-Shifter

(ns demand_builder.phaseshifter
  (:import [javax.swing JFrame JFileChooser JTextArea JPanel JLabel]))


(defn ->frame [& args] ;;Window for error message, first args is Title, second args is message
  (let [f (JFrame.) p (JPanel.) ta (JTextArea.)]
    (.setSize f 500 500) (.setDefaultCloseOperation f 2)
    (.setRows ta 100) (.setSize ta 475 475) (.setLineWrap ta true)
    (.setTitle f (first args)) (.setText ta (second args))
    (.add p ta) (.add f p) (.setVisible f true) f))


;; Replaces the nth value in line with newval
(defn replace-n [line n newval]
  (apply concat [(conj (vec (take n line)) newval) (vec (take-last (dec (- (count line) n)) line))]))


;; Equal to .IndexOf
(defn position-of [seq thing]
  (get (zipmap seq (range (count seq))) thing))


(def phaseheader {"DemandGroup" 0, "Phase" 1, "Start" 2, "Duration" 3}) ;;phases build from demand file, gauranteed to be in this format


(defn read-header [file] ;;Returns map with Column name as head and index as key
  (with-open [r (clojure.java.io/reader file)]
    (let [h (clojure.string/split (first (line-seq r)) #"\t")]
      (zipmap h (range (count h))))))


(defn read-demand-file [demandfile] ;;Reads lines into vector, used to have formatting
  (with-open [r (clojure.java.io/reader demandfile)]
    (into [] (pmap #(clojure.string/split % #"\t") (drop 1 (line-seq r))))))


(defn read-phase-changes [pcfile]
  (with-open [r (clojure.java.io/reader pcfile)] ;;Reads lines into vector, used to have formatting
      (let [header (read-header pcfile)
            lines (into [] (pmap #(clojure.string/split % #"\t") (drop 1 (line-seq r))))]
        (doseq [line lines]
          (if (not (zero? (rem (read-string (nth line (get header "Change"))) 8)))
            (do 
              (->frame "Bad Change Quantity Error" (str "Phase change must be divisible by 8.\n\n @Line:\n" line))
              (throw (Exception. "Bad Change Quantity Error")))))
        lines)))
          


;;; =========== Functions to get phases from formatted demand file =============
;;Returns the start of the phase
(defn find-start [ps] (apply min (map #(read-string (str (nth % 2))) ps)))


;;Returns the duration of the phase
(defn find-dur [ps]
  (if (= 1 (count ps))
    (read-string (str (nth (first ps) 3)))
    (let [md (find-start ps)
          durs (filter #(not= md (read-string (str (nth % 2)))) ps)]
      (apply + (map #(read-string (str (nth % 3))) durs)))))


;;Helper function to build phase map. Returns the phases for each phase by demand group
(defn reduce-phases [group]
  (let [phases (group-by #(nth % 1) group)]
    (for [phase phases] 
      [(ffirst (second phase))
       (first phase)
       (find-start (second phase))
       (apply max
        (for [by-src (group-by #(nth % 4) (second phase)) :let [durs (map #(read-string (str (nth % 3))) (second by-src))]]
          (if (= 1 (count by-src))
           (first durs)
           (if false ;;ALWAYS FALSE, used to need additionl check here. 
            0 ;;ignore redundant phase information that has been artifically extended
            (apply + durs)))))])))


;; Returns map with demand group as key and phase list as values
(defn demands->phases [demands demandheader]
  (let [phases (filter identity 
                 (for [demand demands :let [g (fn [atr] (nth demand (get demandheader atr)))]]
                     (when true;(= "SE" (apply str (take 2 (g "DemandGroup")))) ;;does not have to be a scenario
                       [(g "DemandGroup") (g "Operation") (g "StartDay") (g "Duration") (g "SRC")])))
        phases-by-group (group-by first (sort-by first phases))]
    (apply conj (for [p phases-by-group] {(first p) (reduce-phases (second p))}))))


;;; ========== Functions to modify the phase timing information ================
;;Shifts phase by change. Guarantees the following phase is adjusted but does not effect more than 2 phases.
;;Cascade is used to readjust everything by shifting all phases by 0.
(defn shift-phase [phases phaseheader phase change]
  (let [ps (sort-by #(nth % 2) phases)
        phase-index (position-of (map #(nth % (get phaseheader "Phase")) ps) phase)
        si (get phaseheader "Start") di (get phaseheader "Duration")
        s #(read-string (str (nth % si))) d #(read-string (str (nth % di)))]
    (if (= nil phase-index) 
      ps
      (if (= (count ps) (inc phase-index))
        (let [r [(replace-n (nth ps phase-index) di (+ change (d (nth ps phase-index))))]]
             (concat (filter #(not= (nth (first r) 1) (nth % 1)) phases) r))
        (let [A (nth ps phase-index) B (nth ps (inc phase-index))
              Ap (replace-n A di (+ change (d A)))
              Ae (+ (s Ap) (d Ap))
              Bp (replace-n (replace-n B di (- (d B) change)) si Ae)]
          (if (> (+ (s Ap) (d Ap)) (+ (s Bp) (d Bp)))
            (do (->frame "Bad Change Quantity Error, Overlapping Phase" 
                  (str "Shifting " (second (nth ps phase-index)) " right by " change " exceeds the length of the following phase " (second (nth ps (inc phase-index))) 
                    ".\nPlease shift the phase by an amount less than the length of the adjacent phase.\n\n" (nth ps phase-index) "\n" (nth ps (inc phase-index))))
              (throw (Exception. "Bad Change Quantity Error (Phase Overlap)"))))
          (if (neg? (d Ap))
            (do (->frame "Bad Change Quantity Error, Underlapping Phase" 
                  (str "Shifiting " (second (nth ps phase-index)) " left by " change " exceeds the length of the current phase.\n"
                    "Please shift the phase by an amount less than the duration of the current phase.\n\n" (nth ps phase-index)))
              (throw (Exception. "Bad Change Quantity Error (Phase Underlap)"))))
          (concat (list Ap Bp) (filter #(and (not= (nth Ap 1) (nth % 1)) (not= (nth Bp 1) (nth % 1))) ps)))))))


;;Applies any carried over shifts, gaurantees correctness after shift(s)
;;Needed when shift-phase effects more than the initial and next phase. Calls shift-phase by 0 for all phases 
(defn cascade [phases phaseheader & r] 
  (try
    (let [get-p #(nth % (get phaseheader "Phase"))
           p (if (<= 2 (count phases)) (take 2 (sort-by #(nth % (get phaseheader "Start") phases)) (list phases)))
           s #(nth % (get phaseheader "Start"))]
      (if (<= 2 (count p))
        (let [rr (shift-phase phases phaseheader (get-p (first p)) 0)] ;;shift everything by zero, shifting by zero updates any after effects in shift-phase function 
          (cascade (drop 1 rr) phaseheader (concat (first r) (take 1 rr)))) ;;recursivly iterates through all phases in list
        (sort-by s (set (concat p (first r))))))
    (catch Exception e phases))) ;;EOF Error when phase is the last phase when using read string, just resturn input


;;Given seq of changes from change file and change header, applies changes to phases and returns updated phase map
(defn change-phase [phases phaseheader changes changeheader]
  (let [g (fn [a c] (nth c (get changeheader a))) c (first changes)
        ps (get phases (g "DemandGroup" c))]
    (if (>= 0 (count changes)) 
      phases
      (change-phase (assoc phases (g "DemandGroup" c) (cascade (shift-phase ps phaseheader (g "Phase" c) (read-string (g "Change" c))) phaseheader))
          phaseheader (drop 1 changes) changeheader))))


;;; ========== Functions to determine which phase a record should be in ========
;; Returns the start and end (duration + start) from a single record
(defn get-times [demandline demandheader]
  (let [start (nth demandline (get demandheader "StartDay"))
        duration (nth demandline (get demandheader "Duration"))]
    [(read-string (str start)) (read-string (str duration))]))


;; Returns the phase given a time value
(defn get-phase [phases phaseheader time]
  (let [start (fn [phase] (read-string (str (nth phase (get phaseheader "Start")))))
        duration (fn [phase] (read-string (str (nth phase (get phaseheader "Duration")))))]
    (first (filter (fn [phase] (and (<= (start phase) time) (> (+ (start phase) (duration phase)) time))) phases))))


;; Returns all phases that are crossed over the range [start end]
(defn get-phases [phases phaseheader start end]
  (let [p (set (filter identity (for [i (range start end)] (get-phase phases phaseheader i))))]
    (sort-by #(read-string (str (nth % (get phaseheader "Start")))) p))) 


;; Replaces the phase, start, and duration value in the demandline
(defn replace-times [demandline demandheader newphase newstart newduration]
  (replace-n (replace-n (replace-n demandline (get demandheader "StartDay") newstart) (get demandheader "Duration") newduration)
    (get demandheader "Operation") newphase))


;;Breaks demandline on phases ps (returns seq of new demandlines)
(defn split-line [demandline demandheader ps]
  (let [start #(read-string (str (nth % (get demandheader "StartDay"))))
        dur #(read-string (str (nth % (get demandheader "Duration"))))
          nd (- (+ (nth (first ps) 2) (nth (first ps) 3)) (start demandline))
          nl (replace-times demandline demandheader (nth (first ps) 1) (start demandline) nd)
          nls (concat (list nl) (for [p (drop-last (drop 1 ps))] 
                                  (replace-times demandline demandheader (nth p 1) (nth p 2) (nth p 3))))
        dur-last (- (dur demandline) (apply + (map dur nls)))]
    (concat nls (list (replace-times demandline demandheader (nth (last ps) 1) (nth (last ps) 2) dur-last))))) 


;; Breaks up records into multiple records that cross over multiple phases after phase update
;; Will return the original record if does not need to be split
(defn split-by-new-phase [demandline demandheader phases phaseheader]
  (let [ts (get-times demandline demandheader)
        ps (sort-by #(nth % 2) (get-phases phases phaseheader (first ts) (+ (first ts) (last ts))))
        start #(nth % (get demandheader "StartDay")) dur #(nth % (get demandheader "Duration"))]
    (if (< 1 (count ps))
      (split-line demandline demandheader ps)
      (list (replace-n demandline (get demandheader "Operation") (nth (first ps) (get phaseheader "Phase"))))))) ;;update phase even if no split


;; Splits-by-new-phase on all demandlines
(defn reformat-phases [demands demandheader phases phaseheader]
  (let [demands-by-group (partition-by #(nth % (get demandheader "DemandGroup")) (sort-by #(nth % (get demandheader "DemandGroup")) demands))]
    (apply concat
      (for [group demands-by-group :let [p (get phases (nth (first group) (get demandheader "DemandGroup")))]]
        (if true;(= "SE" (apply str (take 2 (nth (first group) (get demandheader "DemandGroup"))))) ;;does not need to be a scenario
          (for [demandline group] 
            (split-by-new-phase demandline demandheader p phaseheader)) 
          (list group))))))


;;; ========== High level functions and GUI ====================================
;;Formatts lines given input files
(defn demands-new-phases [demandfile changefile]
  (let [demandheader (read-header demandfile) changeheader (read-header changefile)
        demands (read-demand-file demandfile) changes (read-phase-changes changefile)
        phases (demands->phases demands demandheader)
        nd (apply concat (reformat-phases demands demandheader (change-phase phases phaseheader changes changeheader) phaseheader))]
    nd))


;;Calls all functions to format and write output given file inputs
(defn ->output-new-phases [demandfile demandheader changefile outfile]
  (let [new-demands (demands-new-phases demandfile changefile)]
    (with-open [w (clojure.java.io/writer outfile)]
      (doseq [line (concat (list (sort-by #(get demandheader %) (keys demandheader))) new-demands)]
        (doseq [val line] (.write w (str val "\t"))) (.write w "\r\n")))))


