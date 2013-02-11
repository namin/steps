(ns oecm.fig2)

;; Tuples
(defn =tuple []
  (atom (hash-map)))
(defn =set-tuple-at [t k v]
  (swap! t assoc k v))
(defn =tuple-at [t k]
  (get @t k))
(defn =reset-tuple [t & vs]
  (dotimes [i (count vs)]
    (=set-tuple-at t i (nth vs i))))
;; Built-in non-aggregate types
(def <symbol> 0)
(def <number> 1)
(def <list> 2)
(def <subr> 3)
(def number-of-built-in-non-aggregate-types 4)

;; Creation of typed objects
(defn =new [type]
  (let [t (=tuple)]
    (=set-tuple-at t 'type type)
    t))
(defn =type-of [value]
  (cond
    (symbol? value) <symbol>
    (number? value) <number>
    (list? value) <list>
    (fn? value) <subr>
    :else (=tuple-at value 'type)))

;; Figure 2: Adding aggregate types
(def %type-names (=tuple))
(def %type-sizes (=tuple))
(def %type-fields (=tuple))

(def %allocate-type
  (let [last-type (atom number-of-built-in-non-aggregate-types)]
    (fn [name fields]
      (let [type (swap! last-type inc')]
        (=set-tuple-at %type-names type name)
        (=set-tuple-at %type-sizes type (count fields))
        (=set-tuple-at %type-fields type fields)
        type))))

;; Convenient macro for defining an aggregate type
(defmacro =define-type [name fields]
  `(do
     (def ~name (%allocate-type '~name '~fields))
     ~@(map (fn [i field]
              `(do
                 (defn ~(symbol (str `~name '- `~field)) [value#]
                   (and (= ~name (=type-of value#))
                     (=tuple-at value# ~i)))
                 (defn ~(symbol (str 'set- `~name '- `~field)) [value# field-value#]
                   (and (= ~name (=type-of value#))
                     (=set-tuple-at value# ~i field-value#)))))
         (range 0 (count fields))
         fields)
     (defn ~(symbol (str '=new- `~name)) [~@fields]
       (let [self# (=new ~name)]
         (=reset-tuple self# ~@fields)
         self#))
     '~name))
