(ns oecm.fig7
  (:use [oecm.env]
        [oecm.fig2]
        [oecm.fig3]
        [oecm.fig4]
        [oecm.fig5]))

;; Figure 7: Generic function type and its applicative meaning function

(=define-type <generic> (name methods))

(defn =make-generic [name]
  (=new-<generic> name (=tuple)))

(defn %add-multimethod [mm types method]
  (if (empty? types)
    (set-<generic>-methods mm method)
    (letfn [(add! [methods types]
              (if (empty? (rest types))
                (=set-tuple-at methods (first types) method)
                (recur (do
                         (when (not (=tuple-at methods (first types)))
                           (=set-tuple-at methods (first types) (=tuple)))
                         (=tuple-at methods (first types)))
                       (rest types))))]
      (add! (do
              (when (not (<generic>-methods mm))
                (set-<generic>-methods mm (=tuple)))
              (<generic>-methods mm))
            types))))

(=set-tuple-at =*applicators* <generic>
  (fn [[self arguments] env]
    (letfn [(to-method [method args]
              (if (or (empty? args) (not (=raw-tuple? method)))
                method
                (recur (=tuple-at method (=type-of (first args)))
                       (rest args))))]
      (=apply (to-method (<generic>-methods self) arguments) arguments env))))

