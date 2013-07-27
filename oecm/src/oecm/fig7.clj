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

(defn =set-default-<generic>-method [mm method]
  (=set-tuple-at mm 'default method))

(defn =get-default-<generic>-method [mm]
  (=tuple-at mm 'default))

(defn %add-multimethod [mm types method]
  (letfn [(add! [methods types]
            (if (empty? types)
              (=set-default-<generic>-method methods method)
              (recur (do
                       (when (not (=tuple-at methods (first types)))
                         (=set-tuple-at methods (first types) (=tuple)))
                       (=tuple-at methods (first types)))
                     (rest types))))]
    (add! (<generic>-methods mm)
          types)))

(=set-tuple-at =*applicators* <generic>
  (fn [[self arguments] env]
    (letfn [(to-method [method spec-default args]
              (let [next-spec-default (or (=get-default-<generic>-method method) spec-default)]
                (if (empty? args)
                  next-spec-default
                  (let [next-method (=tuple-at method (=type-of (first args)))]
                    (if (not next-method)
                      next-spec-default
                      (recur next-method next-spec-default (rest args)))))))]
      (=apply (to-method (<generic>-methods self) false arguments) arguments env))))

