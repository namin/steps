(ns oecm.fig6
  (:use [oecm.env]
        [oecm.fig2]
        [oecm.fig3]
        [oecm.fig4]
        [oecm.fig5]))

;; Figure 6: Selector type and its applicative meaning function

(=define-type <selector> (name methods))

(defn =make-selector [name]
  (=new-<selector> name (=tuple)))

(defmacro =define-selector [name]
  `(def ~name (make-selector '~name)))

(defn %add-method [self type method]
  (=set-tuple-at (<selector>-methods self) type method))

(defmacro =define-method [selector type args & body]
  `(%add-method ~selector ~type (fn [self ~@args] ~@body)))

(defn =error [& args]
  (throw (Error. (apply str args))))

(=set-tuple-at =*applicators* <selector>
  (fn [self & arguments]
    (=apply
      (or
        (=tuple-at (<selector>-methods self) (=type-of (first arguments)))
        (=error "no method in " (<selector>-name self) " for " (=type-of (first arguments))))
      arguments)))
