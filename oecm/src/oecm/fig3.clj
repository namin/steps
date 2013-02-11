(ns oecm.fig3
  (:use [oecm.fig2]))

(declare =eval =apply)

(def =*evaluators* (=tuple))
(def =*applicators* (=tuple))

;; Built-in aggregate types
(defn <subr>-implementation [f] f)
(defn subr? [exp]
  (fn? exp))
(=define-type <expr> (formals body environment))
(=define-type <fixed> (function))

;; Figure 3: Generalized assignment of meaning to expressions
(defn =eval [exp env]
  (=apply (=tuple-at =*evaluators* (=type-of exp)) (list exp) env))

(defn =apply [fun args env]
  (if (subr? fun)
    ((<subr>-implementation fun) args env)
    (=apply (=tuple-at =*applicators* (=type-of fun)) (list fun args env) env)))
