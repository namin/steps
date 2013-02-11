(ns oecm.fig3
  (:use [oecm.fig2]))

(declare =eval =apply)

(def *evaluators* (=tuple))
(def *applicators* (=tuple))

;; Built-in aggregate types
(=define-type <subr> (implementation))
(defn =subr? [exp]
  (= <subr> (=type-of exp)))
(=define-type <expr> (formals body environment))
(=define-type <fixed> (function))
(defn =fixed [function]
  (let [self (=new <fixed>)]
    (set-<fixed>-function self function)
    self))

;; Figure 3: Generalized assignment of meaning to expressions
(defn =eval [exp env]
  (=apply (=tuple-at *evaluators* (=type-of exp)) (list exp env) env))

(defn =apply [fun args env]
  (if (=subr? fun)
    ((<subr>-implementation fun) args env)
    (=apply (=tuple-at *applicators* (=type-of fun)) (list fun args env) env)))
