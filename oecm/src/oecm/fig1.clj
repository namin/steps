(ns oecm.fig1
  (:use oecm.env))

(declare =eval =apply)

;; Structures
(defn tagged-list? [tag exp]
  (and (seq? exp) (= (first exp) tag)))

(defn =fixed? [exp]
  (tagged-list? '<fixed> exp))
(defn <fixed>-function [exp]
  (second exp))

(defn =subr? [exp]
  (tagged-list? '<subr> exp))
(defn <subr>-implementation [exp]
  (second exp))

(defn =expr? [exp]
  (tagged-list? '<expr> exp))
(defn <expr>-formals [exp]
  (second exp))
(defn <expr>-body [exp]
  (second (rest exp)))
(defn <expr>-environment [exp]
  (second (rest (rest exp))))

(defn =atom? [exp]
  false)

;; Helpers
(defn =evlis [es env]
  (map #(=eval % env) es))

;; Figure 1: Evaluation of symbolic expression
(defn =eval [exp env]
  (cond
    [symbol? exp]
    (=env-get env exp)

    [=atom? exp]
    exp

    :else
    (let [f (=eval (first exp) env)]
      (if (=fixed? f)
        (=apply (<fixed>-function f) (rest exp) env)
        (=apply f (=evlis (rest exp) env) env)))))

(defn =apply [fun args env]
  (cond
    [=subr? fun]
    ((<subr>-implementation fun) args env)

    [=expr? fun]
    (=eval (<expr>-body fun) (=env-extend (<expr>-environment env) (<expr>-formals fun) args))))
