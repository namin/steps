(ns oecm.fig4
  (:use [oecm.env]
        [oecm.fig2]
        [oecm.fig3]))

;; Figure 4: The original language semantics expressed as composition rules

(=set-tuple-at =*evaluators* <symbol> (fn [exp env] (=env-get env exp)))

(=set-tuple-at =*evaluators* <number> (fn [exp env] exp))

(defn =evlis [es env]
  (map #(=eval % env) es))
(=set-tuple-at =*evaluators* <list>
  (fn [exp env]
    (let [fun (=eval (first exp) env)]
      (if (= (=type-of fun) <fixed>)
        (=apply (<fixed>-function fun) (rest exp) env)
        (=apply fun (=evlis (rest exp) env) env)))))

(=set-tuple-at =*applicators* <expr>
  (fn [[fun args] env]
    (=eval (<expr>-body fun) (=env-extend (<expr>-environment fun) (<expr>-formals fun) args))))

(=set-tuple-at =*evaluators* <expr> (fn [exp env] exp))

