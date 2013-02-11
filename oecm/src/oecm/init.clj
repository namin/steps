(ns oecm.init
  (:use [oecm.env]
        [oecm.fig2]
        [oecm.fig3]
        [oecm.fig4]
        [oecm.fig5]
        [oecm.fig6]))

(defn =binop [op]
  (=new-<subr> (fn [[a b] env] (op a b))))

(defn =env-from [env bindings]
  (=env-extend env (map first bindings) (map second bindings)))

(def =default-env
  (=env-from =empty-env
    `((~'+ ~(=binop +)))))
