(ns oecm.init
  (:use [oecm.env]
        [oecm.fig2]
        [oecm.fig3]
        [oecm.fig4]
        [oecm.fig5]
        [oecm.fig6]))

(defn =op [op]
  (=new-<subr> (fn [args env] (apply op args))))

(defn =env-from [env bindings]
  (=env-extend env (map first bindings) (map second bindings)))

(def =default-env
  (=env-from =empty-env
    `(
       (~'+ ~(=op +))
       (~'- ~(=op -))
       (~'* ~(=op *))
       (~'/ ~(=op /))
     )))
