(ns oecm.init
  (:use [oecm.env]
        [oecm.fig2]
        [oecm.fig3]
        [oecm.fig4]
        [oecm.fig5]
        [oecm.fig6]))

(defn =op [op]
  (fn [args env] (apply op args)))

(defn =env-from [env bindings]
  (=env-extend env (map first bindings) (map second bindings)))

(def =default-env
  (=env-from =empty-env
    `(
       (~'+ ~(=op +))
       (~'- ~(=op -))
       (~'* ~(=op *))
       (~'/ ~(=op /))

       (~'let ~(=new-<fixed>
                 (=new-<form>
                   (fn [[bindings body] env]
                     (cond
                       (empty? bindings)
                       (=eval body env)
                       (empty? (rest bindings))
                       (=error "let requires an even number of forms in binding vector")
                       :else
                       (recur
                         [(rest (rest bindings)) body]
                         (=env-extend env [(first bindings)] [(second bindings)])))))))

       (~'fn ~(=new-<fixed>
                (=new-<form>
                  (fn [[formals body] env]
                    (=new-<expr> formals body env)))))
     )))
