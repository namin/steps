(ns oecm.init
  (:use [oecm.env]
        [oecm.fig2]
        [oecm.fig3]
        [oecm.fig4]
        [oecm.fig5]
        [oecm.fig6]
        [oecm.fig7]))

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

       (~'let_ ~(=new-<fixed>
                  (fn [[bindings body] env]
                   (cond
                     (empty? bindings)
                     (=eval body env)
                     (empty? (rest bindings))
                     (=error "let requires an even number of forms in binding vector")
                     :else
                     (recur
                       [(rest (rest bindings)) body]
                       (=env-extend env [(first bindings)] [(second bindings)]))))))
       (~'let ~(=new-<fixed> (=new-<form> (fn [[bindings & body] env]
                                            `(~'let_ ~bindings (~'do ~@body))))))

       (~'fn_ ~(=new-<fixed>
                 (fn [[formals body] env]
                   (=new-<expr> formals body env))))
       (~'fn ~(=new-<fixed> (=new-<form> (fn [[formals & body] env]
                                           `(~'fn_ ~formals (~'do ~@body))))))

       (~'do ~(=new-<fixed>
                (fn [[first & rest] env]
                  (if (empty? rest)
                    (=eval first env)
                    (do
                      (=eval first env)
                      (recur rest env))))))

       (~'quote ~(=new-<fixed>
                  (fn [[datum] env]
                    datum)))

       (~'<number> ~<number>)
       (~'<symbol> ~<symbol>)

       (~'with-selector ~(=new-<fixed>
                          (fn [[name & body] env]
                            (=eval `(~'do ~@body) (=env-extend env [name] [(=make-selector name)])))))

       (~'define-method ~(=new-<fixed>
                          (fn [[selector type args & body] env]
                            (%add-method (=eval selector env)
                                         (=eval type env)
                                         (=eval `(~'fn ~args ~@body) env)))))
       (~'with-generic ~(=new-<fixed>
                          (fn [[name & body] env]
                            (=eval `(~'do ~@body) (=env-extend env [name] [(=make-generic name)])))))

       (~'define-multimethod ~(=new-<fixed>
                               (fn [[mm [types args] & body] env]
                                 (%add-multimethod (=eval mm env)
                                                   (map #(=eval % env) types)
                                                   (=eval `(~'fn ~args ~@body) env)))))

     )))
