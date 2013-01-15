(ns oecm.env)

;; Environments
(defn =env-get [env k]
  (get env k))
(defn =env-extend [env vs args]
  (cond
    (empty? vs)
    env

    (empty? args)
    env

    :else
    (recur (assoc env (first vs) (first args)) (rest vs) (rest args))))
