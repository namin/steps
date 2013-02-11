(ns oecm.tests
  (:use [oecm.env]
        [oecm.fig2]
        [oecm.fig3]
        [oecm.fig4]
        [oecm.fig5]
        [oecm.fig6]
        [oecm.init]
    :reload)
  (:use clojure.test))

(deftest test-basic-eval-1
  (is (= (=eval 5 =empty-env) 5))
  (is (= (=eval 'x (=env-extend =empty-env ['x] [5])) 5)))

(deftest test-env-eval-2
  (is (= (=eval '(+ 1 2) =default-env) 3)))

