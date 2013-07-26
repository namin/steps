(ns oecm.tests
  (:use [oecm.env]
        [oecm.fig2]
        [oecm.fig3]
        [oecm.fig4]
        [oecm.fig5]
        [oecm.fig6]
        [oecm.init]
    :reload)
  (:use clojure.test)
  (:use clojure.tools.trace)
)

;(trace-vars oecm.fig3/=eval oecm.fig3/=apply)

(deftest test-eval-1
  (is (= (=eval 5 =empty-env) 5))
  (is (= (=eval 'x (=env-extend =empty-env ['x] [5])) 5)))

(deftest test-app-1
  (is (= (=eval '(+ 1 2) =default-env) 3))
  (is (= (=eval '(- 1) =default-env)) -1)
  (is (= (=eval '(* 1 2 3 4) =default-env) 24)))

(deftest test-fixed-1
  (is (= (=eval '(let [foo 1] foo) =default-env) 1))
  (is (= (=eval '(let [foo 1 bar 2] (+ foo bar)) =default-env) 3))
  (is (= (=eval '((fn [x] x) 0) =default-env) 0))
  (is (= (=eval '(do 1 2) =default-env) 2))
  (is (= (=eval ''x =default-env) 'x)))

(deftest test-form-1
  (is (= (=eval '(let [foo 1] foo 2) =default-env) 2)))

(deftest test-selector-1
  (is  (= (=eval '(with-selector numberof
                    (define-method numberof <number> [x] x)
                    (numberof 1))
                 =default-env)
          1))
    (is  (= (=eval '(with-selector numberof
                      (define-method numberof <number> [x] x)
                      (define-method numberof <symbol> [x] 0)
                      (+ (numberof 1) (numberof 'foo)))
                 =default-env)
            1))
    (is  (= (=eval '(with-selector ignof
                      (define-method ignof <number> [x y] x)
                      (define-method ignof <symbol> [x y] y)
                      (+ (ignof 1 2) (ignof 'foo 3)))
                 =default-env)
          4)))
