(ns oecm.fig5
  (:use [oecm.env]
        [oecm.fig2]
        [oecm.fig3]
        [oecm.fig4]))

;; Figure 5: Form type for defining macros

(=define-type <form> (function))

(=set-tuple-at =*applicators* <form>
  (fn [fun args env]
    (=eval (=apply (<form>-function fun) args env) env)))
