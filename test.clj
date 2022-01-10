(fun make-name [name]
  (js-string-concat "hello, " name "!" ))

(export make_name
  (lam [name] (make-name name)))