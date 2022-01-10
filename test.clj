(let hello-world 12)

(fun yes [a] a)

(let add (lam [] (jsadd 1 2 3)))

(let hello {
  (let world "world")

  world
})

(fun map [arr]
  (js-arr-map
    arr
    (lam [i]
      (add i 12))))

(fun add2 [] {
  (let x 12)

  x
})

(let add ((lam [a b] (jsadd a b)) 1 2))
(let my_obj ( :hello "world" :world 1 :my-key "sth" ))

(module mymod {
  (fun x [a] a)
  (export x x)
})

(export yes yes)
(export hello hello)