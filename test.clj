(let hello 12)

(fun yes [a] a)

(let add (lam [] (jsadd 1)))

(module mymod {
  (fun x [a] a)
  (export x x)
  
  (module somemod {
    (export log (lam [x] (log x)))
  })
  (export somemod somemod)
})

(export yes yes)
(export hello hello)