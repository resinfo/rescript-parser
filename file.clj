;; Literals
hello
1
"1"
false
true
[1 2 3]
( :foo "bar"
  :baz 12 )
(lam [_] 12)
(add 1 1)
{
  (let x (add 1 1))
  (add x x)
}

(let hello (lam [a b] 12))
(fun hello [a b] 12)

(fun fib n {
  (fun _fib left right n 
    (if (= n 0)
      0
      (if (= n 1)
        right
        (_fib right (add left right) (- n 1)))))

  (_fib 0 1 n)
})

(fun fib n {
  (fun _fib left right n 
    (if
      (= n 0)
      0
      (if
        (= n 1)
        right
        (_fib
          right
          (add left right)
          (- n 1)
        )
      )
    )
  )

  _fib(0, 1, n)
})



types

<my-var x :: (Some x) None>
<thing :: string>
<thing :: >

(switch x 
  :: None "hello"
  :: (Some "hello") "hello"
  :: (Some arg) arg)

(module name {
  <world :: string>
  (let world "hello")

  <:: world>
  (export hello world)
  <:: string>
  (export world "hello")
})