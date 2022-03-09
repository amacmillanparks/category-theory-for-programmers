let id a = a

let compose g f a = g (f a)
let and_then f g a = g (f a)

let () = assert (1 == compose id id 1)
