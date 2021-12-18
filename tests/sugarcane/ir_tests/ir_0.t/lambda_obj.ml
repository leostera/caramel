let obj0 () = object end

let obj1 () = object
  val hello = 1
end

let obj2 () = object
  val hello = 1
  method get_hello () = hello
end
