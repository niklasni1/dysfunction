let y = x. (if (x = 4) then 0 else let x = (print x; x + 1) in y(x)) in
 y(0)
