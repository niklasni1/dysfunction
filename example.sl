let y = x. {(if (x = 4) then 5 else let x = (print x; x + 1) in call y x)} in
 call y 0
