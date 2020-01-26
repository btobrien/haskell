

prod [] = 1
prod (n:ns) = n * prod ns

main = print (prod [1..3])
