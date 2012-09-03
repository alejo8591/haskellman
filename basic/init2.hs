yopoYepe xs = [if x < 10 then "yopo" else "yepe" | x <- xs, odd x]
length' xt = sum[1|_ <- xt ]
removeLower st = [c | c <- st, c `elem` ['a'..'z']] 
