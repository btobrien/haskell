
walls (width,height) above = if height == 0 then 1 else
    sum $ layers width above >>=
    return . walls (width,height-1) . scanl1 (+)

layers width above = 
    if width == 0 then [[]] 
    else if width < 0 || any (==0) above then []
    else [2,3] >>= \brick ->
    map (brick:) (layers (width - brick) (map (subtract brick) above))
