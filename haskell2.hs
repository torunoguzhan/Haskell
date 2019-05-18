myLast ns = head(drop(length ns-1) ns )


rev       :: [a] -> [a] 
rev []     = [] 
rev (x:xs) = rev xs ++ [x]


zips              :: [a] -> [b] -> [(a,b)]
zips []     _      = [] 
zips _      []     = [] 
zips (x:xs) (y:ys) = (x,y) : zip xs ys 




drops             :: Int -> [a] -> [a]
drops 0     xs     = xs 
drops (n+1) []     = [] 
drops (n+1) (_:xs) = drops n xs 


qsorts       :: [Int] -> [Int]
qsorts []     = [] 
qsorts (x:xs) = 

   qsorts smaller ++ [x] ++ qsorts larger    
   where       
               smaller = [a | a <- xs, a <= x]       
               larger  = [b | b <- xs, b > x] 


f = [x | x<-[length f]]


ax=[5]