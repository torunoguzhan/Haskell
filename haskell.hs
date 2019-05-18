myInýt ns = take(length ns -1)ns

f a b = b*b+a

a=10+b
  where
  b=20

c=30+a

data Day = M | Tu | W | Th | F | Sa | Su deriving(Show , Eq , Ord , Enum)

--Gelecek Gün Hesabý
nextDay d =  head ( tail [d..Su]++[M])

--Önceki Gün Hesabý
prevDay z = last ([Su] ++ init [M .. z] )

--Derece Yön Tayini
data Direction = North | South | East | West deriving(Show , Eq , Ord , Enum)
derece d = [0,90,180,360]!!(4 - length[d..West] )

--Hipotenüs Hesabý
hipohesapla n = [(x,y,n)  | x<-[0..20],y<-[0..20], x*x+y*y==n*n]



fMSum xss = sum [ x | xs <- xss , x <-xs ] 
fM xss =  [ x| xs <- xss , x <- xs ] 


fafa n = map(\x-> x+2) [1..length(show n)]


func 0 = []

func n = mod n 10 : func (div n 10)



fL = take 20(1:[ 2*x | x<-fL])

fL2 = take 20 [2^(x-1) | x<-[1..]]


any p xs = or [p x | x <- xs ]

map2 = map(+2).map(+3)

deger =reverse.map(+2).init 



takewhile p []=[] 

 
takewhile p (x:xs) | p x = x : takewhile p xs | otherwise  = []

func2 n = takewhile ((<=n)) 


fun square n = n*n;


dist(x,y) = sqrt(x*x + y*y ) 

type Position = (Int,Int)
origin::Position

left :: Position -> Position
left (x,y) = (x-1,y)

leftright :: Position -> Position
leftright (x,y) = (x-1,y-1)

origin = (0,0)


type Pairr a = (a,a)

copy :: a -> Pairr a 
copy x = (x,x)

mult :: Pairr Int->Int
mult (m,n)=m*n

type Yeni = (Int , Int)

katla :: Yeni -> Yeni
katla (x,y) = (3*x , 3*y)



alma = take 20(1 : [2 * x | x<-alma])


list = [tail, (2:), take 2, map (+1)]
 
fCur = head [last] [length] [null] 



data E = A E E | T E E  | D E E  | P E E  | N Int
postfix :: E -> String
postfix( A e1 e2) = postfix e1 ++ " " ++ postfix e2 ++ "+"
postfix (N n) =show n


faf= do
  x<-getChar
  getChar 
  y<-getChar
  return (x,y)


getc :: IO ()
getc = 
    do 
       a<-getChar
       putChar '\t'
       b<-getChar
       putChar '\n'
       putChar a
       putChar b



