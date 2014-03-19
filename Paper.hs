{-
    Occam Agent: Paper.hs
-}
Width ->>_Param 8
Depth ->>_Param 10
Solution ->>_Param 6
0 ->>_Lang Dig
1 ->>_Lang Dig
2 ->>_Lang Dig
Dig ->>_Lang Num
Num : Dig ->>_Lang Num
Num ->>_Lang Math
Math + Math ->>_Lang Math
8 + 3 ->>_Equality 1 : 1
(1 : 1) + 3 ->>_Equality 1 : 4
f 1 ->>_Equality f (0 + 1)
f 2 ->>_Equality f (1 + 1)
f 0 ->>_Equality 8
(Equality,1,34,f)
(Equality,0,4,8)
(Equality,0,12,0)
(Equality,0,10,f 0)
(Equality,0,2,0 + 1)
(Equality,0,2,f (0 + 1))
(Equality,0,2,f 1)
(Equality,0,2,8 + 3)
(Lang,0,8,Num)
(Lang,0,2,2 : (1 : 2))
(Lang,0,4,0)
(Lang,0,2,2 : 0)
(Lang,0,2,(2 : 0) : 1)
(Lang,0,6,Dig)
(Lang,0,6,1 : 2)
(Lang,0,2,1 + 2)
(Lang,0,2,2 + 1)
(Lang,0,16,2)
(Lang,0,16,1)
(Lang,0,6,Math)
(Lang,2,12,:)
(Lang,2,4,+)
(Equality,0,2,(1 : 1) + 3)
(Equality,0,4,1 : 1)
(Equality,0,4,3)
(Equality,0,2,1 : 4)
(Equality,0,2,4)
(Equality,2,6,:)
(Equality,0,2,f 2)
(Equality,0,2,2)
(Equality,0,2,f (1 + 1))
(Equality,0,2,1 + 1)
(Equality,0,4,f Num.x)
(Equality,0,12,f (Num.x + 1))
(Equality,0,12,Num.x + 1)
(Equality,0,16,Num.x)
(Equality,0,30,1)
(Equality,2,20,+)
