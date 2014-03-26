{-
    Occam Agent: Arith.hs
-}
Width ->>_Param 8
Depth ->>_Param 10
Solution ->>_Param 4
0 ->>_Lang Dig
1 ->>_Lang Dig
2 ->>_Lang Dig
Dig ->>_Lang Num
Num : Dig ->>_Lang Num
Num ->>_Lang Math
Math + Math ->>_Lang Math
Math * Math ->>_Lang Math
Math.z + 0 ->>_Equality Math.z
Math.y + Math.z ->>_Equality Math.z + Math.y
8 + 3 ->>_Equality 1 : 1
(1 : 1) + 3 ->>_Equality 1 : 4
f 1 ->>_Equality f (0 + 1)
f 2 ->>_Equality f (1 + 1)
(Equality,2,8,+)
(Equality,1,2,f)
(Equality,0,4,1 + 1)
(Equality,0,2,f (1 + 1))
(Equality,0,2,2)
(Equality,0,2,f 2)
(Equality,0,6,0)
(Equality,0,4,0 + 1)
(Equality,0,2,f (0 + 1))
(Equality,0,8,1)
(Equality,0,2,f 1)
(Equality,2,2,:)
(Equality,0,2,4)
(Equality,0,2,1 : 4)
(Equality,0,2,(1 : 1) + 3)
(Equality,0,2,1 : 1)
(Equality,0,2,3)
(Equality,0,2,8)
(Equality,0,2,8 + 3)
(Equality,0,4,1 + 0)
(Lang,2,2,*)
(Lang,0,4,Math)
(Lang,0,8,2)
(Lang,0,8,1)
(Lang,0,2,1 * 2)
(Lang,2,2,+)
(Lang,0,2,1 + 2)
(Lang,2,2,:)
(Lang,0,2,2 : 1)
(Lang,0,2,1 : (2 : 1))
(Lang,0,2,1 : 2)
(Lang,0,2,Num)
(Lang,0,2,Dig)
(Lang,0,2,0)
