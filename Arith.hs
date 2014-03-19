{-
    Occam Agent: Arith.hs
-}
Width ->>_Param 8
Depth ->>_Param 10
Solution ->>_Param 6
0 ->>_Lang Dig
1 ->>_Lang Dig
2 ->>_Lang Dig
3 ->>_Lang Dig
4 ->>_Lang Dig
5 ->>_Lang Dig
6 ->>_Lang Dig
7 ->>_Lang Dig
8 ->>_Lang Dig
9 ->>_Lang Dig
Dig ->>_Lang Num
Num : Dig ->>_Lang Num
Num ->>_Lang Math
Math + Math ->>_Lang Math
Math.z + 0 ->>_Equality Math.z
0 + Math.z ->>_Equality Math.z
Math.z + Math.y ->>_Equality Math.y + Math.z
5 + 5 ->>_Equality 1 : 0
5 + 7 ->>_Equality 1 : 2
(Equality,0,14,0)
(Equality,0,2,1 : 0)
(Equality,0,2,5 + 5)
(Equality,0,4,0 + (1 + 2))
(Equality,0,4,0 + 2)
(Equality,0,2,(1 + 2) + 0)
(Equality,0,2,2 + 0)
(Lang,2,14,:)
(Lang,0,4,4)
(Lang,0,4,3)
(Lang,0,2,3 : 4)
(Lang,0,6,1 : 2)
(Lang,0,8,Num)
(Lang,0,2,2 : (1 : 2))
(Lang,0,2,2 : 0)
(Lang,0,2,(2 : 0) : 1)
(Lang,0,2,7)
(Lang,0,2,6)
(Lang,0,2,5)
(Lang,0,2,8)
(Lang,0,2,9)
(Lang,0,20,Dig)
(Lang,0,2,1 + 2)
(Lang,0,2,2 + 1)
(Lang,0,18,1)
(Lang,0,2,2 + 0)
(Lang,0,18,2)
(Lang,0,6,0)
(Lang,0,12,Math)
(Lang,2,6,+)
(Equality,0,4,2 + 1)
(Equality,0,22,1 + 2)
(Equality,0,4,2 + 3)
(Equality,0,4,3 + 2)
(Equality,0,8,3)
(Equality,0,2,5 + 7)
(Equality,0,6,5)
(Equality,0,2,7)
(Equality,0,2,1 : 2)
(Equality,0,32,1)
(Equality,0,52,2)
(Equality,2,50,+)
(Equality,2,4,:)
