{-
    Occam Agent: Agent.hs
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
Num ->>_Lang Aterm
Aterm + Aterm ->>_Lang Aterm
1 + 1 ->>_Aequals 2
1 + 2 ->>_Aequals 3
1 + 3 ->>_Aequals 4
1 + 4 ->>_Aequals 5
1 + 5 ->>_Aequals 6
1 + 6 ->>_Aequals 7
1 + 8 ->>_Aequals 9
1 + 9 ->>_Aequals 1 : 0
2 + 1 ->>_Aequals 3
2 + 2 ->>_Aequals 4
2 + 3 ->>_Aequals 5
2 + 4 ->>_Aequals 6
2 + 5 ->>_Aequals 7
2 + 6 ->>_Aequals 8
2 + 7 ->>_Aequals 9
2 + 8 ->>_Aequals 1 + 9
2 + 9 ->>_Aequals 1 : 1
3 + 1 ->>_Aequals 4
3 + 2 ->>_Aequals 5
3 + 3 ->>_Aequals 6
3 + 4 ->>_Aequals 7
3 + 5 ->>_Aequals 8
3 + 6 ->>_Aequals 9
3 + 7 ->>_Aequals 1 + 9
3 + 8 ->>_Aequals 2 + 9
3 + 9 ->>_Aequals 1 : 2
4 + 1 ->>_Aequals 5
4 + 2 ->>_Aequals 6
4 + 3 ->>_Aequals 7
4 + 4 ->>_Aequals 8
4 + 5 ->>_Aequals 9
4 + 6 ->>_Aequals 1 : 0
4 + 7 ->>_Aequals 1 : 1
4 + 8 ->>_Aequals 1 : 2
4 + 9 ->>_Aequals 1 : 3
5 + 1 ->>_Aequals 6
5 + 2 ->>_Aequals 7
5 + 3 ->>_Aequals 8
5 + 4 ->>_Aequals 9
5 + 5 ->>_Aequals 1 : 0
5 + 6 ->>_Aequals 1 : 1
5 + 7 ->>_Aequals 1 : 2
5 + 8 ->>_Aequals 1 : 3
5 + 9 ->>_Aequals 1 : 4
6 + 1 ->>_Aequals 7
6 + 2 ->>_Aequals 8
6 + 3 ->>_Aequals 9
6 + 4 ->>_Aequals 1 : 0
6 + 5 ->>_Aequals 1 : 1
6 + 6 ->>_Aequals 1 : 2
6 + 7 ->>_Aequals 1 : 3
6 + 8 ->>_Aequals 1 : 4
6 + 9 ->>_Aequals 1 : 5
7 + 1 ->>_Aequals 8
7 + 2 ->>_Aequals 9
7 + 3 ->>_Aequals 1 : 0
7 + 4 ->>_Aequals 1 : 1
7 + 5 ->>_Aequals 1 : 2
7 + 6 ->>_Aequals 1 : 3
7 + 7 ->>_Aequals 1 : 4
7 + 8 ->>_Aequals 1 : 5
7 + 9 ->>_Aequals 1 : 6
8 + 1 ->>_Aequals 9
8 + 2 ->>_Aequals 1 : 0
8 + 3 ->>_Aequals 1 : 1
8 + 4 ->>_Aequals 1 : 2
8 + 5 ->>_Aequals 1 : 3
8 + 6 ->>_Aequals 1 : 4
8 + 7 ->>_Aequals 1 : 5
8 + 8 ->>_Aequals 1 : 6
8 + 9 ->>_Aequals 1 : 7
9 + 1 ->>_Aequals 1 : 0
9 + 2 ->>_Aequals 1 : 1
9 + 3 ->>_Aequals 1 : 2
9 + 4 ->>_Aequals 1 : 3
9 + 5 ->>_Aequals 1 : 4
9 + 6 ->>_Aequals 1 : 5
9 + 7 ->>_Aequals 1 : 6
9 + 8 ->>_Aequals 1 : 7
9 + 9 ->>_Aequals 1 : 8
(Lang,2,3,+)
(Lang,2,8,:)
(Lang,0,6,Aterm)
(Lang,0,12,2)
(Lang,0,11,1)
(Lang,0,3,1 + 2)
(Lang,0,6,1 : 2)
(Aequals,0,58,7)
(Aequals,0,4,1 : 7)
(Aequals,0,2,9 + 8)
(Aequals,0,56,5)
(Aequals,0,8,1 : 5)
(Aequals,0,2,9 + 6)
(Aequals,0,60,3)
(Aequals,0,12,1 : 3)
(Aequals,0,2,9 + 4)
(Aequals,0,20,1 : 1)
(Aequals,0,2,9 + 2)
(Aequals,0,2,8 + 9)
(Aequals,0,2,8 + 7)
(Aequals,0,2,8 + 5)
(Aequals,0,2,8 + 3)
(Aequals,0,2,8 + 1)
(Aequals,0,2,7 + 8)
(Aequals,0,2,7 + 6)
(Aequals,0,2,7 + 4)
(Aequals,0,2,7 + 2)
(Aequals,0,2,6 + 9)
(Aequals,0,2,6 + 7)
(Aequals,0,2,6 + 5)
(Aequals,0,2,6 + 3)
(Aequals,0,2,6 + 1)
(Aequals,0,2,5 + 8)
(Aequals,0,2,5 + 6)
(Aequals,0,2,5 + 3)
(Aequals,0,2,5 + 1)
(Aequals,0,2,4 + 8)
(Aequals,0,2,4 + 6)
(Aequals,0,2,4 + 4)
(Aequals,0,2,4 + 2)
(Aequals,0,2,3 + 9)
(Aequals,0,6,3 + 7)
(Aequals,0,2,3 + 5)
(Aequals,0,2,3 + 3)
(Aequals,0,2,3 + 1)
(Aequals,0,4,2 + 8)
(Aequals,0,2,2 + 7)
(Aequals,0,2,2 + 5)
(Aequals,0,2,2 + 3)
(Aequals,0,2,2 + 1)
(Aequals,0,2,1 + 8)
(Aequals,0,2,1 + 5)
(Aequals,0,2,1 + 3)
(Lang,0,1,2 : (1 : 2))
(Lang,0,1,(1 : 2) : 2)
(Lang,0,1,6)
(Lang,0,1,0)
(Lang,0,1,3)
(Lang,0,1,4)
(Lang,0,1,5)
(Lang,0,1,9)
(Lang,0,2,7)
(Lang,0,2,8)
(Lang,0,12,Dig)
(Lang,0,4,Num)
(Aequals,0,3,1 + 2)
(Aequals,0,2,1 + 4)
(Aequals,0,2,1 + 6)
(Aequals,0,2,1 + 9)
(Aequals,0,2,2 + 2)
(Aequals,0,2,2 + 4)
(Aequals,0,2,2 + 6)
(Aequals,0,2,2 + 9)
(Aequals,0,2,3 + 2)
(Aequals,0,2,3 + 4)
(Aequals,0,2,3 + 6)
(Aequals,0,6,3 + 8)
(Aequals,0,2,4 + 1)
(Aequals,0,2,4 + 3)
(Aequals,0,2,4 + 5)
(Aequals,0,2,4 + 7)
(Aequals,0,2,4 + 9)
(Aequals,0,2,5 + 2)
(Aequals,0,2,5 + 4)
(Aequals,0,4,5 + 5)
(Aequals,0,2,5 + 7)
(Aequals,0,2,5 + 9)
(Aequals,0,2,6 + 2)
(Aequals,0,2,6 + 4)
(Aequals,0,2,6 + 6)
(Aequals,0,2,6 + 8)
(Aequals,0,2,7 + 1)
(Aequals,0,2,7 + 3)
(Aequals,0,2,7 + 5)
(Aequals,0,2,7 + 7)
(Aequals,0,6,7 + 9)
(Aequals,0,2,8 + 2)
(Aequals,0,2,8 + 4)
(Aequals,0,2,8 + 6)
(Aequals,0,2,8 + 8)
(Aequals,0,2,9 + 1)
(Aequals,0,26,1 : 0)
(Aequals,0,26,0)
(Aequals,0,2,9 + 3)
(Aequals,0,14,1 : 2)
(Aequals,0,2,9 + 5)
(Aequals,0,10,1 : 4)
(Aequals,0,54,4)
(Aequals,0,2,9 + 7)
(Aequals,0,10,1 : 6)
(Aequals,0,56,6)
(Aequals,0,2,9 + 9)
(Aequals,0,56,9)
(Aequals,0,141,2)
(Aequals,0,89,1 + 1)
(Aequals,0,2,1 : 8)
(Aequals,0,335,1)
(Aequals,0,56,8)
(Aequals,2,264,+)
(Aequals,2,106,:)
