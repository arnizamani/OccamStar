{-
    Occam Agent: Arithmetic.hs
-}
Width ->>_Param 8
Depth ->>_Param 10
Solution ->>_Param 0
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
Aterm * Aterm ->>_Lang Aterm
Aterm.x * 0 ->>_Equal 0
0 * Aterm.x ->>_Equal 0
Aterm.x + 0 ->>_Equal Aterm.x
0 + Aterm.x ->>_Equal Aterm.x
Aterm.x * 1 ->>_Equal Aterm.x
1 * Aterm.x ->>_Equal Aterm.x
Aterm.x * Aterm.y ->>_Equal Aterm.y * Aterm.x
Aterm.x + Aterm.y ->>_Equal Aterm.y + Aterm.x
(Aterm.x + Aterm.y) + Aterm.z ->>_Equal Aterm.x + (Aterm.y + Aterm.z)
Aterm.x + (Aterm.y + Aterm.z) ->>_Equal (Aterm.x + Aterm.y) + Aterm.z
Aterm.x * (Aterm.y + Aterm.z) ->>_Equal (Aterm.x * Aterm.y) + (Aterm.x * Aterm.z)
(Num.x : Dig.y) + Num.z ->>_Equal Num.x : (Dig.y + Num.z)
Num.x : (Num.y : Num.z) ->>_Equal (Num.x + Num.y) : Num.z
(Num.x : Dig.y) * Num.z ->>_Equal (Num.x * Num.z) : (Dig.y * Num.z)
(Num.x : 0) * Num.z ->>_Equal (Num.x * Num.z) : 0
1 + 1 ->>_Equal 2
1 + 2 ->>_Equal 3
1 + 3 ->>_Equal 4
1 + 4 ->>_Equal 5
1 + 5 ->>_Equal 6
1 + 6 ->>_Equal 7
1 + 7 ->>_Equal 8
1 + 8 ->>_Equal 9
1 + 9 ->>_Equal 1 : 0
2 + 1 ->>_Equal 3
2 + 2 ->>_Equal 4
2 + 3 ->>_Equal 5
2 + 4 ->>_Equal 6
2 + 5 ->>_Equal 7
2 + 6 ->>_Equal 8
2 + 7 ->>_Equal 9
2 + 8 ->>_Equal 1 : 0
2 + 9 ->>_Equal 1 : 1
3 + 1 ->>_Equal 4
3 + 2 ->>_Equal 5
3 + 3 ->>_Equal 6
3 + 4 ->>_Equal 7
3 + 5 ->>_Equal 8
3 + 6 ->>_Equal 9
3 + 7 ->>_Equal 1 : 0
3 + 8 ->>_Equal 1 : 1
3 + 9 ->>_Equal 1 : 2
4 + 1 ->>_Equal 5
4 + 2 ->>_Equal 6
4 + 3 ->>_Equal 7
4 + 4 ->>_Equal 8
4 + 5 ->>_Equal 9
4 + 6 ->>_Equal 1 : 0
4 + 7 ->>_Equal 1 : 1
4 + 8 ->>_Equal 1 : 2
4 + 9 ->>_Equal 1 : 3
5 + 1 ->>_Equal 6
5 + 2 ->>_Equal 7
5 + 3 ->>_Equal 8
5 + 4 ->>_Equal 9
5 + 5 ->>_Equal 1 : 0
5 + 6 ->>_Equal 1 : 1
5 + 7 ->>_Equal 1 : 2
5 + 8 ->>_Equal 1 : 3
5 + 9 ->>_Equal 1 : 4
6 + 1 ->>_Equal 7
6 + 2 ->>_Equal 8
6 + 3 ->>_Equal 9
6 + 4 ->>_Equal 1 : 0
6 + 5 ->>_Equal 1 : 1
6 + 6 ->>_Equal 1 : 2
6 + 7 ->>_Equal 1 : 3
6 + 8 ->>_Equal 1 : 4
6 + 9 ->>_Equal 1 : 5
7 + 1 ->>_Equal 8
7 + 2 ->>_Equal 9
7 + 3 ->>_Equal 1 : 0
7 + 4 ->>_Equal 1 : 1
7 + 5 ->>_Equal 1 : 2
7 + 6 ->>_Equal 1 : 3
7 + 7 ->>_Equal 1 : 4
7 + 8 ->>_Equal 1 : 5
7 + 9 ->>_Equal 1 : 6
8 + 1 ->>_Equal 9
8 + 2 ->>_Equal 1 : 0
8 + 3 ->>_Equal 1 : 1
8 + 4 ->>_Equal 1 : 2
8 + 5 ->>_Equal 1 : 3
8 + 6 ->>_Equal 1 : 4
8 + 7 ->>_Equal 1 : 5
8 + 8 ->>_Equal 1 : 6
8 + 9 ->>_Equal 1 : 7
9 + 1 ->>_Equal 1 : 0
9 + 2 ->>_Equal 1 : 1
9 + 3 ->>_Equal 1 : 2
9 + 4 ->>_Equal 1 : 3
9 + 5 ->>_Equal 1 : 4
9 + 6 ->>_Equal 1 : 5
9 + 7 ->>_Equal 1 : 6
9 + 8 ->>_Equal 1 : 7
9 + 9 ->>_Equal 1 : 8
2 * 2 ->>_Equal 4
2 * 3 ->>_Equal 6
2 * 4 ->>_Equal 8
2 * 5 ->>_Equal 1 : 0
2 * 6 ->>_Equal 1 : 2
2 * 7 ->>_Equal 1 : 4
2 * 8 ->>_Equal 1 : 6
2 * 9 ->>_Equal 1 : 8
3 * 2 ->>_Equal 6
3 * 3 ->>_Equal 9
3 * 4 ->>_Equal 1 : 2
3 * 5 ->>_Equal 1 : 5
3 * 6 ->>_Equal 1 : 8
3 * 7 ->>_Equal 2 : 1
3 * 8 ->>_Equal 2 : 4
3 * 9 ->>_Equal 2 : 7
4 * 2 ->>_Equal 8
4 * 3 ->>_Equal 1 : 2
4 * 4 ->>_Equal 1 : 6
4 * 5 ->>_Equal 2 : 0
4 * 6 ->>_Equal 2 : 4
4 * 7 ->>_Equal 2 : 8
4 * 8 ->>_Equal 3 : 2
4 * 9 ->>_Equal 3 : 6
5 * 2 ->>_Equal 1 : 0
5 * 3 ->>_Equal 1 : 5
5 * 4 ->>_Equal 2 : 0
5 * 5 ->>_Equal 2 : 5
5 * 6 ->>_Equal 3 : 0
5 * 7 ->>_Equal 3 : 5
5 * 8 ->>_Equal 4 : 0
5 * 9 ->>_Equal 4 : 5
6 * 2 ->>_Equal 1 : 2
6 * 3 ->>_Equal 1 : 8
6 * 4 ->>_Equal 2 : 4
6 * 5 ->>_Equal 3 : 0
6 * 6 ->>_Equal 3 : 6
6 * 7 ->>_Equal 4 : 2
6 * 8 ->>_Equal 4 : 8
6 * 9 ->>_Equal 5 : 4
7 * 2 ->>_Equal 1 : 4
7 * 3 ->>_Equal 2 : 1
7 * 4 ->>_Equal 2 : 8
7 * 5 ->>_Equal 3 : 5
7 * 6 ->>_Equal 4 : 2
7 * 7 ->>_Equal 4 : 9
7 * 8 ->>_Equal 5 : 6
7 * 9 ->>_Equal 6 : 3
8 * 2 ->>_Equal 1 : 6
8 * 3 ->>_Equal 2 : 4
8 * 4 ->>_Equal 3 : 2
8 * 5 ->>_Equal 4 : 0
8 * 6 ->>_Equal 4 : 8
8 * 7 ->>_Equal 5 : 6
8 * 8 ->>_Equal 6 : 4
8 * 9 ->>_Equal 7 : 2
9 * 2 ->>_Equal 1 : 8
9 * 3 ->>_Equal 2 : 7
9 * 4 ->>_Equal 3 : 6
9 * 5 ->>_Equal 4 : 5
9 * 6 ->>_Equal 5 : 4
9 * 7 ->>_Equal 6 : 3
9 * 8 ->>_Equal 7 : 2
9 * 9 ->>_Equal 8 : 1
Num.x ->>_Equal Num
(Equal,2,38,+)
(Equal,0,2,Num)
(Equal,0,18,4)
(Equal,0,52,2)
(Equal,0,10,2 + 4)
(Equal,0,2,Dig.x)
(Equal,0,18,6)
(Equal,0,36,1)
(Equal,0,2,1 : 3)
(Equal,0,2,(1 : 3) : 4)
(Equal,0,2,(6 : 7) * 2)
(Equal,0,6,67)
(Equal,0,6,67 * 8)
(Equal,0,6,4 : 1)
(Equal,0,6,1 : 8)
(Equal,0,14,2 : 3)
(Equal,0,6,(2 : 3) + (1 : 8))
(Equal,0,4,3 : 1)
(Equal,0,4,(2 : 3) + 8)
(Equal,0,2,2 : 8)
(Equal,0,2,(2 : 3) + 5)
(Equal,0,4,2 * 0)
(Lang,2,4,+)
(Lang,0,4,Aterm)
(Lang,0,4,3)
(Lang,0,4,2)
(Lang,0,4,2 + 3)
(Equal,0,14,2 + 8)
(Equal,0,14,1 : 0)
(Equal,0,2,(2 : 3) + (1 : 6))
(Equal,0,2,1 : 6)
(Equal,0,2,3 : 9)
(Equal,0,2,9 * 8)
(Equal,0,4,9)
(Equal,0,2,7 : 2)
(Equal,0,2,(6 : 0) * 2)
(Equal,0,2,6 : 0)
(Equal,0,2,(1 : 2) : 0)
(Equal,0,2,1 : 2)
(Equal,0,26,0)
(Equal,0,2,(6 : 7) * 8)
(Equal,0,4,6 : 7)
(Equal,0,6,7)
(Equal,0,36,8)
(Equal,0,8,(5 : 3) : 6)
(Equal,0,8,5 : 3)
(Equal,0,10,5)
(Equal,0,30,3)
(Equal,2,18,*)
(Equal,2,82,:)
(Equal,0,2,Dig)
(Equal,0,2,Num.x)
