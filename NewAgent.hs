{-
    Author: Abdul Rahim Nizamani, ITIT, Gothenburg University
    Created: 2014-02-18
    Note: This is an agent file that defines the agent "NewAgent"
          Syntax:
              Agent working memory: defined by parameter Width
              Agent attention span: defined by parameter Depth
              Max solution length:  defined by parameter Solution
              Training problems for the agent: parameter Filename
              Concepts for the agent: parameter Concepts
          This file is automatically updated by the program.
          Manual changes may be overwritten.
    These starting comments will not be overwritten. Any other comments
      in the file will automatically be removed.
-}
Width ->>_Param 8
Depth ->>_Param 10
Solution ->>_Param 0
Filename ->>_Param "Occam2Examples.hs"
Concepts ->>_Param "Occam2Concepts.hs"
0 ->>_Language Digit
1 ->>_Language Digit
2 ->>_Language Digit
3 ->>_Language Digit
4 ->>_Language Digit
5 ->>_Language Digit
6 ->>_Language Digit
7 ->>_Language Digit
8 ->>_Language Digit
9 ->>_Language Digit
Digit ->>_Language Number
Digit : Digit ->>_Language Number
Number : Number ->>_Language Number
Number ->>_Language Aterm
Aterm + Aterm ->>_Language Aterm
Aterm * Aterm ->>_Language Aterm
0 + 0 ->>_Arith 0
1 + 1 ->>_Arith 2
1 + 2 ->>_Arith 3
1 + 3 ->>_Arith 4
1 + 4 ->>_Arith 5
1 + 5 ->>_Arith 6
1 + 6 ->>_Arith 7
1 + 7 ->>_Arith 8
1 + 8 ->>_Arith 9
1 + 9 ->>_Arith 1 : 0
2 + 1 ->>_Arith 3
2 + 2 ->>_Arith 4
2 + 3 ->>_Arith 5
2 + 4 ->>_Arith 6
2 + 5 ->>_Arith 7
2 + 6 ->>_Arith 8
2 + 7 ->>_Arith 9
2 + 8 ->>_Arith 1 : 0
2 + 9 ->>_Arith 1 : 1
3 + 1 ->>_Arith 4
3 + 2 ->>_Arith 5
3 + 3 ->>_Arith 6
3 + 4 ->>_Arith 7
3 + 5 ->>_Arith 8
3 + 6 ->>_Arith 9
3 + 7 ->>_Arith 1 : 0
3 + 8 ->>_Arith 1 : 1
3 + 9 ->>_Arith 1 : 2
4 + 1 ->>_Arith 5
4 + 2 ->>_Arith 6
4 + 3 ->>_Arith 7
4 + 4 ->>_Arith 8
4 + 5 ->>_Arith 9
4 + 6 ->>_Arith 1 : 0
4 + 7 ->>_Arith 1 : 1
4 + 8 ->>_Arith 1 : 2
4 + 9 ->>_Arith 1 : 3
5 + 1 ->>_Arith 6
5 + 2 ->>_Arith 7
5 + 3 ->>_Arith 8
5 + 4 ->>_Arith 9
5 + 5 ->>_Arith 1 : 0
5 + 6 ->>_Arith 1 : 1
5 + 7 ->>_Arith 1 : 2
5 + 8 ->>_Arith 1 : 3
5 + 9 ->>_Arith 1 : 4
6 + 1 ->>_Arith 7
6 + 2 ->>_Arith 8
6 + 3 ->>_Arith 9
6 + 4 ->>_Arith 1 : 0
6 + 5 ->>_Arith 1 : 1
6 + 6 ->>_Arith 1 : 2
6 + 7 ->>_Arith 1 : 3
6 + 8 ->>_Arith 1 : 4
6 + 9 ->>_Arith 1 : 5
7 + 1 ->>_Arith 8
7 + 2 ->>_Arith 9
7 + 3 ->>_Arith 1 : 0
7 + 4 ->>_Arith 1 : 1
7 + 5 ->>_Arith 1 : 2
7 + 6 ->>_Arith 1 : 3
7 + 7 ->>_Arith 1 : 4
7 + 8 ->>_Arith 1 : 5
7 + 9 ->>_Arith 1 : 6
8 + 1 ->>_Arith 9
8 + 2 ->>_Arith 1 : 0
8 + 3 ->>_Arith 1 : 1
8 + 4 ->>_Arith 1 : 2
8 + 5 ->>_Arith 1 : 3
8 + 6 ->>_Arith 1 : 4
8 + 7 ->>_Arith 1 : 5
8 + 8 ->>_Arith 1 : 6
8 + 9 ->>_Arith 1 : 7
9 + 1 ->>_Arith 1 : 0
9 + 2 ->>_Arith 1 : 1
9 + 3 ->>_Arith 1 : 2
9 + 4 ->>_Arith 1 : 3
9 + 5 ->>_Arith 1 : 4
9 + 6 ->>_Arith 1 : 5
9 + 7 ->>_Arith 1 : 6
9 + 8 ->>_Arith 1 : 7
9 + 9 ->>_Arith 1 : 8
Aterm.x + 0 ->>_Arith Aterm.x
0 + Aterm.x ->>_Arith Aterm.x
Aterm.x + Aterm.y ->>_Arith Aterm.y + Aterm.x
(Aterm.x : 0) + Aterm.y ->>_Arith Aterm.x : Aterm.y
Aterm.z + (Aterm.x : 0) ->>_Arith Aterm.x : Aterm.z
(Aterm.x : Aterm.y) + (Aterm.w : 0) ->>_Arith (Aterm.x + Aterm.w) : Aterm.y
(Aterm.x : 0) + (Aterm.w : Aterm.z) ->>_Arith (Aterm.x + Aterm.w) : Aterm.z
(Aterm.x : Aterm.y) + (Aterm.w : Aterm.z) ->>_Arith (Aterm.x + Aterm.w) : (Aterm.y + Aterm.z)
(Aterm.x : Aterm.y) + Aterm.z ->>_Arith Aterm.x : (Aterm.y + Aterm.z)
(Aterm.x : Aterm.y) + Aterm.z ->>_Arith Aterm.x : (Aterm.z + Aterm.y)
Aterm.z + (Aterm.x : Aterm.y) ->>_Arith Aterm.x : (Aterm.y + Aterm.z)
Aterm.z + (Aterm.x : Aterm.y) ->>_Arith Aterm.x : (Aterm.z + Aterm.y)
(Aterm.x : Aterm.y) + Aterm.z ->>_Arith (Aterm.x : 0) + (Aterm.y + Aterm.z)
T ->>_Language Pterm
F ->>_Language Pterm
X ->>_Language Pvar
Y ->>_Language Pvar
Z ->>_Language Pvar
P ->>_Language Pvar
Q ->>_Language Pvar
Pvar ->>_Language Pterm
Neg Pterm ->>_Language Pterm
Pterm :| Pterm ->>_Language Pterm
Pterm :& Pterm ->>_Language Pterm
Pterm :> Pterm ->>_Language Pterm
Pterm :<> Pterm ->>_Language Pterm
T ->>_Ptruth True
F ->>_Ptruth False
Neg T ->>_Ptruth False
Neg F ->>_Ptruth True
T :| Pterm.x ->>_Ptruth True
Pterm.x :| T ->>_Ptruth True
F :| Pterm.x ->>_Ptruth Pterm.x
Pterm.x :| F ->>_Ptruth Pterm.x
Pterm.x :& F ->>_Ptruth False
F :& Pterm.x ->>_Ptruth False
T :& Pterm.x ->>_Ptruth Pterm.x
Pterm.x :& T ->>_Ptruth Pterm.x
Pterm.x :> T ->>_Ptruth True
F :> Pterm.x ->>_Ptruth True
T :> Pterm.x ->>_Ptruth Pterm.x
T :<> T ->>_Ptruth True
F :<> F ->>_Ptruth True
T :<> F ->>_Ptruth False
F :<> T ->>_Ptruth False
Neg F ->>_Ptaut T
Neg (Neg T) ->>_Ptaut T
Neg (Neg Pterm.x) ->>_Ptaut Pterm.x
Neg (Pterm.x :| Pterm.y) ->>_Ptaut (Neg Pterm.x) :& (Neg Pterm.y)
Neg (Pterm.x :& Pterm.y) ->>_Ptaut (Neg Pterm.x) :| (Neg Pterm.y)
Neg (Pterm.x :> Pterm.y) ->>_Ptaut Pterm.x :& (Neg Pterm.y)
Neg ((Neg Pterm.x) :& (Neg Pterm.y)) ->>_Ptaut Pterm.x :| Pterm.y
Neg (Pterm.x :& (Neg Pterm.y)) ->>_Ptaut (Neg Pterm.x) :| Pterm.y
Neg ((Neg Pterm.x) :& Pterm.y) ->>_Ptaut Pterm.x :| (Neg Pterm.y)
Neg ((Neg Pterm.x) :| (Neg Pterm.y)) ->>_Ptaut Pterm.x :& Pterm.y
Neg (Pterm.x :| (Neg Pterm.y)) ->>_Ptaut (Neg Pterm.x) :& Pterm.y
Neg ((Neg Pterm.x) :| Pterm.y) ->>_Ptaut Pterm.x :& (Neg Pterm.y)
Neg (Pterm.x :<> T) ->>_Ptaut Neg Pterm.x
Neg (T :<> Pterm.x) ->>_Ptaut Neg Pterm.x
Neg (F :<> Pterm.x) ->>_Ptaut Pterm.x
Neg (Pterm.x :<> F) ->>_Ptaut Pterm.x
Neg (Pterm.x :<> Pterm.y) ->>_Ptaut Neg (Pterm.y :<> Pterm.x)
Neg ((Neg Pterm.x) :<> Pterm.x) ->>_Ptaut T
Neg (Pterm.x :<> (Neg Pterm.x)) ->>_Ptaut T
Pterm.x :| T ->>_Ptaut T
T :| Pterm.x ->>_Ptaut T
Pterm.x :| (Neg Pterm.x) ->>_Ptaut T
(Neg Pterm.x) :| Pterm.x ->>_Ptaut T
Pterm.x :| ((Neg Pterm.x) :| y) ->>_Ptaut T
Pterm.x :| (Pterm.y :| (Neg Pterm.x)) ->>_Ptaut T
(Pterm.x :| (Neg Pterm.x)) :| Pterm.y ->>_Ptaut T
(Pterm.x :| Pterm.y) :| (Neg Pterm.x) ->>_Ptaut T
Pterm.y :| (Pterm.x :| (Neg Pterm.x)) ->>_Ptaut T
Pterm.y :| ((Neg Pterm.x) :| Pterm.x) ->>_Ptaut T
(Pterm.y :| Pterm.x) :| (Neg Pterm.x) ->>_Ptaut T
(Pterm.y :| (Neg Pterm.x)) :| Pterm.x ->>_Ptaut T
((Neg Pterm.x) :| Pterm.x) :| Pterm.y ->>_Ptaut T
((Neg Pterm.x) :| Pterm.y) :| Pterm.x ->>_Ptaut T
(Neg Pterm.x) :| (Pterm.y :| Pterm.x) ->>_Ptaut T
(Neg Pterm.x) :| (Pterm.x :| Pterm.y) ->>_Ptaut T
F :| Pterm.x ->>_Ptaut Pterm.x
Pterm.x :| F ->>_Ptaut Pterm.x
Pterm.x :| Pterm.x ->>_Ptaut Pterm.x
Pterm.x :| Pterm.y ->>_Ptaut Pterm.y :| Pterm.x
(Pterm.x :| Pterm.y) :| Pterm.z ->>_Ptaut Pterm.x :| (Pterm.y :| Pterm.z)
Pterm.x :| (Pterm.y :| Pterm.z) ->>_Ptaut (Pterm.x :| Pterm.y) :| Pterm.z
(Pterm.x :& Pterm.y) :| (Pterm.x :& Pterm.z) ->>_Ptaut Pterm.x :& (Pterm.y :| Pterm.z)
Pterm.x :| (Pterm.y :& Pterm.z) ->>_Ptaut (Pterm.x :| Pterm.y) :& (Pterm.x :| Pterm.z)
Pterm.x :| Pterm.y ->>_Ptaut Neg ((Neg Pterm.x) :& (Neg Pterm.y))
(Neg Pterm.x) :| Pterm.y ->>_Ptaut Neg (Pterm.x :& (Neg Pterm.y))
Pterm.x :| (Neg Pterm.y) ->>_Ptaut Neg ((Neg Pterm.x) :& Pterm.y)
(Neg Pterm.x) :| Pterm.y ->>_Ptaut Pterm.x :> Pterm.y
(Neg Pterm.x) :| (Neg Pterm.y) ->>_Ptaut Neg (Pterm.x :& Pterm.y)
Pterm.x :| Pterm.y ->>_Ptaut Pterm.x
Pterm.x :| Pterm.y ->>_Ptaut Pterm.y
Pterm.x :| Pterm.y ->>_Ptaut Pterm.x :| Pterm.y
T :& T ->>_Ptaut T
T :& Pterm.x ->>_Ptaut Pterm.x
Pterm.x :& T ->>_Ptaut Pterm.x
Pterm.x :& Pterm.y ->>_Ptaut Pterm.y :& Pterm.x
(Pterm.x :& Pterm.y) :& Pterm.z ->>_Ptaut Pterm.x :& (Pterm.y :& Pterm.z)
Pterm.x :& (Pterm.y :& Pterm.z) ->>_Ptaut (Pterm.x :& Pterm.y) :& Pterm.z
(Pterm.x :| Pterm.y) :& (Pterm.x :| Pterm.z) ->>_Ptaut Pterm.x :| (Pterm.y :& Pterm.z)
Pterm.x :& (Pterm.y :| Pterm.z) ->>_Ptaut (Pterm.x :& Pterm.y) :| (Pterm.x :& Pterm.z)
(Neg Pterm.x) :& (Neg Pterm.y) ->>_Ptaut Neg (Pterm.x :| Pterm.y)
Pterm.x :& (Neg Pterm.y) ->>_Ptaut Neg (Pterm.x :> Pterm.y)
Pterm.x :& Pterm.y ->>_Ptaut Neg ((Neg Pterm.x) :| (Neg Pterm.y))
(Neg Pterm.x) :& Pterm.y ->>_Ptaut Neg (Pterm.x :| (Neg Pterm.y))
Pterm.x :& (Neg Pterm.y) ->>_Ptaut Neg ((Neg Pterm.x) :| Pterm.y)
Pterm.x :> T ->>_Ptaut T
F :> Pterm.x ->>_Ptaut T
Pterm.x :> Pterm.x ->>_Ptaut T
(T :> Pterm.x) :<> Pterm.x ->>_Ptaut T
(Pterm.x :> F) :<> (Neg Pterm.x) ->>_Ptaut T
Pterm.x :> (Pterm.x :| Pterm.y) ->>_Ptaut T
Pterm.y :> (Pterm.x :| Pterm.y) ->>_Ptaut T
T :> Pterm.x ->>_Ptaut Pterm.x
Pterm.x :> F ->>_Ptaut Neg Pterm.x
(Neg Pterm.y) :> (Neg Pterm.x) ->>_Ptaut Pterm.x :> Pterm.y
(Neg Pterm.x) :> Pterm.x ->>_Ptaut Pterm.x
Pterm.x :> (Neg Pterm.x) ->>_Ptaut Neg Pterm.x
(Pterm.x :& Pterm.y) :> Pterm.x ->>_Ptaut T
(Pterm.x :& Pterm.y) :> Pterm.y ->>_Ptaut T
(Pterm.x :<> Pterm.y) :> (Pterm.x :> Pterm.y) ->>_Ptaut T
(Pterm.x :<> Pterm.y) :> (Pterm.y :> Pterm.x) ->>_Ptaut T
Pterm.x :> (Pterm.y :> Pterm.x) ->>_Ptaut T
Pterm.x :> Pterm.y ->>_Ptaut Pterm.y
Pterm.x :> Pterm.y ->>_Ptaut Neg Pterm.x
(Neg (Neg Pterm.x)) :> Pterm.y ->>_Ptaut Pterm.x :> Pterm.y
Pterm.x :> (Neg (Neg Pterm.y)) ->>_Ptaut Pterm.x :> Pterm.y
Pterm.x :> Pterm.y ->>_Ptaut (Neg Pterm.x) :| Pterm.y
T :<> T ->>_Ptaut T
F :<> F ->>_Ptaut T
Pterm.x :<> Pterm.x ->>_Ptaut T
(Pterm.x :| Pterm.x) :<> Pterm.x ->>_Ptaut T
(Pterm.x :& Pterm.x) :<> Pterm.x ->>_Ptaut T
(Neg Pterm.x) :<> (Neg Pterm.x) ->>_Ptaut T
(Neg (Neg Pterm.x)) :<> Pterm.x ->>_Ptaut T
(Neg (Neg (Neg Pterm.x))) :<> (Neg Pterm.x) ->>_Ptaut T
Pterm.x :<> T ->>_Ptaut Pterm.x
T :<> Pterm.x ->>_Ptaut Pterm.x
F :<> Pterm.x ->>_Ptaut Neg Pterm.x
Pterm.x :<> F ->>_Ptaut Neg Pterm.x
Pterm.x :<> Pterm.y ->>_Ptaut Pterm.y :<> Pterm.x
Pterm.x :<> Pterm.y ->>_Ptaut Pterm.x :<> Pterm.y
(Pterm.x :<> Pterm.y) :<> Pterm.z ->>_Ptaut Pterm.x :<> (Pterm.y :<> Pterm.z)
Pterm.x :<> (Pterm.y :<> Pterm.z) ->>_Ptaut (Pterm.x <> Pterm.y) :<> Pterm.z
((Neg Pterm.x) :| Pterm.y) :<> (Pterm.x :> Pterm.y) ->>_Ptaut T
(Pterm.x :& (Neg Pterm.y)) :<> (Neg (Pterm.x :> Pterm.y)) ->>_Ptaut T
((Neg Pterm.y) :> (Neg Pterm.x)) :<> (Pterm.x :> Pterm.y) ->>_Ptaut T
(Pterm.x :> (Neg Pterm.x)) :<> (Neg Pterm.x) ->>_Ptaut T
((Neg Pterm.x) :> Pterm.x) :<> Pterm.x ->>_Ptaut T
(Neg Pterm.x) :& Pterm.x ->>_Ptaut F
Pterm.x :& Pterm.x ->>_Ptaut Pterm.x

