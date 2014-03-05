
--(Arith,4 + 3,3 + 4,1)
--(Arith,1 + 0,1,1)
--(Language,2*3,Aterm,1)
--(Arith,(2 : 5) + (1 : 6),(4 : 1),1)
-- (Ptruth,(F :| (F :| T)) :| (T :| X),True,1)

1 (Ptaut,P :> ((Neg (Neg P) :> P) :| Q),T,1)
2 done (Ptaut,(Neg P :<> P) :> (Q :<> Q),T,1)
3 done (Ptaut,(P :| P) :> Neg (Neg P),T,1)
5 done (Ptaut,(Neg P :& P) :> Q,T,1)
7 done (Ptaut,P :> (Q :> P),T,1)
9 done (Ptaut,Neg (Neg P :<> P),T,1)
10 (Ptaut,(Neg (P :> Q) :| X) :> Neg Q,T,1)
11 done (Ptaut,Neg (Neg (Neg (P :& Neg P))),T,1)
12 done (Ptaut,(P :> P) :| Q,T,1)
15 done (Ptaut,(P :& P) :| Neg P,T,1)



Examples to solve:
4:4 + 8             5
4:0 + (4 + 8)       7
4:0 + 1:2           7
4+1):2             5
5:2                 2
