(Language,2,1,*)
(Language,0,8,Aterm)
(Language,0,44,3)
(Language,0,46,2)
(Language,0,1,2 * 3)
(Arith,0,69,0)
(Arith,0,28,3 : 0)
(Arith,0,1,(2 : 5) + 5)
(Arith,0,21,3 + 4)
(Arith,0,22,4 + 3)
(Arith,0,57,1)
(Arith,0,17,1 + 0)
(Language,2,1,+)
(Language,0,9,0)
(Language,0,1,1 + 0)
(Arith,0,17,8)
(Arith,0,16,1 : 8)
(Arith,0,2,(1 : 8) + 0)
(Language,0,158,Number)
(Arith,0,3,7)
(Ptaut,0,1,P :& (Neg P))
(Ptaut,0,1,Neg (P :& (Neg P)))
(Ptaut,0,1,Neg (Neg (P :& (Neg P))))
(Ptaut,0,1,Neg (Neg (Neg (P :& (Neg P)))))
(Ptaut,0,1,Q :> P)
(Ptaut,0,1,P :> (Q :> P))
(Ptaut,0,2,(Neg P) :& P)
(Ptaut,0,2,((Neg P) :& P) :> Q)
(Ptaut,0,7,P :> (Neg (Neg P)))
(Ptaut,0,2,(P :| P) :> P)
(Ptaut,0,8,X)
(Ptaut,0,8,T :| X)
(Ptaut,0,13,F :| T)
(Ptaut,0,12,F :| (F :| T))
(Ptaut,0,8,(F :| (F :| T)) :| (T :| X))
(Ptruth,2,16,:|)
(Ptruth,0,4,True)
(Ptruth,0,4,X)
(Ptruth,0,8,T)
(Ptruth,0,4,T :| X)
(Ptruth,0,8,F)
(Ptruth,0,4,F :| T)
(Ptruth,0,4,F :| (F :| T))
(Ptruth,0,4,(F :| (F :| T)) :| (T :| X))
(Ptaut,0,1,x)
(Ptaut,0,1,T :| x)
(Ptaut,0,1,(F :| (F :| T)) :| (T :| x))
(Ptaut,0,1,(F :| (F :| T)) :| (F :| T))
(Ptaut,0,5,F :& F)
(Ptaut,0,4,F :| (F :& F))
(Ptaut,0,3,(F :| (F :& F)) :| T)
(Ptaut,0,2,T :| (F :| F))
(Ptaut,0,2,T :| F)
(Ptaut,0,1,T :| (F :& F))
(Arith,0,1,5 : 9)
(Arith,0,1,1 : 5)
(Arith,0,6,4 : 4)
(Arith,0,1,(4 : 4) + (1 : 5))
(Arith,0,1,6 : 2)
(Arith,0,1,(4 : 4) + (1 : 8))
(Arith,0,1,7 : 5)
(Arith,0,1,3 : 1)
(Arith,0,1,(4 : 4) + (3 : 1))
(Arith,0,4,4 + (3 : 0))
(Arith,0,22,(3 : 0) + 4)
(Arith,0,2,2 + 9)
(Arith,0,2,9 + 2)
(Arith,0,4,9 + 0)
(Arith,0,4,0 + 9)
(Arith,0,5,9 + 9)
(Language,0,4,"Number!")
(Language,0,2,8 : 0)
(Language,0,1,1 : (8 : 0))
(Language,0,32,9)
(Language,0,2,7)
(Language,0,2,6)
(Language,0,36,3 : 4)
(Language,0,36,1 : 2)
(Language,0,1,y)
(Language,0,1,x)
(Language,0,3,X)
(Language,0,4," ")
(Language,0,2,5)
(Language,0,14,"Digit!")
(Arith,0,3,0 + 0)
(Arith,0,1,A + 0)
(Arith,0,1,A)
(Arith,0,3,0 + (1 : 8))
(Arith,0,1,(4 : 4) + (3 : 0))
(Arith,0,1,7 : 4)
(Arith,0,1,(4 : 4) + 8)
(Arith,0,1,5 : 2)
(Arith,0,1,(4 : 4) + (1 : 6))
(Arith,0,1,1 : 6)
(Arith,0,1,6 : 0)
(Arith,0,3,6)
(Arith,0,1,4 + ((1 : 1) : 5))
(Arith,0,1,(1 : 1) : 5)
(Arith,0,1,(1 : 1) : 9)
(Arith,0,2,1 : 1)
(Ptaut,0,1,Neg F)
(Ptaut,0,1,T :| (F :| (F :& F)))
(Ptaut,0,1,(F :| (F :| T)) :| F)
(Ptaut,0,1,(F :| (F :| T)) :| (F :| F))
(Ptaut,0,3,F :| F)
(Ptaut,0,1,((Neg P) :| P) :> Q)
(Ptaut,0,1,(Neg P) :| P)
(Ptaut,0,1,F :> Q)
(Ptaut,0,50,F)
(Ptaut,0,32,True)
(Ptaut,0,2,(P :| P) :> (Neg P))
(Ptaut,0,7,P :> (Neg P))
(Ptaut,0,2,(P :| P) :> (neg (neg P)))
(Ptaut,0,2,neg (neg P))
(Ptaut,0,2,neg P)
(Ptaut,1,4,neg)
(Language,0,3,Neg (Neg Pterm))
(Language,0,7,Neg Pterm)
(Language,0,5,Neg (Neg P))
(Language,0,6,Neg P)
(Language,0,7,P)
(Language,0,19,Pterm)
(Language,1,21,Neg)
(Ptaut,0,8,(P :| P) :> (Neg (Neg P)))
(Ptaut,0,14,P :| P)
(Ptaut,0,15,Neg (Neg P))
(Ptaut,0,1,((Neg P) :<> P) :> (Q :<> Q))
(Ptaut,0,1,Q :<> Q)
(Ptaut,0,1,Neg ((Neg P) :<> P))
(Ptaut,0,2,(Neg P) :<> P)
(Ptaut,2,3,:<>)
(Ptaut,0,1,(P :> P) :| Q)
(Ptaut,0,2,P :> P)
(Ptaut,0,8,Q)
(Ptaut,2,37,:>)
(Ptaut,0,2,(P :& P) :| (Neg P))
(Ptaut,0,2,P :& P)
(Ptaut,0,32,Neg P)
(Ptaut,0,94,P)
(Ptaut,0,77,T)
(Ptaut,1,52,Neg)
(Ptaut,2,80,:|)
(Ptaut,2,10,:&)
(Ftruth,0,1,Red)
(Ftruth,0,3,Red "1.1")
(Ftruth,0,5,"1.1")
(Ftruth,0,2,Red 1)
(Ftruth,0,2,1)
(Ftruth,0,6,T)
(Ftruth,1,5,Red)
(Language,0,15,Digit)
(Language,0,41,4)
(Language,0,1,1 : 8)
(Language,0,54,1)
(Language,0,36,8)
(Language,2,76,:)
(Arith,0,3,4 + (2 : 5))
(Arith,0,3,2 : 9)
(Arith,0,1,(2 : 5) + 9)
(Arith,0,5,2 : 5)
(Arith,0,14,2)
(Arith,0,11,5)
(Arith,0,35,9)
(Arith,0,27,3 : 4)
(Arith,0,99,3)
(Arith,0,113,4)
(Arith,2,124,+)
(Arith,2,98,:)