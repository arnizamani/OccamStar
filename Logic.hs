{-
    Occam Agent: Logic.hs
-}
Width ->>_Param 8
Depth ->>_Param 10
Solution ->>_Param 6
F ->>_Lang Logic
T ->>_Lang Logic
T ->>_Truth Goal
q ->>_Lang Logic
p ->>_Lang Logic
Logic :& Logic ->>_Lang Logic
Logic :| Logic ->>_Lang Logic
(Lang,2,6,:|)
(Lang,0,20,Logic)
(Lang,0,10,p)
(Lang,0,10,F)
(Lang,0,2,F :| p)
(Lang,0,6,T)
(Lang,0,2,T :| F)
(Lang,0,6,q)
(Lang,0,2,p :| q)
(Truth,0,2,T)
(Truth,0,2,Goal)
(Lang,0,2,p :& q)
(Lang,0,2,T :& F)
(Lang,0,2,F :& p)
(Lang,2,6,:&)
