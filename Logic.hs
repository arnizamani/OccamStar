{-
    Occam Agent: Logic.hs
-}
Width ->>_Param 8
Depth ->>_Param 10
Solution ->>_Param 6
F ->>_Lang Logic
T ->>_Lang Logic
q ->>_Lang Logic
p ->>_Lang Logic
Logic :& Logic ->>_Lang Logic
Logic :| Logic ->>_Lang Logic
T :| Logic.z ->>_Truth T
(Truth,2,16,:|)
(Truth,0,20,T)
(Truth,0,8,q)
(Truth,0,16,F)
(Truth,0,8,F :| q)
(Truth,0,4,p)
(Truth,0,4,T :| p)
(Truth,0,4,T :| F)
(Lang,2,6,:|)
(Lang,0,24,Logic)
(Lang,0,10,p)
(Lang,0,12,F)
(Lang,0,2,F :| p)
(Lang,0,8,T)
(Lang,0,2,T :| F)
(Lang,0,6,q)
(Lang,0,2,p :| q)
(Lang,0,2,p :& q)
(Lang,0,2,T :& F)
(Lang,0,2,F :& p)
(Lang,2,6,:&)
