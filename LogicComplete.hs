{-
    Occam Agent: Logic.hs
    (Logic.x :| Logic.y) :| Logic.z ->>_Truth Logic.x :| (Logic.y :| Logic.z)
    Logic.x :| (Logic.y :| Logic.z) ->>_Truth (Logic.x :| Logic.y) :| Logic.z
-}
Width ->>_Param 8
Depth ->>_Param 10
Solution ->>_Param 0
F ->>_Lang Logic
T ->>_Lang Logic
p ->>_Lang Logic
q ->>_Lang Logic
r ->>_Lang Logic
s ->>_Lang Logic
not Logic ->>_Lang Logic
Logic :& Logic ->>_Lang Logic
Logic :| Logic ->>_Lang Logic
Logic :> Logic ->>_Lang Logic
Logic :<> Logic ->>_Lang Logic
not T ->>_Truth F
not F ->>_Truth T
T :| Logic.x ->>_Truth T
Logic.x :| T ->>_Truth T
F :| Logic.x ->>_Truth Logic.x
Logic.x :| F ->>_Truth Logic.x
Logic.x :& F ->>_Truth F
F :& Logic.x ->>_Truth F
T :& Logic.x ->>_Truth Logic.x
Logic.x :& T ->>_Truth Logic.x
Logic.x :> T ->>_Truth T
T :> Logic.x ->>_Truth Logic.x
F :> Logic.x ->>_Truth T
T :<> T ->>_Truth T
F :<> F ->>_Truth T
T :<> F ->>_Truth F
F :<> T ->>_Truth F
Logic.x :> Logic.y ->>_Truth (not Logic.x) :| Logic.y
Logic.x :| (not Logic.x) ->>_Truth T
Logic.x :| Logic.y ->>_Truth Logic.y :| Logic.x
Logic.x :& Logic.y ->>_Truth Logic.y :& Logic.x
Logic.x :<> Logic.y ->>_Truth Logic.y :<> Logic.x
(Truth,2,74,:|)
(Truth,1,50,not)
(Truth,0,120,T)
(Truth,0,54,p)
(Truth,0,14,not p)
(Truth,0,44,q)
(Truth,0,2,q :| (not p))
(Truth,0,2,(q :| (not p)) :| p)
(Truth,0,4,q :| T)
(Truth,0,14,T :| q)
(Truth,0,2,T :| p)
(Truth,0,4,T :| F)
(Valid,0,6,F)
(Valid,0,6,not F)
(Truth,0,2,(T :| q) :<> F)
(Truth,0,2,(not p) :| T)
(Lang,1,12,not)
(Lang,0,18,Logic)
(Lang,0,18,T)
(Lang,0,12,not T)
(Lang,0,2,T :| p)
(Lang,0,4,p)
(Lang,2,4,:|)
(Truth,0,6,not F)
(Truth,0,2,T :| (not T))
(Truth,0,2,((not p) :& q) :| T)
(Truth,0,2,(not p) :& q)
(Truth,0,8,not (T :| q))
(Truth,0,2,(T :| q) :<> (F :| T))
(Truth,2,4,:<>)
(Valid,0,2,not (not T))
(Valid,0,2,not T)
(Valid,0,10,T)
(Valid,1,10,not)
(Truth,0,2,(not (not T)) :& (F :| T))
(Truth,0,4,F :| T)
(Truth,0,6,(not (not T)) :& (F :| (T :& T)))
(Truth,0,10,not (not T))
(Truth,0,12,not T)
(Truth,0,6,F :| (T :& T))
(Truth,0,30,F)
(Truth,0,6,T :& T)
(Truth,2,16,:&)
(Truth,0,14,(p :> q) :| p)
(Truth,0,14,p :> q)
(Truth,2,14,:>)
(Truth,0,8,q :| ((not p) :| p))
(Truth,0,8,(not p) :| p)
