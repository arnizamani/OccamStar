
--(Lang,T,Logic,1)
--(Lang,F,Logic,1)

--(Truth,T,Goal,1)

--(Lang,p,Logic,1)
--(Lang,q,Logic,1)

--(Lang,p :& q,Logic,1)
--(Lang,T :& F,Logic,1)
--(Lang,F :& p,Logic,1)

(Lang,p :| q,Logic,1)
(Lang,T :| F,Logic,1)
(Lang,F :| p,Logic,1)

--(Lang,p :> q,Logic,1)
--(Lang,T :> F,Logic,1)
--(Lang,F :> p,Logic,1)

--(Lang,not p,Logic,1)
--(Lang,not q,Logic,1)
--(Lang,not T,Logic,1)

--(Truth,T :| F,T,1)
--(Truth,T :| p,T,1)
--(Truth,F :| q,F,-1)
--(Truth,F :| q,T,-1)

--(Truth,F :| T,T,1)
--(Truth,p :| T,T,1)
--(Truth,q :| F,F,-1)
--(Truth,q :| F,T,-1)

--(Truth,(p :> q),(not p :| q),1)
--(Truth,(q :> p),(not q :| p),1)

--(Truth,not (not T) :& (F :| (T :& T)),T,1)
--(Truth,(q :| not p) :| p,T,1)




