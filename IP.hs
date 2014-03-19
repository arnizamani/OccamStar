which negative examples are not needed?

Example 3: done
--(Aequals,(3*0)+2,0+2,1)

Example 4: done
--(Aequals,(1+2)*3,9,1)

Example 6: Teaching digits - done
--(Lang,0,Dig,1)
--(Lang,1,Dig,1)
--(Lang,2,Dig,1)

Example 6a: done
--(Lang,0,Dig,1)

Example 7: Teaching numbers - done
--(Lang,1,Num,1)
--(Lang,1:2,Num,1)
--(Lang,((2:0):1),Num,1)
--(Lang,2:(1:2),Num,-1)

Example 7a: done
--(Lang,1:2,Num,1)

Example 8: done
--(Lang,1:2,Math,1)
--(Lang,1+2,Math,1)
--(Lang,2+1,Math,1)

Example 8a: done
--(Lang,1+2,Math,1)

Example 9: done
--(Lang,1*2,Math,1)
--(Lang,2*1,Math,1)
--(Lang,2*0,Math,1)

Example 10: done
--(Equality,2+0,2,1)
--(Equality,(1+2)+0,(1+2),1)
--(Equality,(1+2),1,-1)

Example 10a: done
--(Equality,(1+2)+0,(1+2),1)

Teaching addition tables
--(Equality,(9+9),1:8,1)
--(Equality,(9+9),2,-1)
--(Equality,(1+1),1:8,-1)

Example 11: done
--(Equality,1+2,2+1,1)

Eq 25:
--(Equality,8+3,1:1,1)

--(Equality,(1:1)+3,1:4,1)

--(Equality,(1:4)+3,1:7,1)

--(Equality,f(1),f(1),0)

--(Equality,f(1),f(0+1),1)
--(Equality,f(2),f(1+1),1)

Example 10: not done

Example 11: done
--(Equality,f(0),f(0),0)
--(Equality,f(Num.x),f(Num.x + 1),0)
--(Equality,f(Num.x + 1),f(Num.x + 1),0)

Example 12:
--(Equality,f(0),8,1)
--(Equality,f(1),1:1,1)
--(Equality,f(2),(1:4),1)

--(Equality,f(3),(1:7),1)

Example 12(b): done

f(2)
f(1+1)
f(1)+3
f(0+1) + 3
(f(0) + 3) + 3
(8 + 3) + 3
1:1 + 3
1:4



-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

Arithmetic agent

--(Lang,0,Dig,1)
--(Lang,1,Dig,1)

--(Lang,2,Dig,1)
--(Lang,3,Dig,1)

--(Lang,4,Dig,1)
--(Lang,5,Dig,1)

--(Lang,6,Dig,1)
--(Lang,7,Dig,1)

--(Lang,8,Dig,1)
--(Lang,9,Dig,1)

--(Lang,1,Num,1)
--(Lang,1:2,Num,1)
--(Lang,((2:0):1),Num,1)
--(Lang,2:(1:2),Num,-1)

--(Lang,1,Math,1)
--(Lang,1:2,Math,1)
--(Lang,3:4,Math,1)

--(Lang,1+2,Math,1)
--(Lang,2+1,Math,1)
--(Lang,2+0,Math,1)

--(Equality,2+0,2,1)
--(Equality,(1+2)+0,(1+2),1)
--(Equality,(1+2),1,-1)

--(Equality,0+2,2,1)
--(Equality,0+(1+2),(1+2),1)
--(Equality,(1+2),2,-1)

--(Equality,1+2,2+1,1)
--(Equality,2+1,1+2,1)
--(Equality,3+2,2+3,1)
--(Equality,2+3,3+2,1)

--(Equality,5+5,1:0,1)

--(Equality,5+7,1:2,1)

--(Equality,(1:0)+1,1:(0+1),1)
--(Equality,(1:2)+1,1:(2+1),1)

--(Equal,(6:7)*8,(5:3):6,1)
(Equal,2+4,Num,1)
