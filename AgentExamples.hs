Example 3: done
--(Aequals,(3*0)+2,0+2,1)

Example 4: done
--(Aequals,(1+2)*3,9,1)

Example 5: done
--(Lang,0,Dig,1)
--(Lang,1,Dig,1)
--(Lang,2,Dig,1)
--(Lang,A,Dig,-1)

Example 6: done, 2 hours
--(Lang,1,Num,1)
--(Lang,(1:2),Num,1)
--(Lang,((1:2):2),Num,1)
--(Lang,A,Num,-1)

Example 6.2: done
--(Lang,1:2,Num,1)

Example 7: not done
--(Lang,1:2,Aterm,1)
(Lang,1+2,Aterm,1)
(Lang,A,Aterm,-1)

Example 7.2: done
--(Lang,1*2,Aterm,1)

Example 8: not done

Example 8.2: done
--(Aequals,(1:2)+0,(1:2),1)

Example 9: not done

Example 9.2: done
--(Aequals,1+2,2+1,1)

Example 10: not done

Example 11: done
--(Aequals,f(0),f(0),0)
--(Aequals,f (Num.x),f (Num.x),0)
--(Aequals,f (Num.x + 1),f (Num.x + 1),0)

Example 12:
--(Aequals,f(0),8,1)
--(Aequals,f(1),1:1,1)
--(Aequals,f(2),(1:4),1)
--(Aequals,f(0),0,-1)

Example 12(b): done

f(2)
f(1+1)
f(1)+3
f(0+1) + 3
(f(0) + 3) + 3
(8 + 3) + 3
1:1 + 3
1:4
