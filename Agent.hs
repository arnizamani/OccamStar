{-
    Author: Abdul Rahim Nizamani, ITIT, Gothenburg University
    Created: 2014-03-04
    Note: This is an agent file that defines the agent "Agent"
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
Solution ->>_Param 6
Filename ->>_Param "AgentExamples.hs"
Concepts ->>_Param "AgentConcepts.hs"
0 ->>_Lang Dig
1 ->>_Lang Dig
2 ->>_Lang Dig
Dig ->>_Lang Num
Num : Num ->>_Lang Num
Num ->>_Lang Aterm

