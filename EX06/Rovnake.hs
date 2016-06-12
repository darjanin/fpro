module Rovnake where
import TypyLExp
-- type Var = String
-- data LExp = LAMBDA Var LExp | ID Var | APP LExp LExp
rovnake :: LExp -> LExp -> Bool
rovnake (ID x) (ID y) = x == y
rovnake (LAMBDA x lexpx) (LAMBDA y lexpy) = x == y && rovnake lexpx lexpy
rovnake (APP lexpx1 lexpx2) (APP lexpy1 lexpy2)= (rovnake lexpx1 lexpy1) && (rovnake lexpx2 lexpy2)
rovnake _ _ = False
--Príklady:
--a1 = rovnake (ID "x") (ID "x") == True
--a2 = rovnake (ID "x") (ID "y") == False
--a3 = rovnake (LAMBDA "x" (ID "x")) (LAMBDA "x" (ID "x")) == True
--a4 = rovnake (LAMBDA "x" (ID "x")) (LAMBDA "y" (ID "y")) == False