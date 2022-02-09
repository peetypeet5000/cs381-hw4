-- Stacklang1 Tests

stack1::Stack
stack1 = [1, 2, 3, 4, 5] 
test1 = [LD 3,DUP,ADD,DUP,MULT]  
test2 = [LD 3,ADD] 
test3 = [] 
test4 = [ADD, ADD, ADD, ADD]

-- Stacklang2 Tests
stack1 :: Stack
stack1 = [Right 1, Right 3, Right 5, Right 7, Right 9]
stack2 :: Stack
stack2 = [Left True, Right 3] 
test1 = [LDI 3, DUP, ADD, DUP, MULT]
test2 = [LDB True, DUP, IFELSE [LDI 1][LDI 0]]
test3 = [LEQ]
test4 = [ADD, ADD, MULT, DUP]
test5 = [LEQ, IFELSE [] [], LDI 9]
test6 = [LDI 5, LDI 2, LEQ, IFELSE [LDI 10, DUP] [], ADD]
test7 = [LDI 5, LDI 7, LEQ, IFELSE [LDI 10, DUP] [LDI 20, DUP], ADD]
test8 = [LDI 1, LDI 2, LDI 3, LDI 4, LDI 5, ADD, ADD, ADD, ADD]
