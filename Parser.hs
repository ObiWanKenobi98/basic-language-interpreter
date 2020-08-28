module Parser
where

import Data.Maybe
import InferenceDataType
import ClassState

--pentru a reprezenta un program, m-am folosit de o lista de clase(folosind reprezentarea definita anterior)
data Program = Program{classes :: [ClassState]}
--o intructiune poate fi reprezentata de:
--1:o clasa noua, care contine numele si parintele
--2:o variabila noua, care contine numele si tipul
--3:o functie noua, care contine numele, clasa, tipul returnat si lista de parametri
data Instruction = NewClass{className :: String, classParent :: String} | NewVar{varName :: String, varType :: String} | NewFunction{funcName :: String, className :: String, returnType :: String, params :: [String]} | NoInstr | Expr

--initializez un program nou, care contine o clasa goala
initEmptyProgram :: Program
initEmptyProgram = (Program ((ClassState globalParent [] [] []):[]))

--formateaza variabilele din reprezentarea interna in formatul cerut la iesire
formatVars :: [(String, String)] -> [[String]]
formatVars [] = []
formatVars ((x, y):xs) = ([x, y]:(formatVars xs))

--intoarce toate variabilele din program, formatate corespunzator(mai intai se formateaza variabilele din clasa curenta, apoi se trece la clasa urmatoare)
getVars :: Program -> [[String]]
getVars (Program []) = []
getVars (Program (crt_class:xs)) = ((formatVars (getVariables crt_class))++(getVars (Program xs)))

--intoarce toate clasele din program(numele lor)
getClasses :: Program -> [String]
getClasses (Program []) = ([])
getClasses (Program (crt_class:xs)) = ((getName crt_class):(getClasses (Program xs)))

--intoarce parintele unei clase, data prin numele ei
getParentClass :: String -> Program -> String
getParentClass _ (Program []) = globalParent
getParentClass name (Program (crt_class:xs))
	|((getName crt_class) == name) && (((getParent crt_class) == "") || ((getParent crt_class) == "Global")) = globalParent
	|(getName crt_class) == name = (getParent crt_class)
	|otherwise = (getParentClass name (Program xs))

--formateaza o lista de functii pentru a se potrivi cu formatul de la output
formatFuncs :: [(String, [String])] -> [[String]]
formatFuncs [] = []
formatFuncs ((x, y):xs) = ((x:y):(formatFuncs xs))

--intoarce lista de functii a unei clase(data prin numele sau)
getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass name (Program []) = []
getFuncsForClass name (Program (crt_class:xs))
	|(getName crt_class) == name = (formatFuncs (getFunctions crt_class))
	|otherwise = (getFuncsForClass name (Program xs))

--o functie care separa un string intr-o lista de stringuri, in functie de delimitatorul specificat
splitBy :: String -> Char -> String -> [String]
splitBy [] _ [] = []
splitBy [] _ acc = (acc:[])
splitBy (x:xs) c acc
	|(x == c) = (acc:(splitBy xs c []))
	|otherwise = (splitBy xs c (acc++(x:[])))

--separa inputul in linii
splitToRows :: String -> [String]
splitToRows input = (splitBy input '\n' [])

--separa o linie a inputului in elementele constituente(se pastreaza doar elementele semantice relevante, se elimina simbolurile auxiliare)
split :: String -> String -> [String]
split [] [] = []
split [] acc = (acc:[])
split (x:xs) acc
	|((x == ',') || (x == '.') || (x == '(') || (x == ')') || (x == ':') || (x == '=') || (x == ' ')) && (acc /= []) = (acc:(split xs []))
	|((x == ',') || (x == '.') || (x == '(') || (x == ')') || (x == ':') || (x == '=') || (x == ' ')) = (split xs [])
	|otherwise = (split xs (acc++(x:[])))

--separa lista de randuri a inputului in elementele constituenta, folosind split
splitRow :: [String] -> [[String]]
splitRow [] = []
splitRow (x:xs) = ((split x []):(splitRow xs))

--transforma o linie care contine o functie intr-o instructiune(reprezentarea ei interna)
functionToInstruction :: [String] -> Instruction
functionToInstruction input
	|((tail (tail input)) /= []) = (NewFunction (head (tail (tail input))) (head (tail input)) (head input) (tail (tail (tail input))))
	|otherwise = (NewFunction (head (tail (tail input))) (head (tail input)) (head input) [])

--dupa eliminarea caracterelor speciale, se transforma fiecare rand intr-o instructiune corespunzatoare
rowsToInstructions :: [[String]] -> [Instruction]
rowsToInstructions [] = []
rowsToInstructions (x:xs) 
	|(x == []) = (rowsToInstructions xs)
	|((head x) == "class") && ((length x) == 2) = ((NewClass (head (tail x)) globalParent):(rowsToInstructions xs))
	|((head x) == "class") = ((NewClass (head (tail x)) (last x)):(rowsToInstructions xs))
	|((head x) == "newvar") = ((NewVar (head (tail x)) (last x)):(rowsToInstructions xs))
	|otherwise = ((functionToInstruction x):(rowsToInstructions xs))

--se efectueaza parsarea efectiva a inputului
parse :: String -> [Instruction]
parse input = (rowsToInstructions (splitRow (splitToRows input)))

--se verifica daca o clasa se afla in program(dupa nume); o clasa fara nume sau clasa Global se afla mereu in program
classInProgram :: String -> Program -> Bool
classInProgram name (Program [])
	|(name == "") || (name == globalParent) = True
	|otherwise = False
classInProgram name (Program (x:xs))
	|(name == "") || (name == globalParent) = True
	|(name == (getName x)) = True
	|otherwise = (classInProgram name (Program xs))

--se verifica daca toate clasele specificate se afla in program, folosind, pe rand, functia classInProgram
classesInProgram :: [String] -> Program -> Bool
classesInProgram _ (Program []) = False
classesInProgram [] _ = True
classesInProgram (name:xs) (Program classes)
	|((classInProgram name (Program classes)) == True) = (classesInProgram xs (Program classes))
	|otherwise = False

--se insereaza in clasa specificata o variabila(daca se gaseste in cadrul programului)(folosesc functia de inserare de la primul subpunct)
insertInClass :: String -> String ->[ClassState] -> [ClassState]
insertInClass className varName [] = []
insertInClass className varName (x:xs)
	|(className == (getName x)) = ((insertIntoClass x Var [varName, className]):xs)
	|otherwise = (x:(insertInClass className varName xs))

--se insereaza in clasa specificata o functie(daca se gaseste in cadrul programului)(folosesc functia de inserare de la primul subpunct)
insertFunctionInClass :: Instruction -> [ClassState] -> [ClassState]
insertFunctionInClass instruction [] = []
insertFunctionInClass (NewFunction funcName className returnType params) (x:xs)
	|(className == (getName x)) = ((insertIntoClass x Func ([returnType, funcName] ++ params)):xs)
	|otherwise = (x:(insertFunctionInClass(NewFunction funcName className returnType params) xs))

--functia de interpretare a unei instructiuni; daca vreau sa inserez o clasa care se afla deja in program, programul nu se modifica
--daca gasesc parintele in program, inserez corespunzator clasa, altfel parintele implicit este Global
--la inserarea unvei variabile, daca tipul variabilei este definit, efectuez inserarea, altfel programul nu se modifica
--la inserarea unei functii, caut tipul clasei in program, daca il gasesc inserez, altfel nu modific programul
interpret :: Instruction -> Program -> Program
interpret (NewClass className classParent) (Program classes)
	|(classInProgram className (Program classes)) == True = (Program classes)
	|(classInProgram classParent (Program classes)) == True = (Program ((ClassState className classParent [] []):classes))
	|otherwise = (Program ((ClassState className globalParent [] []):classes))
interpret (NewVar varName varType) (Program classes)
	|(classInProgram varType (Program classes)) == True = (Program (insertInClass varType varName classes))
	|otherwise = (Program classes)
interpret (NewFunction funcName className returnType params) (Program classes)
	|(classesInProgram (className:(returnType:params)) (Program classes)) == True = (Program (insertFunctionInClass (NewFunction funcName className returnType params) classes))
	|otherwise = (Program classes)
interpret _ (Program classes) = (Program classes)

--verifica daca o variabila se gaseste in program, in caz pozitiv returnand tipul sau
variableInProgram :: String -> Program -> Maybe String
variableInProgram _ (Program []) = Nothing
variableInProgram name (Program (crt_class:xs))
	|((variableInClass name (getVariables crt_class)) /= Nothing) = (variableInClass name (getVariables crt_class))
	|otherwise = (variableInProgram name (Program xs))

--verifica daca o variabila se gaseste intr-o clasa data prin  lista sa de variabile, in caz pozitiv returnand tipul sau
variableInClass :: String -> [(String,String)] -> Maybe String
variableInClass _ [] = Nothing
variableInClass name ((x,y):xs)
	|(x == name) = (Just y)
	|otherwise = (variableInClass name xs)

--verifica daca o functie se afla in program, in caz pozitiv intorcand tipul sau de return
functionInProgram :: String -> Program -> Maybe String
functionInProgram _ (Program []) = Nothing
functionInProgram name (Program (crt_class:xs))
	|((functionInClass name (getFunctions crt_class)) /= Nothing) = (functionInClass name (getFunctions crt_class))
	|otherwise = (functionInProgram name (Program xs))

--verifica daca o functie se afla intr-o clasa data prin lista sa de functii, in caz pozitiv intorcand tipul sau de return
functionInClass :: String -> [(String, [String])] -> Maybe String
functionInClass _ [] = Nothing
functionInClass name ((nm, params):xs)
	|(name == nm) = (Just (head params))
	|otherwise = (functionInClass name xs)

--verifica daca o functie se afla intr-o clasa data prin numele sau, in caz pozitiv intorcand tipul sau de return
functionInThisClass :: String -> String -> Program -> Maybe String
functionInThisClass _ _ (Program []) = Nothing
functionInThisClass fname clss (Program (x:xs))
	|((getName x) == clss) = (functionInClass fname (getFunctions x))
	|otherwise = (functionInThisClass fname clss (Program xs))

--intoarce lista de parametri a unei functii(din cadrul unei clase, data prin lista sa de functii)
getFunctionParams :: String -> [(String, [String])] -> [String]
getFunctionParams name [] = []
getFunctionParams name ((nm, aux):xs)
	|(name == nm) = (tail aux)
	|otherwise = (getFunctionParams name xs)

--intoarce atat lista de parametri a unei functii, cat si tipul returnat(din cadrul unei clase, data prin lista sa de functii)
getFunctionInfo :: String -> [(String, [String])] -> [String]
getFunctionInfo name [] = []
getFunctionInfo name ((nm, aux):xs)
	|(name == nm) = aux
	|otherwise = (getFunctionInfo name xs)

--transforma o valoare de tip Maybe String intr-un String
transform :: Maybe String -> String
transform Nothing = []
transform (Just val) = val

--transforma o lista de Maybe String intr-o lista de String
transformList :: [Maybe String] -> [String]
transformList [] = []
transformList (Nothing:xs) = ("":(transformList xs))
transformList ((Just x):xs) = (x:(transformList xs))

--transforma lista de expresii(argumentele unei functii) intr-o lista de Maybe Stringuri(le evalueaza)
checkParams :: [Expr] -> Program -> [Maybe String]
checkParams [] _ = []
checkParams (e1:e) (Program []) = (Nothing:(checkParams e (Program [])))
checkParams (e1:e) program = ((infer e1 program):(checkParams e program))

--verifica daca cel putin un element dintr-o lista de Maybe String este de tipul Nothing, caz in care intoarce True
hasNothing :: [Maybe String] -> Bool
hasNothing [] = False
hasNothing (Nothing:xs) = True
hasNothing ((Just s):xs) = (hasNothing xs)

--verifica daca lista de parametri a unei functii(din cadrul clasei) coincide cu tipurile rezultate in urma evaluarii parametrilor(din expr) si transformarii listei rezultate intr-o lista de stringuri
paramsMatch :: [String] -> [String] -> Bool
paramsMatch [] [] = True
paramsMatch [] _ = False
paramsMatch _ [] = False
paramsMatch (x:xs) (y:ys) = ((x == y) && (paramsMatch xs ys))

--intoarce class-state-ul corespunzator unei functii date prin numele sau
nameToClassState :: String -> Program -> ClassState
nameToClassState _ (Program []) = NoClassState
nameToClassState name (Program (x:xs))
	|((getName x) == name) = x
	|otherwise = (nameToClassState name (Program xs))

--intoarce toate clasele care contin functia data ca parametru pe lantul de mostenire, pentru a le putea evalua in cadrul arborelui
evalFunction :: String -> ClassState -> Program -> [ClassState]
evalFunction name clss program 
	|(((functionInClass name (getFunctions clss)) == Nothing) && ((getParent clss) == "")) = []
	|((functionInClass name (getFunctions clss)) == Nothing) = (evalFunction name (nameToClassState (getParent clss) program) program)
	|((getParent clss) == "") = ((nameToClassState (getName clss) program):[])
	|otherwise = ((nameToClassState (getName clss) program):(evalFunction name (nameToClassState (getParent clss) program) program))

--intoarce toate functiile(tipul returnat + argumente) cu numele specificat din classstate-urile date prin lista
allFunctions :: String -> [ClassState] -> [[String]]
allFunctions name [] = []
allFunctions name (x:xs) = ((getFunctionInfo name (getFunctions x)):(allFunctions name xs))

--verifica daca cel putin o functie de pe lantul de mostenire are tipurile rezultate in urma evaluarii expresiei
checkAllParams :: [String] -> [[String]] -> Maybe String
checkAllParams [] [] = Nothing
checkAllParams _ [] = Nothing
checkAllParams [] _ = Nothing
checkAllParams l (x:xs)
	|(x == []) =  (checkAllParams l xs)
	|((paramsMatch l (tail x)) == True) = (Just (head x))
	|otherwise = (checkAllParams l xs)

--functia efectiva de inter
infer :: Expr -> Program -> Maybe String
infer (Va name) (Program []) = Nothing
infer (Va name) (Program (crt_class:xs))
	|((variableInClass name (getVariables crt_class)) == Nothing) = (infer (Va name) (Program xs))
	|otherwise = (variableInClass name (getVariables crt_class))
infer (FCall var func params) (Program []) = Nothing
infer (FCall var func params) program
	|((variableInProgram var program) == Nothing) = Nothing
	|((hasNothing (checkParams params program)) == True) = Nothing
	|((paramsMatch (transformList (checkParams params program)) (getFunctionParams func (getFunctions (nameToClassState (transform (variableInProgram var program)) program)))) == True) = (functionInThisClass func (transform (variableInProgram var program)) program)
	|otherwise = (checkAllParams (transformList (checkParams params program)) (allFunctions func fncclss))
	where fncclss = (evalFunction func (nameToClassState (transform (variableInProgram var program)) program) program)
