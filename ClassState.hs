module ClassState
where

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq)

--name este numele clasei
--parent este parintele clasei
--functions contine o lista in care se memoreaza perechi de forma (nume_functie, lista_informatii_functie)
--unde lista_informatii_functie contine tipul functiei si tipurile parametrilor
--variables contine o lista de perechi de forma (nume_variabila, tip_variabila)
data ClassState = ClassState{name :: String, parent :: String, functions :: [(String, [String])], variables :: [(String, String)]} | NoClassState

--intoarce numele clasei
getName :: ClassState -> String
getName (ClassState name parent functions variables) = name

--intoarce numele parintelui clasei
getParent :: ClassState -> String
getParent (ClassState name parent functions variables) = parent

--intoarce lista de functii a clasei, condificata conform conventiei anterioare
getFunctions :: ClassState -> [(String, [String])]
getFunctions (ClassState name parent functions variables) = functions

--intoarce lista de variabile a clasei, codificata conform conventiei anteiroare
getVariables :: ClassState -> [(String, String)]
getVariables (ClassState name parent functions variables) = variables

--constanta, parintele implicit al tuturor claselor; parintele lui global este "", folosit in urmatoarele subpuncte
globalParent :: String
globalParent = "Global"

--initializeaza o clasa vida, fara nume, variabile sau functii
initEmptyClass :: ClassState
initEmptyClass = (ClassState [] globalParent [] [])

--insereaza in clasa o functie sau o variabila
insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (ClassState name parent functions variables) Var [var_name, var_type] = (ClassState name parent functions ((var_name, var_type):variables))
--in cazul inserarii unei functii, prelucrez lista primita ca input pentru a se potrivi cu reprezentarea interna a unei functii
insertIntoClass (ClassState name parent functions variables) Func list = (ClassState name parent (((head (tail list)), ((head list):(tail (tail list)))):functions) variables)

--intoarce lista de functii sau de variabile a unei clase; se parcurge lista aferenta si se formateaza(in cazul functiilor)
getValues :: ClassState -> InstrType -> [[String]]
getValues (ClassState name parent functions []) Var = []
getValues (ClassState name parent functions ((var_name, var_type):xs)) Var = ([var_name, var_type]:(getValues (ClassState name parent functions xs) Var))
getValues (ClassState name parent [] variables) Func = []
getValues (ClassState name parent ((function_name, function_info):xs) variables) Func = (((head function_info):function_name:(tail function_info)):(getValues (ClassState name parent xs variables)) Func)
