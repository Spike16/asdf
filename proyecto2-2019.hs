data Carrera = Matematica | Fisica | Computacion | Astronomia | Profesorado deriving (Eq,Show,Ord)
titulo :: Carrera -> String
titulo Matematica = "Profesorado en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en ciencias  de la Computacion"
titulo Astronomia = "Licenciatura en Astronomia"
--usando pattern matching
--1 b)
titulo' :: Carrera -> String
titulo' a | a == Matematica = "Profesorado en Matematica  "
		 | a ==	Fisica = "Licenciatura en Fisica"
		 | a == Computacion = " Licenciatura en Computacion"
		 | a == Astronomia = "Licenciatura en Astronomia"
{-No instance for (Eq Carrera) arising from a use of ‘==’ 
no puede instanciar el valor de a con la el nombre de alguna carrera 
porque el tipo de dato creado no es comparable (Eq=) podria lograrse mediant un deriving Eq-}

--2 (A)
--Ingreso es un sinonimo de tipo
type Ingreso = Int
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving(Eq,Show,Ord)
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving(Eq,Show,Ord)
 --Rol es un tipo algebraico
data Rol =   Decanx {- constructor sin argumento-} | Docente Cargo {- constructor con un argumento -}
             | NoDocente Area  {- constructor con un argumento-} | Estudiante Carrera Ingreso -- constructor con dos argumentos 
             deriving (Eq,Show,Ord)

-- 2 (b) Es un constructor del tipo Rol
cuantos_doc :: [Rol] -> Cargo -> Int
cuantos_doc [] _ = 0
cuantos_doc ((Estudiante b a):xs) c  = cuantos_doc xs c
cuantos_doc (NoDocente a:xs) c = cuantos_doc xs c
cuantos_doc (Decanx:xs) c  = cuantos_doc xs c
cuantos_doc (Docente Titular:xs) Titular = 1 + cuantos_doc xs Titular
cuantos_doc (Docente Asociado:xs) Asociado = 1 + cuantos_doc xs Asociado
cuantos_doc (Docente Adjunto:xs) Adjunto = 1 + cuantos_doc xs Adjunto
cuantos_doc (Docente Asistente:xs) Asistente = 1 + cuantos_doc xs Asistente
cuantos_doc (Docente Auxiliar:xs) Auxiliar = 1 + cuantos_doc xs Auxiliar
cuantos_doc ((Docente _):xs) c = cuantos_doc xs c --Por aca entra cualquier cosa no deseada ,
--es decir en la recursion puede pasar que sea un docente y que no tenga el cargo que deseamos  

-- 3(d)
cuantos_doc' :: [Rol] -> Cargo ->Int
cuantos_doc' xs c =  length (filter ( \x -> x == Docente c ) xs) 
--para aplicar este filter necesito declarar que los argumentos de el tipo Rol sean comparables (EQ)

--3(e)
data Rol' =   Decanx' Genero {- constructor con nuevo argumento-} | Docente' Cargo {- constructor con un argumento -}
             | NoDocente' Area  {- constructor con un argumento-} | Estudiante' Carrera Ingreso deriving(Eq,Show,Ord	) -- constructor con dos argumentos 
data Genero = Masculino | Femenino	deriving(Eq,Show,Ord	)
data Rol'' =   Decanx'' Genero {- constructor con nuevo argumento-} | Docente'' Cargo  {- constructor con un argumento -}
             | NoDocente'' Area {- constructor con un argumento-} | Estudiante'' [(Carrera, Ingreso)] deriving(Eq,Show,Ord	) -- constructor con dos argumentos (der)
 --3(g)
{-estudia :: [Rol''] -> Carrera -> Bool
estudia Estudiante''[] b = False 
estudia Estudiante'' ((Fisica, a):xs) Fisica = True
estudia Estudiante'' ((Matematica,a ):xs) Matematica = True 
estudia Estudiante'' ((Computacion,a):xs) Computacion = True
estudia Estudiante'' ((Astronomia,a):xs ) Astronomia = True
estudia Estudiante'' ((Profesorado,a):xs) Profesorado = True
estudia Estudiante''((c,a):xs) 
estudia _ _ = False -}
estudia :: Rol'' -> Carrera -> Bool
estudia (Estudiante''[]) b = False   
estudia (Estudiante''((Profesorado,i):xs)) Profesorado = True 
estudia (Estudiante''((Matematica,i):xs)) Matematica = True 
estudia (Estudiante''((Astronomia,i):xs)) Astronomia = True
estudia (Estudiante''((Computacion,i):xs)) Computacion = True --Calate ese Patter matching, deriving quien te juna
estudia (Estudiante''((Fisica,i):xs)) Fisica = True 
estudia _ _ = False

--3 a)
data Persona = Per String String Int Int Int Rol'' deriving (Show,Ord,Eq)
edad :: Persona -> (Int, Int, Int) -> Int
edad (Per _ _ a b c _)  (d,e,f) | c >= f  = error "ingrese una fecha valida"
  							   | e > b   = (f-c) -- si el mes es mayor el mes de cumpleaños solo restamos a nuestra fecha dada ,el año de cumpleaños
 							   | e == b  &&  d < a  = (f-c)-1 -- si el mes es menor o igual a el mes de cumpl y el dia dado es menor debo ,restar 1 porque aun no lo cumplio al año entero
 							   | e <= b && d >= a = f-c -- para los demas casos 
--3 b)
existe :: String -> [Persona] -> Bool
existe a [] = True
existe a ((Per _ x _ _ _ _ ):xs) = a == x && existe a (xs)		
darRol:: Persona -> Rol''
darRol (Per _ _ _ _ _ a) = a

--3 c)			   							   
est_astronomia :: [Persona] -> [Persona] --se utiliza la funcion estudia y una auxiliar llamada darRol  para darla ala funicon estudia
est_astronomia [] = []
est_astronomia (x:xs) | (estudia (darRol x) Astronomia)  == True = x : est_astronomia xs
					  | otherwise = est_astronomia xs 
nombredad:: Persona -> (String,Int)
nombredad  (Per a b _ _ c _) = (a++b,c)

{-padron_nodocente :: [Persona] -> [(String, Int)] 
padron_nodocente [] = []
padron_nodocente ((Per a b c _ _  NoDocente''):xs) = (a++b,c) : padron_nodocente xs
padron_nodocente ((Per a b c _ _ _ ):xs) = padron_nodocente xs -}
									
padron_nodocente :: [Persona] -> [(String, Int)]
padron_nodocente [] = []
padron_nodocente ((Per a n d _ _ (NoDocente'' r)):xs) = (a ++ n,d):padron_nodocente xs 
padron_nodocente ((Per _ _ _ _ _ r):xs) = padron_nodocente xs 
data Cola  = Vacia | Encolada Persona Cola deriving (Show,Eq,Ord)

 --5 a
atender :: Cola -> Cola 
atender Vacia = Vacia --Si la cola es vacia entonces devolvemos otra cola vacia
atender (Encolada x xs) | xs /= Vacia = Encolada x (atender xs) --Si x no tiene al frente una cola vacia entonces hago que vaya al siguiente.
                        | otherwise   = atender xs --Ahora que llegamos a un caso en el que la persona no tenga nadie al frente, 
                                                   --osea una cola vacia, devolvemos la cola sin esa persona.

encolar :: Persona -> Cola -> Cola --Encolamos una persona a una cola de personas, en la ultima posicion.  
encolar p Vacia = Encolada p Vacia --Si la cola esta vacia antes de que llegue esta persona, entonces devolvemos una cola con solamente esta persona.
encolar p (Encolada x xs) = Encolada p (Encolada x xs) -- coloca el tipo"enconlada y en el argumnto recursivo pone el "arreglo"

darCargo :: Persona -> Cargo
darCargo (Per _ _ _ _ _ (Docente'' x)) = x
darCargo _ =  error "no es la docente "

{-}
busca :: Cola -> Cargo -> Persona
busca Vacia c = (Per "nadie en el cargo" "o" 1 2 3 _Dencanx''(Masculino))
busca (Encolada x xs) c | (darRol x) == Docente''c = x
						| otherwise = busca Encolada xs c
-}
busca :: Cola -> Cargo -> Persona
busca Vacia c = (Per "nadie en el cargo" "o" 1 2 3 (NoDocente'' Ensenanza) )
busca (Encolada x xs) c | darCargo x == c = x
						| otherwise = busca xs c
data ListaAsoc a b = Empty | Nodo a b ( ListaAsoc a b )deriving(Show,Eq)
type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String
data Guiatelefonica = Diccionario

la_long :: Integral c => ListaAsoc a b -> c
la_long Empty = 0
la_long (Nodo a b (xs)) = 1 + la_long xs

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b 
la_concat xs Empty = Empty
la_concat Empty ys = Empty
la_concat (Nodo a b xs) ys = Nodo a b (la_concat xs ys)

la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Empty = []
la_pares (Nodo a b (xs)) =  (a,b) : la_pares xs
la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b --Toma una lista, una clave y devuelve el dato asociado.
la_busca Empty a = Nothing 
la_busca (Nodo a b c) x | a == x = Just b
						|otherwise = (la_busca c x) 
la_aListaDePares :: ListaAsoc a b -> [(a,b)]-- que devuelve la lista de pares contenida en la lista de asociaciones.
la_aListaDePares Empty = []
la_aListaDePares (Nodo a b (xs)) = (a,b) : la_aListaDePares xs -- igual al ejercicio 3! notar que en estos ejercicios la palabra nodo hace
--de constructor  y que  Lo podemos ver como Nodo a b Empty = (a b) : []
-- como necsito pegar algo a una lista utilizo el constructor de listas : y pongo en una tupla los datos del Nodo .
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b --que dada una clave a elimina la entrada en la lista.
la_borrar c Empty  = Empty
la_borrar c (Nodo a b (xs))   | c ==a = la_borrar c (xs)
							  | otherwise = Nodo a b (la_borrar c (xs) )

data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a )
a_long :: Integral b => Arbol a -> b
a_long Hoja = 0
a_long Rama (x_y)=  1 	+ ((a_long x) + (a_long y))