{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

import Control.Applicative	(pure)
import Control.DeepSeq 		(NFData, ($!!))
import Control.Monad 		(void)
import Data.Map				(Map, empty, foldMapWithKey, singleton)
import GHC.Generics 		(Generic)
import System.Environment	(getArgs, getProgName)
import System.IO			(hPutStrLn, stderr)

data Expresion
	= Suma				Expresion Expresion
	| Resta				Expresion Expresion
	| Multiplicacion	Expresion Expresion
	| Division			Expresion Expresion
	| Negativo			Expresion
	| Literal			Integer
	deriving(Eq, Read, Show)

t1 :: Expresion
t2 :: Expresion
t3 :: Expresion

{- Ejercicio 1-}

t1 = Literal(42)

t2 = Suma(Literal(27))(t1)

t3 = Suma(Multiplicacion(t2)(Multiplicacion(t2)(Literal(1))))
	(Negativo(Division(Suma(t1)(Literal(0)))(Literal(3))))

{- Ejercicio 2-}

evaluar :: Expresion -> Double
evaluar
	= \ case
		Suma	e1 e2 -> evaluar(e1) + evaluar(e2)
		Resta	e1 e2 -> evaluar(e1) - evaluar(e2)
		Multiplicacion	e1 e2 -> evaluar(e1) * evaluar(e2)
		Division	e1 e2 -> evaluar(e1) / evaluar(e2)
		Negativo	e -> -evaluar(e)
		Literal	n -> fromIntegral(n)

{- Ejercicio 3-}

operaciones :: Expresion -> Integer
operaciones
	= \ case
		Suma	e1 e2 -> operaciones(e1) + operaciones(e2) + 1
		Resta	e1 e2 -> operaciones(e1) + operaciones(e2) + 1
		Multiplicacion	e1 e2 -> operaciones(e1) + operaciones(e2) + 1
		Division	e1 e2 -> operaciones(e1) + operaciones(e2) + 1
		Negativo	e -> operaciones(e) + 1
		Literal	n -> 0

{- Ejercicio 4-}

sumaLiterales :: Expresion -> Integer
sumaLiterales
	= \ case
		Suma	e1 e2 -> sumaLiterales(e1) + sumaLiterales(e2)
		Resta	e1 e2 -> sumaLiterales(e1) + sumaLiterales(e2)
		Multiplicacion	e1 e2 -> sumaLiterales(e1) + sumaLiterales(e2)
		Division	e1 e2 -> sumaLiterales(e1) + sumaLiterales(e2)
		Negativo	e -> sumaLiterales(e)
		Literal	n -> n

{- Ejercicio 5-}

literales :: Expresion -> [Integer]
literales
	= \ case
		Suma	e1 e2 -> literales(e1) ++ literales(e2)
		Resta	e1 e2 -> literales(e1) ++ literales(e2)
		Multiplicacion	e1 e2 -> literales(e1) ++ literales(e2)
		Division	e1 e2 -> literales(e1) ++ literales(e2)
		Negativo	e -> literales(e)
		Literal	n -> [n]

{- Ejercicio 6-}

altura :: Expresion -> Integer
altura
	= \ case 
		Suma	e1 e2 -> max(altura(e1))(altura(e2)) + 1
		Resta	e1 e2 -> max(altura(e1))(altura(e2)) + 1
		Multiplicacion	e1 e2 -> max(altura(e1))(altura(e2)) + 1
		Division	e1 e2 -> max(altura(e1))(altura(e2)) + 1
		Negativo	e -> altura(e) + 1
		Literal	n -> 0

{- Ejercicio 7-}





{-	Ejercicio 9-}

type Atributos
	= Map String String

newtype Documento
	= Documento Elemento
	deriving Show

data Elemento
	= Elemento String Atributos [Elemento]
	| Texto String
	deriving Show


htmlE, headE, bodyE, divE :: [Elemento] -> Elemento
htmlE	f =	(Elemento) ("html") (singleton ("xmlns")("http://www.w3.org/1999/xhtml")) f
headE	f =	(Elemento) ("head") empty f
bodyE	f =	(Elemento) ("body") empty f
divE 	f =	(Elemento) ("div") empty f


{-	Ejercicio 10-}

styleE, titleE, hiE, pE :: String -> Elemento
styleE  s	=	(Elemento) ("style") (singleton ("type")("text/css")) [Texto s]
titleE  s	=	(Elemento) ("title") empty [Texto s]
hiE	    s	=	(Elemento) ("h1") empty [Texto s]
pE		s 	=	(Elemento) ("p") empty [Texto s]

{-	Ejercicio 11-}

showP :: Show a => a -> Elemento
showP a = (Elemento) (show a) empty [Texto (show a)]


{- Ejercicio 12-}

class RenderXHTML a where
	render :: a -> String

instance RenderXHTML Documento where
	render (Documento raiz)
		= encabezado ++ render raiz
		where
			encabezado
				= unlines
				[ "<?xml version='1.0' encoding='UTF-8'?>"
				, "<!DOCTYPE html"
				, "     PUBLIC '-//W3C//DTD XHTML 1.0 Strict//EN'"
				, "     'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd'>"
				]

instance RenderXHTML Atributos where
	render (Atributos raiz)
		= lista ++ render raiz
		where
			lista
				= unlines
				[ singleton("foo")("bar baz")
				, singleton("quux")("meh")
				, singleton("wtf")("wow://such.example.com/amaze/")
				]

instance RenderXHTML Elemento where
	render (Elemento raiz)
		= undefined

