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

--sumaLiterales :: Expresion -> Integer
--sumaLiterales = 
