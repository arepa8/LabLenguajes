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
{-Comment-}
t1 = Literal(42)
t2 = Suma(Literal(27))(t1)
--t3 = (t2 * (t2 * 1)) + (- ((t1 + 0) / 3))
t3 = Suma(Multiplicacion(t2)(Multiplicacion(t2)(Literal(1))))(Negativo(Division(Suma(t1)(Literal(0)))(Literal(3))))
