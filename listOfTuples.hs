module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Char
import Control.Monad

type StateType = [(Int,String)]
type StateMonad a = StateT StateType IO a

convert :: String -> Int
convert [] = 0
convert (x:xs) = ((digitToInt x) * 10^(length xs)) + convert xs

addElement :: [(Int, String)] -> String -> StateMonad () --TROVARE UN MODO AFFINCHÈ NON CI SIA BISOGNO DI PASSARE LA LISTA IN INGRESSO! NON CAPISCO SE GET RITORNA UN TIPO STATEMONAD O [(INT,STRING)]
addElement xs x = modify (\xs -> insert xs x)

insert :: [(Int, String)] -> String -> [(Int, String)]
insert xs x = xs ++ [(length xs + 1 , x)]

cancElement :: [(Int, String)] -> String -> StateMonad ()
cancElement xs x = modify (\xs -> take ( (convert x) -1) xs ++ drop (convert x) xs)

askLoop :: StateMonad () --si utilizza lift perché lift è un trasformatore da monade a monade. In questo caso lift serve per passare dalla monade di IO alla monade IO+State che è appunto StateMonad a.
askLoop = do {
	lift $ putStrLn "Scegli (1:Inserisci aforisma, 2:Scegli elemento da cancellare, 3:visualizza lista)";
	arg <- lift getLine;
	v <- get;
	case arg of
		"1" -> do {
			lift $ putStrLn "Inserisci aforisma";
			aforisma <- lift getLine;
			addElement v aforisma;
			askLoop;
			}
		"2" -> do {
			lift $ putStrLn "Inserisci numero elemento da cancellare";
			num <- lift getLine;
			cancElement v num;
			askLoop;
			}
		"3" -> do {
			lift $ putStrLn "Ecco la tua lista di aforismi";
			lift (print v); --come gestire la stampa di una^^^^^^^^^^^^^^  lista di tuple?
			askLoop;
			}
		_ -> return()
	}

list :: [(Int, String)]
list = [(1, "Ernesto"), (2, "Che"), (3, "Guevara")]


main = do
	runStateT askLoop list
	return ()

