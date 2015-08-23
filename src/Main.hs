module Main where
import Parser (runParser, lambdaExpr, showLambda, oper)

main :: IO ()
main =
    do { putStr "> "
       ; l <- getLine
       ; case l of
           "exit" -> return ()
           _      -> do { case runParser lambdaExpr () "" l of
                            Right t      -> mapM_ (putStrLn . showLambda) (oper t)
                            Left _       -> putStrLn "<parser error>"
                        ; main
                        }
       }

