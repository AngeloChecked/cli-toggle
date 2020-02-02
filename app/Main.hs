module Main where


import Options.Applicative
import Data.Semigroup ((<>))

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Sample -> IO ()
greet (Sample (Hello a) False 1) = print $ a
greet (Sample Init False 1) = print $ "Init"
greet (Sample Template False 1) = print $ "Template"
greet (Sample Push False 1) = print $ "Push"
greet (Sample Pull False 1) = print $ "Pull"
greet a = print $ a

hello :: Parser Cmd
hello = Hello <$> many (argument str (metavar "TARGET..." <> help "Target@"))

data Cmd =  
    Hello [String] 
    | Init 
    | Template 
    | Push 
    | Pull 
    deriving (Eq, Show)

data Sample = Sample 
      { cmd     :: Cmd
      , quiet      :: Bool
      , enthusiasm :: Int } 
    deriving (Eq, Show)

sample :: Parser Sample
sample = Sample
     <$> subparser
       ( command "hello" (info (hello) (progDesc "Print greeting"))
      <> command "init" (info (pure Init) (progDesc "Say Init"))
      <> command "template" (info (pure Template) (progDesc "Say Template"))
      <> command "push" (info (pure Push) (progDesc "Say Push"))
      <> command "pull" (info (pure Pull) (progDesc "Say Pull"))
       )
     <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

