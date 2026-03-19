module Dobby where

-- base
import Control.Applicative ((<**>))
import Data.Char (toLower)
import Data.Foldable (fold)
import Data.Version (showVersion)

-- dobby
import Paths_dobby (version)

-- http-client
import Network.HTTP.Client (Manager)

-- http-client-tls
import Network.HTTP.Client.TLS (newTlsManager)

-- optparse-applicative
import Options.Applicative (execParser)
import qualified Options.Applicative as Options

main :: IO ()
main = do
  command <- execParser commandParserInfo
  manager <- newTlsManager
  run command manager

commandParserInfo :: Options.ParserInfo Command
commandParserInfo =
  let
    mkCommandMod command =
      Options.command
        (fmap toLower (show command))
        (Options.info
          (pure command)
          (fold [Options.progDesc (show command)]))
  in
    Options.info
      (Options.hsubparser (foldMap mkCommandMod [minBound..maxBound])
        <**> Options.helper
        <**> Options.simpleVersioner (showVersion version))
      (fold
        [ Options.fullDesc
        , Options.header mempty
        , Options.progDesc mempty
        , Options.footer mempty
        ])

data Command
  = Hello
  deriving (Bounded, Enum, Eq, Show)

run :: Command -> Manager -> IO ()
run command _ =
  case command of
    Hello ->
      putStrLn "Hello"
