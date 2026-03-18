module Dobby where

-- base
import Control.Applicative ((<**>))
import Data.Foldable (fold)
import Data.Version (showVersion)

-- dobby
import Paths_dobby (version)

-- http-client-tls
import Network.HTTP.Client.TLS (newTlsManager)

-- optparse-applicative
import Options.Applicative (execParser)
import qualified Options.Applicative as Options

main :: IO ()
main = do
  command <- execParser commandParserInfo
  _manager <- newTlsManager
  case command of
    Hello ->
      putStrLn "Hello"

data Command
  = Hello

commandParserInfo :: Options.ParserInfo Command
commandParserInfo =
  let
    commandParser =
      commandParser2
        <**> Options.helper
        <**> Options.simpleVersioner (showVersion version)
    mods =
      [ Options.fullDesc
      , Options.header mempty
      , Options.progDesc mempty
      , Options.footer mempty
      ]
  in
    Options.info commandParser (fold mods)

commandParser2 :: Options.Parser Command
commandParser2 =
  let
    helloParserInfo =
      let
        mods =
          [ Options.progDesc "Hello"
          ]
      in
        Options.info (pure Hello) (fold mods)
    cmds =
      [ Options.command "hello" helloParserInfo
      ]
  in
    Options.hsubparser (fold cmds)
