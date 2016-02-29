module Cli
( Options(..)
, parseOptions
) where

import Options.Applicative

data Options = Options
    { port :: Int
    , conf :: String
    } deriving (Show)

portP :: Parser Int
portP = option auto $ mconcat
    [ short 'p', long "port"
    , help "listen port"
    , metavar "PORT"
    , value 3000
    , showDefault
    ]

confP :: Parser String
confP = strOption $ mconcat
    [ short 'c', long "conf"
    , help "config file"
    , metavar "FILE"
    , value "./conf.yaml"
    , showDefaultWith id
    ]

optionsP :: Parser Options
optionsP = Options <$> portP <*> confP

parseOptions = execParser $ info (helper <*> optionsP) $ mconcat
    [ fullDesc
    , progDesc "redirects your http traffic"
    , header "r-proxy - a simple reverse proxy"
    ]

