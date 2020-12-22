module HsSearch.Color
  (
      reset
    , black
    , red
    , green
    , yellow
    , blue
    , purple
    , magenta
    , cyan
    , white
  ) where


reset :: String
reset = "\x1b[0m"

black :: String
black = "\x1b[30m"

red :: String
red = "\x1b[31m"

green :: String
green = "\x1b[32m"

yellow :: String
yellow = "\x1b[33m"

blue :: String
blue = "\x1b[34m"

purple :: String
purple = "\x1b[35m"

magenta :: String
magenta = purple

cyan :: String
cyan = "\x1b[36m"

white :: String
white = "\x1b[37m"
