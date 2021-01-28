module Comonads.TracedCowriter where
import Control.Comonad.Traced

import Data.Function 
type Option = String 

newtype Config = MakeConfig [Option]
     deriving (Show)

configBuilder :: [Option] -> Config 
configBuilder = MakeConfig

defaultBuilder :: [Option] -> Config 
defaultBuilder opt = MakeConfig ("-Wall" :  opt)

type ConfigBuilder = [Option] -> Config 

profile :: ConfigBuilder -> Config 
profile builder = builder ["-prof", "-auto-all"]

profileDefalt :: Config 
profileDefalt = profile defaultBuilder

goFaster :: ConfigBuilder -> Config 
goFaster builder = builder ["-O2"]

-- хотим совместить goFaster + profiler 

profile' :: ConfigBuilder -> ConfigBuilder 
profile' builder = \opt -> builder (["-prof", "-auto-all"] ++ opt)

goFaster' :: ConfigBuilder -> ConfigBuilder 
goFaster' builder = \opt -> builder (["-O2"] ++ opt)

extract :: ConfigBuilder -> Config 
extract builder = builder []

builder1 :: ConfigBuilder
builder1 = defaultBuilder & goFaster' 

-- Make automatic 

-- someBuilder :: ConfigBuilder -> Config 
-- extend someBuilder :: ConfigBuilder -> ConfigBuilder 
-- move arg to right
-- extend :: ( ConfigBuilder -> Config ) -> ConfigBuilder -> ConfigBuilder 
extend4 :: ( ([Option] -> Config) -> Config ) -> ([Option] -> Config) -> [Option] -> Config 
extend4 builderConfig builder options = builderConfig $ \opt ->  builder ( options ++  opt)

-- goFaster builder = builder ["-O2"]

goFaster'' :: ([Option] -> Config) -> [Option] -> Config
goFaster'' builder = extend goFaster builder -- same as goFaster

type ConfigBuilderTraced = Traced [Option] Config
 
profileTraced :: ConfigBuilderTraced -> Config
profileTraced builder = runTraced builder ["-prof", "-auto-all"]

goFasterTraced :: ConfigBuilderTraced -> Config
goFasterTraced builder = runTraced builder ["-O2"]
 
