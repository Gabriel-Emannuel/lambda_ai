module BaseConcepts.Types where

data Agent t = Reactive t | WithPast t [t]

{--
    perception enviroment agent = newAgent

    action agent enviroment = (newEnviroment, newAgent)
--}