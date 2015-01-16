module MA where
-- Export everything

import List
import {-# SOURCE #-} ClassRep


-----------------------------------------------------------
-- The VM Method Area
-----------------------------------------------------------

type MAIx = Int
type MA = [Class]


newMA :: MA
newMA = []


getClass :: MA -> MAIx -> Class
getClass = (!!)


getClassByName :: MA -> String -> Maybe Class
getClassByName ma name = find ((==name).getName) ma


addClass :: MA -> Class -> (MA,MAIx)
addClass ma c = (ma ++ [c], length ma)


replaceClass :: MA -> MAIx -> Class -> MA
replaceClass = replace


isLoaded :: MA -> String -> Maybe Int
isLoaded ma name = findIndex (==name) (map getName ma)
