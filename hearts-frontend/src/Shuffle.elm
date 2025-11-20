module Shuffle exposing (shuffle)

import List.Extra exposing (getAt, removeAt)
import Random exposing (Seed)

shuffleHelper : Seed -> List a -> List a -> List a
shuffleHelper seed xs result =
    if List.isEmpty xs
    then result
    else
        let indexGenerator = Random.int 0 ((List.length xs) - 1)
            (index, nextSeed) = Random.step indexGenerator seed
            valAtIndex = getAt index xs
            xsWithoutValAtIndex = removeAt index xs
        in case valAtIndex of
               Just i ->
                   shuffleHelper nextSeed xsWithoutValAtIndex (i :: result)
               Nothing ->
                   Debug.todo "Index not in list"

shuffle : Seed -> List a -> List a
shuffle seed xs =
    shuffleHelper seed xs []
