module ST where

data Liveness = Dead | Alive
  deriving (Show, Eq)

data Animal weight =
    Dillo { dilloAlive :: Liveness, dilloWeight :: weight }
  | Parrot String weight
  | Snake Integer Integer
  deriving (Show, Eq)

dillo1 = Dillo { dilloAlive = Alive, dilloWeight = (kg 10) }
dillo2 = Dillo Dead (kg 12)

data Weight = Kg Integer
  deriving (Eq, Show)

instance Ord Weight where
    (Kg a) <= (Kg b) = a <= b

instance Num Weight where
  (Kg w1) + (Kg w2) = Kg (w1 + w2)

kg :: Integer -> Weight
kg quantity
  | quantity >= 0 = Kg quantity
  | otherwise = undefined

-- Tier überfahren
-- runOverAnimal dillo@(Dillo False weight) = dillo
-- runOverAnimal (Dillo _ weight) = Dillo False weight
-- runOverAnimal (Dillo { dilloAlive = alive, dilloweight = weight}) = undefined
-- runOverAnimal dillo@(Dillo {}) = Dillo False (dilloWeight dillo)
runOverAnimal :: Animal weight -> Animal weight
runOverAnimal dillo@(Dillo {}) = dillo { dilloAlive = Dead }
runOverAnimal (Parrot sentence weight) = Parrot "" weight

-- Tier füttern
-- feedAnimal :: Integer -> (Animal -> Animal)
-- geschönfinkelte/currifizierte Funktion
feedAnimal :: Num weight => weight -> Animal weight -> Animal weight
feedAnimal amount (Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)

data StateTransformer state result = ST (state -> (result, state))

removeFromStash :: Weight -> StateTransformer Weight Weight
removeFromStash amount =
   ST (\ stash ->
       if stash >= amount
       then (amount, stash - amount)
       else (stash, kg 0))

banalST :: result -> StateTransformer state result
banalST result = ST (\ state -> (result, state))

addToStash :: Weight -> StateTransformer Weight ()
addToStash amount =
    ST (\ stash -> ((), stash + amount))

removeAndAddDouble :: Weight -> StateTransformer Weight ()
removeAndAddDouble amount =
    removeFromStash amount `andThen` (\ actualAmount ->
        addToStash actualAmount `andThen` (\ () ->
            addToStash actualAmount))

applyST :: StateTransformer state result -> state -> (result, state)
applyST (ST f) state = f state

feedAnimalFromStash :: Weight -> Animal Weight -> StateTransformer Weight (Animal Weight)
feedAnimalFromStash amount animal =
    ST (\ stash ->
         let (actualAmount, newStash) = applyST (removeFromStash amount) stash
         in (feedAnimal actualAmount animal, newStash))


andThen :: StateTransformer state result1 -> (result1 -> StateTransformer state result2)
               -> StateTransformer state result2
andThen st1 fst2 =
    ST (\ state ->
        let (result1, state1) = applyST st1 state
            (result2, state2) = applyST (fst2 result1) state1
        in (result2, state2))

instance Functor (StateTransformer state) where
    -- fmap :: (a -> b) -> StateTransformer state a -> StateTransformer state b
    fmap f st =
        st `andThen` (\ a -> banalST (f a))

instance Applicative (StateTransformer state) where
    pure = banalST
    (<*>) = undefined

-- StateTransformer :: * -> (* -> *)
-- StateTransformer state :: * -> *
instance Monad (StateTransformer state) where
    (>>=) = andThen
    return = banalST

{-
   (>>=) :: [a] -> (a -> [b]) -> [b]
   return :: a -> [a]
   
-}