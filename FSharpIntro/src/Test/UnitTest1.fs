module Test

open NUnit.Framework

// A pet is ...
type Pet = Dog | Cat | Snake

let isCute (pet: Pet): bool =
   match pet with
   | Dog -> true
   | Cat -> true
   | Snake -> false

type Liveness = Alive | Dead

type Weight = double

type Dillo = { liveness : Liveness 
               weight : Weight }

let runOverDillo (dillo: Dillo): Dillo =
    // { liveness = Dead
    //  weight = dillo.weight }
    { dillo with liveness = Dead } // functional update

type Parrot = { sentence : string
                weight : Weight }

let runOverParrot (parrot: Parrot): Parrot =
    { sentence = ""
      weight = parrot.weight }

type Animal =
| MkDillo of Dillo
| MkParrot of Parrot 

let runOverAnimal (animal: Animal): Animal =
    match animal with
    | MkDillo dillo -> MkDillo (runOverDillo dillo)
    | MkParrot parrot -> MkParrot (runOverParrot parrot)

// A list is one the following:
// - the empty list []      OR
// - a cons list ...   ::
let rec listSum (list: list<int>): int =
    match list with
    | [] -> 0
    | (first :: rest) -> first + listSum rest

let rec listFold e o list =
    match list with
    | [] -> e
    | (first :: rest) -> o first (listFold e o rest)

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.AreEqual(true, isCute Dog)
