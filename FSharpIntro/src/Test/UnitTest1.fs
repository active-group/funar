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

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.AreEqual(true, isCute(Dog))
