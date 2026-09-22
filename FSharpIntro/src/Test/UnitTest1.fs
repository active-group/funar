module Test

open NUnit.Framework

// A pet is ...
type Pet = Dog | Cat | Snake

let isCute (pet: Pet): bool =
   match pet with
   | Dog -> true
   | Cat -> true
   | Snake -> false

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.AreEqual(true, isCute(Dog))
