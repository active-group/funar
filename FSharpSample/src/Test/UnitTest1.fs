module Test

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

let foo x y = x + y

[<Test>]
let Test1 () =
    Assert.AreEqual(foo 1 2, 3)
