module day07tests

open NUnit.Framework
open day07

let sample1 = "abba[mnop]qrst"
let sample2 = "abcd[bddb]xyyx"

let sample3 = "aaaa[qwer]tyui"

let sample4 = "ioxxoj[asdfgh]zxcvbn"

[<Test>]
let ``tryGetAbbas with 'abba[mnop]qrst' should return a none empty sequence`` () =
    Assert.IsNotEmpty(sample1 |> tryGetAbbas)

[<Test>]
let ``getHyperNetSequences with 'abba[mnop]qrst' should return a none empty sequence`` () =
    Assert.IsNotEmpty(sample1 |> getHyperNetSequences)


[<Test>]
let ``Ip 'abba[mnop]qrst' should return Some ip for supporting tls`` () = 
    let expected = Some sample1
    let result = sample1 |> tryIp7Tls
    Assert.That(result, Is.EqualTo expected)


[<Test>]
let ``Ip 'abcd[bddb]xyyx' should return None for supporting tls`` () = 
    let expected = None
    let result = sample2 |> tryIp7Tls
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Ip 'aaaa[qwer]tyui' should return None for supporting tls`` () = 
    let expected = None
    let result = sample3 |> tryIp7Tls
    Assert.That(result, Is.EqualTo expected)

[<Test>]
let ``Ip 'ioxxoj[asdfgh]zxcvbn' should return Some ip for supporting tls`` () = 
    let expected = Some sample4
    let result = sample4 |> tryIp7Tls
    Assert.That(result, Is.EqualTo expected)