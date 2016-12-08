module day07tests

open NUnit.Framework
open day07

[<TestFixture>]
type Day07Part2ValidSslTests () =
    [<Datapoints>]
    member x.ValidSsls = [|"aba[bab]xyz"; "aaa[kek]eke"; "zazbz[bzb]cdb"|]

    [<Theory>]
    member x.``tryIpSsl with a SSL supporting IP should return Some IP`` ip =
        let expected = Some ip
        let result = ip |> tryIpSsl
        Assert.That(result, Is.EqualTo expected) 

[<TestFixture>]
type Day07Tests () = 
    let sample1 = "abba[mnop]qrst"
    let sample2 = "abcd[bddb]xyyx"

    let sample3 = "aaaa[qwer]tyui"

    let sample4 = "ioxxoj[asdfgh]zxcvbn"

    let part2_data1 = "aba[bab]xyz"
    let part2_data2 = "xyx[xyx]xyx"

    [<Test>]
    member x.``getSsls with 'xyx[xyx]xyx' should result in a non empty sequence`` () =
        Assert.IsNotEmpty(part2_data2 |> getSsls)

    [<Test>]
    member x.``tryIpSsl with ip 'xyx[xyx]xyx' should return None`` () =
        let expected = None
        let result = part2_data2 |> tryIpSsl
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member x.``getSsls with 'aba[bab]xyz' should return a non empty sequence`` () =
        Assert.IsNotEmpty(part2_data1 |> getSsls)

    [<Test>]
    member x.``getAbbas with 'abba[mnop]qrst' should return a non empty sequence`` () =
        Assert.IsNotEmpty(sample1 |> getAbbas)

    [<Test>]
    member x.``getHyperNetSequences with 'abba[mnop]qrst' should return a none empty sequence`` () =
        Assert.IsNotEmpty(sample1 |> ipSegments |> fst)


    [<Test>]
    member x.``tryIpTls with ip 'abba[mnop]qrst' should return Some ip`` () = 
        let expected = Some sample1
        let result = sample1 |> tryIpTls
        Assert.That(result, Is.EqualTo expected)


    [<Test>]
    member x.``tryIpTls with ip 'abcd[bddb]xyyx' should return None`` () = 
        let expected = None
        let result = sample2 |> tryIpTls
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member x.``tryIpTls with ip 'aaaa[qwer]tyui' should return None`` () = 
        let expected = None
        let result = sample3 |> tryIpTls
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member x.``tryIpTls with ip 'ioxxoj[asdfgh]zxcvbn' should return Some ip`` () = 
        let expected = Some sample4
        let result = sample4 |> tryIpTls
        Assert.That(result, Is.EqualTo expected)