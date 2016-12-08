module day07tests

open NUnit.Framework
open day07

[<TestFixture>]
type Day07Tests () = 
    let sample1 = "abba[mnop]qrst"
    let sample2 = "abcd[bddb]xyyx"

    let sample3 = "aaaa[qwer]tyui"

    let sample4 = "ioxxoj[asdfgh]zxcvbn"

    let part2_data1 = "aba[bab]xyz"
    let part2_data2 = "xyx[xyx]xyx"
    let part2_data3 = "aaa[kek]eke"
    let part2_data4 = "zazbz[bzb]cdb"

    [<Test>]
    member x.``tryIpSsl with ip 'zazbz[bzb]cdb' should return Some ip`` () =
        let expected = Some part2_data4
        let result = part2_data4 |> tryIpSsl
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member x.``tryIpSsl with ip 'aaa[kek]eke' should return Some ip`` () =
        let expected = Some part2_data3
        let result = part2_data3 |> tryIpSsl
        Assert.That(result, Is.EqualTo expected)


    [<Test>]
    member x.``getSsls with 'xyx[xyx]xyx' should result in a non empty sequence`` () =
        Assert.IsNotEmpty(part2_data2 |> getSsls)

    [<Test>]
    member x.``tryIpSsl with ip 'xyx[xyx]xyx' should return None`` () =
        let expected = None
        let result = part2_data2 |> tryIpSsl
        Assert.That(result, Is.EqualTo expected)

    [<Test>]
    member x.``tryIpSsl with ip 'aba[bab]xyz' should return Some ip`` () =
        let expected = Some part2_data1
        let result = part2_data1 |> tryIpSsl
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