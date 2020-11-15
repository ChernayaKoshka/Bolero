namespace Bolero.Tests.Web

open NUnit.Framework
open OpenQA.Selenium
open FsCheck.NUnit
open FsCheck
open Swensen.Unquote
open Bolero.Tests

/// Server remote calls.
[<Category "Remoting"; NonParallelizable>]
module Remoting =

    let elt = NodeFixture(By.Id "test-fixture-remoting")

    [<Ignore "Remoting test randomly fails; TODO fix">]
    [<Property(MaxTest = 10); NonParallelizable>]
    let ``Set and remove key`` (Alphanum k) (Alphanum v) =
        let keyInp = elt.ByClass("key-input")
        let valInp = elt.ByClass("value-input")
        let addBtn = elt.ByClass("add-btn")
        let remBtn = elt.ByClass("rem-btn")
        keyInp.Clear()
        // Add an "End" key so that some input is sent
        // even if the string is empty, to ensure that oninput is triggered
        keyInp.SendKeys(k + Keys.End)
        valInp.Clear()
        valInp.SendKeys(v + Keys.End)
        $"{k} => {v}" @| [
            "remove" @| (
                remBtn.Click()
                testNotNull <@ elt.Wait(fun () -> elt.ByClass("output-empty")) @>
            )
            "add" @| (
                addBtn.Click()
                test <@ elt.Wait(fun () -> elt.ByClass("output")).Text = v @>
            )
        ]

    [<Test; NonParallelizable>]
    let ``Authorized remote function succeeds when signed in`` () =
        let username = elt.ByClass("signin-input")
        username.Clear()
        username.SendKeys("someone")
        elt.ByClass("signin-button").Click()
        elt.Eventually <@ elt.ByClass("is-signedin").Text = "someone" @>

    [<Test; NonParallelizable>]
    let ``Authorized remote function fails when signed out`` () =
        elt.ByClass("signout-button").Click()
        elt.Eventually <@ elt.ByClass("is-signedin").Text = "<not logged in>" @>

    [<Test; NonParallelizable>]
    let ``Authorized remote function fails if role is missing`` () =
        let username = elt.ByClass("signin-input")
        username.Clear()
        username.SendKeys("someone")
        elt.ByClass("signin-button").Click()
        elt.Eventually <@ elt.ByClass("is-signedin").Text = "someone" @>
        elt.ByClass("get-admin").Click()
        elt.Eventually <@ elt.ByClass("is-admin").Text = "<not admin>" @>

    [<Test; NonParallelizable>]
    let ``Authorized remote function succeeds if role is present`` () =
        let username = elt.ByClass("signin-input")
        username.Clear()
        username.SendKeys("admin")
        elt.ByClass("signin-button").Click()
        elt.Eventually <@ elt.ByClass("is-signedin").Text = "admin" @>
        elt.ByClass("get-admin").Click()
        elt.Eventually <@ elt.ByClass("is-admin").Text = "admin ok" @>

    // TODO: should these tests be elsewhere?
    open System
    open Bolero.Remoting

    let unwrap (item : Result<_, string>) =
        match item with
        | Ok item -> item
        | Error err -> failwith err

    let isOk = function Ok _ -> true | Error _ -> false

    // TODO: Belongs elsewhere?
    [<Test>]
    let ``Serializing a record should result in a well formed query`` () =
        let testRecord : {|  AnInteger: int; OptionalData: Option<string> ; SomeData: string |} =
            {|  AnInteger = 20; OptionalData = Some "Optional data"; SomeData = "Some ^|^ data" |}
        let result = QueryStringSerializer.serialize(testRecord)
        Assert.True(isOk result)
        let result = unwrap result
        let expected = """?AnInteger=20&OptionalData=Optional+data&SomeData=Some+%5E%7C%5E+data"""
        Assert.AreEqual(expected, result)

    [<Test>]
    let ``Serializing a record with None value should result in a well formed query`` () =
        let testRecord : {|  AnInteger: int; OptionalData: Option<string> ; SomeData: string |} =
            {|  AnInteger = 20; OptionalData = None; SomeData = "Some ^|^ data" |}
        let result = QueryStringSerializer.serialize(testRecord)
        Assert.True(isOk result)
        let result = unwrap result
        let expected = """?AnInteger=20&SomeData=Some+%5E%7C%5E+data"""
        Assert.AreEqual(expected, result)

    [<Test>]
    let ``Serializing a unit value should result in a well formed query`` () =
        let result = QueryStringSerializer.serialize(())
        Assert.True(isOk result)
        let result = unwrap result
        Assert.AreEqual(String.Empty, result)

    [<Test>]
    let ``Serializing a record with a non primitive should return Error`` () =
        let testRecord = {| SomeData = "Some ^|^ Data"; NonPrimitive = Uri("http://google.com") |}
        let result = QueryStringSerializer.serialize(testRecord)
        Assert.False(isOk result)

    [<Test>]
    let ``Serializing a non-record type should return Error`` () =
        let testType = Uri("http://google.com")
        let result = QueryStringSerializer.serialize(testType)
        Assert.False(isOk result)

    [<Test>]
    let ``Deserializing from a well formed query string should succeed`` () =
        let result =
            QueryStringSerializer.deserialize<{| AnInteger: int; OptionalData: Option<string> ; SomeData: string |}>
                ("""?AnInteger=20&OptionalData=Optional+data&SomeData=Some+%5E%7C%5E+data""")
        Assert.True(isOk result)
        let result = unwrap result
        let expected : {|  AnInteger: int; OptionalData: Option<string> ; SomeData: string |} =
            {|  AnInteger = 20; OptionalData = Some "Optional data"; SomeData = "Some ^|^ data" |}
        Assert.AreEqual(expected, result)

    [<Test>]
    let ``Deserializing to a record with a None value from a well formed query string should succeed`` () =
        let result =
            QueryStringSerializer.deserialize<{| AnInteger: int; OptionalData: Option<string>; SomeData: string |}>
                ("""?AnInteger=20&SomeData=Some+%5E%7C%5E+data""")
        Assert.True(isOk result)
        let result = unwrap result
        let expected : {|  AnInteger: int; OptionalData: Option<string> ; SomeData: string |} =
            {|  AnInteger = 20; OptionalData = None; SomeData = "Some ^|^ data" |}
        Assert.AreEqual(expected, result)

    [<Test>]
    let ``Deserializing a unit value from a well formed query string should succeed`` () =
        let result = QueryStringSerializer.deserialize<unit>(String.Empty)
        Assert.True(isOk result)
        let result = unwrap result
        Assert.AreEqual((), result)