module Bolero.Remoting.QueryStringSerializer

open System
open System.Reflection
open FSharp.Reflection
open System.Net

let internal getRecordFields (recordType: Type) =
    if FSharpType.IsRecord(recordType) then
        recordType
        |> FSharpType.GetRecordFields
        |> Ok
    else
        Error "Only records/unit are supported for query string (de)serialization."

let internal isOptionType (typ: Type) =
    typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<Option<_>>

let internal isTypeSerializable (typ: Type) =
    let rec checkType (checkingOptionCase: bool) (typ: Type) =
        if typ.IsPrimitive || typ = typeof<string> then
            true
        // keeping code block like this for clarity (hopefully)
        // fsharplint:disable-next-line Hints
        else if
            // don't want to return a false positive on a type like Option<Option<string>>
            not checkingOptionCase
            && isOptionType typ
            && checkType true (typ.GetGenericArguments().[0]) then
            true
        else
            false
    checkType false typ

let internal areConstraintsSatisfied (recordType: Type) (props: PropertyInfo array) =
    let satisfied =
        props
        |> Array.map (fun prop ->
            (prop, isTypeSerializable prop.PropertyType)
        )
    if Array.forall (snd >> (=) true) satisfied then
        Ok props
    else
        satisfied
        |> Array.filter (snd >> (=) false)
        |> Array.map (fun (prop, _) -> prop.Name)
        |> String.concat ", "
        |> sprintf "All properties of type '%s' must be primitives or string. Offending properties as follows: %s" recordType.AssemblyQualifiedName
        |> Error

// TODO: Purely for organization, could be moved up?
module private Serialize =
    // TODO: Could be replaced with Microsoft.AspNetCore.Http.QueryString. Will that play nice with Blazor?
    let toQueryString (vals: (string * string) seq) =
        vals
        |> Seq.map (fun (param, value) ->
            sprintf "%s=%s" (WebUtility.UrlEncode(param)) (WebUtility.UrlEncode(value)))
        |> String.concat "&"
        |> sprintf "?%s"

    let extractOptionValue (instance: obj) (prop: PropertyInfo) =
        let value = prop.GetValue(instance)
        if isNull value then
            None
        else
            FSharpValue.GetUnionFields(value, prop.PropertyType)
            |> snd
            |> Array.head
            |> Some

    let extractPropertyValues (instance: obj) (props: PropertyInfo array) =
        props
        |> Array.choose (fun prop ->
            if isOptionType prop.PropertyType then
                prop
                |> extractOptionValue instance
                |> Option.map (fun value -> (prop.Name, string value))
            else
                Some (prop.Name, string (prop.GetValue(instance)))
        )

    let serialize (toSerialize: obj) =
        let t = toSerialize.GetType()
        if t = typeof<unit> then
            Ok String.Empty
        else
            t
            |> getRecordFields
            |> Result.bind (areConstraintsSatisfied t)
            |> Result.map (extractPropertyValues toSerialize)
            |> Result.map toQueryString

// TODO: Purely for organization, could be moved up?
module private Deserialize =
    let fillPropertyValues<'T> (values: (string * string) array) (props: PropertyInfo array) =
        let t = typeof<'T>
        let valueMap = Map.ofArray values
        // we know that all values need to be accounted for in this case
        if FSharpType.IsRecord(t) then
            let recordFields =
                props
                |> Array.map (fun prop ->
                    match Map.tryFind prop.Name valueMap, isOptionType prop.PropertyType with
                    | Some value, true ->
                        let someCase =
                            FSharpType.GetUnionCases(prop.PropertyType)
                            |> Array.find (fun uc -> uc.Name = "Some")
                        FSharpValue.MakeUnion(someCase, [| Convert.ChangeType(value, prop.PropertyType.GetGenericArguments().[0]) |])
                    | Some value, false ->
                        Convert.ChangeType(value, prop.PropertyType)
                    | None, true ->
                        box None
                    | None, false ->
                        raise (System.Collections.Generic.KeyNotFoundException("The given key was not present in the dictionary."))
                )
            FSharpValue.MakeRecord(t, recordFields)
            :?> 'T
            |> Ok
        else
            Error "'T must be an F# Record type or unit!"

    // TODO: Could be replaced with Microsoft.AspNetCore.Http.QueryString. Will that play nice with Blazor?
    let fromQueryString (queryString: string) =
        queryString.TrimStart('?').Split('&')
        |> Array.map (fun pair ->
            let [| query; value |] = pair.Split('=')
            WebUtility.UrlDecode(query), WebUtility.UrlDecode(value)
        )

    let deserialize<'T>(queryString: string) =
        let t = typeof<'T>
        if t = typeof<unit> then
            // need to return default (resulting in `unit` anyway!) in order to keep the compiler happy
            Ok <| Unchecked.defaultof<'T>
        else if isNull queryString then
            Error "Query string cannot be null when the type is not unit."
        else
            let values = fromQueryString queryString
            t
            |> getRecordFields
            |> Result.bind (areConstraintsSatisfied t)
            |> Result.bind (fillPropertyValues<'T> values)

let serialize = Serialize.serialize
let deserialize<'T> = Deserialize.deserialize<'T>