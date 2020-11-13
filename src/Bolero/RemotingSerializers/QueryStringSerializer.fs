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

let internal areConstraintsSatisfied (recordType: Type) (props: PropertyInfo array) =
    let satisfied =
        props
        |> Array.forall (fun prop ->
            prop.PropertyType.IsPrimitive || prop.PropertyType = typeof<string>
        )
    if satisfied then Ok props
    else Error (sprintf "All properties of type '%s' must be primitives." recordType.AssemblyQualifiedName)

// TODO: Purely for organization, could be moved up?
module private Serialize =
    // TODO: Could be replaced with Microsoft.AspNetCore.Http.QueryString. Will that play nice with Blazor?
    let toQueryString (vals: (string * string) seq) =
        vals
        |> Seq.map (fun (param, value) ->
            sprintf "%s=%s" (WebUtility.UrlEncode(param)) (WebUtility.UrlEncode(value)))
        |> String.concat "&"
        |> sprintf "?%s"

    let extractPropertyValues (instance: obj) (props: PropertyInfo array) =
        props
        |> Array.map (fun prop ->
            (prop.Name, string (prop.GetValue(instance)))
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
                    Convert.ChangeType(valueMap.[prop.Name], prop.PropertyType)
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