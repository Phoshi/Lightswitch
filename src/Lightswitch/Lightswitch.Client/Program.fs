// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Lightswitch
open System.Text.RegularExpressions

let (|On|_|) t = match t with 
    | "on" -> Some On
    | _ -> None
let (|Off|_|) t = match t with
    | "off" -> Some Off
    | _ -> None

let (|Colour|_|) input =
    let parse n = System.Int32.Parse(n, System.Globalization.NumberStyles.HexNumber)
    let m = Regex.Match(input, @"#([0-9A-F]{2})([0-9A-F]{2})([0-9A-F]{2})")
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ] |> List.map parse)
    else None



let (|Brightness|_|) input =
    match System.Int32.TryParse(input) with
    | (true, brightness) -> Some brightness
    | _ -> None



let rec parseCommand command =
    let (|Specific|_|) input = 
        let m = Regex.Match(input, "^(.*)@(.*)$")
        if m.Success then match List.tail [for g in m.Groups -> g.Value] with
            | [light; command] -> Some(light, parseCommand command)
        else None
    let (|Many|_|) (input: string) = 
        let split = input.Split [|'|'|]
        match split |> Seq.toList with
        | [single] -> None
        | [] -> None
        | lst -> lst |> List.map parseCommand |> Some
    match command with
    | Specific(light, command) -> match command with 
        | Some c -> Some (SpecificLight(light, c))
    | Many commands -> 
        let fold' c1 c2 = match c1 with
            | Some c -> Some(Many(c, c2))
            | None -> Some(c2)
        commands |> List.choose id |> List.fold fold' None
    | On -> Some On
    | Off -> Some Off
    | Colour [r;g;b] -> Some (Colour(RGB (r, g, b)))
    | Brightness b -> Some (Brightness b)
    | "list" -> Some List
    | _ -> None

let rec prepareCommand commands = match commands |> List.choose id with
    | [command] -> Some command
    | [left; right] -> Some(Many(left, right))
    | head::tail -> match prepareCommand (tail |> List.map Some) with 
                        | Some command -> Some(Many(head, command))
                        | None -> Some head
    | _ -> None

let consume (fn: System.IDisposable) v = 
    printf "%A" v
    fn.Dispose()

let await queue = 
    subscribe queue consume |> ignore

let executeCommand command = match command with 
    | Some List -> 
        enqueue Query List
        await Response
    | Some cmd -> enqueue StandardCommand cmd
    | None -> ()

[<EntryPoint>]
let main argv = 
    argv |> Array.toList |> List.map parseCommand |> prepareCommand |> executeCommand
    0
