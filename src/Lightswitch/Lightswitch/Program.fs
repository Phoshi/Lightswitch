// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module Lightswitch.Main

open System

let rec handle (client: Client) _ (message: Command) = 
    let rec handle' msg submit = match msg with 
        | On -> Commands.id() |> Commands.on |> submit
        | Off -> Commands.id() |> Commands.off |> submit
        | Colour(RGB(r, g, b)) -> Commands.id() |> Commands.colour r g b |> submit
        | Brightness b -> Commands.id() |> Commands.brightness b |> submit
        | SpecificLight(light, command) -> (handle' command (client.submitForLight light))
        | Many(c1, c2) -> async {
                let! r1 = (handle' c1 submit) 
                let! r2 = (handle' c2 submit)

                return r2
            }
    handle' message client.submit |> Async.RunSynchronously |> ignore

let handleQuery (client: Client) _ (message: Command) =
    match message with
        | List -> enqueue Response ((client.lights()) |> Async.RunSynchronously |> List.map (fun l -> l.Name))
        | _ -> ()

let main() = 
        let client = Client.make
        let processCommandQueue = subscribe<Command> StandardCommand (handle client)
        let processQueryQueue = subscribe<Command> Query (handleQuery client)
        let rec wait ()=
            System.Threading.Thread.Sleep(System.Int32.MaxValue)
            wait()
        wait()


main() |> ignore
