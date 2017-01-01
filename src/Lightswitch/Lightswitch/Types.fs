[<AutoOpen>]
module Lightswitch.Types

open Q42.HueApi

type Settings = FSharp.Configuration.AppSettings<"app.config">

type Client = { 
    submit: (LightCommand -> Async<Models.Groups.HueResults>);
    submitForLight: (string -> LightCommand -> Async<Models.Groups.HueResults>);
    lights: (unit -> Async<list<Light>>);
    }
    
type Colour = RGB of int * int * int
type Command = 
    | On 
    | Off 
    | Colour of Colour
    | Brightness of int
    | Many of Command * Command
    | SpecificLight of string * Command
    | List