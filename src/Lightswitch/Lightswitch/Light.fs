module public Lightswitch.Client

open Q42.HueApi

type private IP = IP of string
type private AppKey = AppKey of string

let private bridges = 
    let locator = new Q42.HueApi.HttpBridgeLocator();
    async {
        let! bridges = locator.LocateBridgesAsync (System.TimeSpan.FromSeconds 5.) |> Async.AwaitTask
        return bridges |> Seq.map IP
    }
    
let private appname = "kitten-control"
let private devicename = "raspberry"

let private appkey (client: LocalHueClient) = 
    async {
        let! appKey = client.RegisterAsync(appname, devicename) |> Async.AwaitTask
        return AppKey appKey;
    }

let private storedAppkey (client: LocalHueClient) = async{
    let contents = System.IO.File.ReadAllText appname
    client.Initialize(contents)
    return AppKey contents
}

let private register (client) = async {
    let! appkey = match System.IO.File.Exists(appname) with
        | true -> storedAppkey client
        | false -> appkey client

    match appkey with AppKey ak -> System.IO.File.WriteAllText(appname, ak)
    return client
}

let private client ip = match ip with IP ip -> new Q42.HueApi.LocalHueClient(ip) |> register

let private singleClient = async { 
    let! localBridges = bridges
    let! ourClient = localBridges |> Seq.map client |> Seq.head 
    return ourClient
}

let private lights (client: LocalHueClient) = 
    async {
        let! lights = client.GetLightsAsync() |> Async.AwaitTask
        
        return lights 
    }
    
let private lightid (light: Light) = light.Id;

let private submitCommand (client: LocalHueClient) lights command = async {
    return! client.SendCommandAsync(command, lights |> Seq.map lightid) |> Async.AwaitTask
}

let make: Lightswitch.Types.Client = { 
    submit = fun command -> async {
        let! client = singleClient
        let! lights = lights client

        return! submitCommand client lights command
    };
    submitForLight = fun lightName command -> async{
        let! client = singleClient
        let! lights = lights client
        let rec light' (xs: Light list) l = match xs with
            | head::tail when head.Name = l -> Some head
            | _::tail -> light' tail l
            | _ -> None

        let light = light' (lights |> Seq.toList) lightName

        return! match light with 
        | Some l ->
            submitCommand client [l] command
        | None -> raise (System.ArgumentException("No such light " + lightName))
    };
    lights = fun () -> async {
        let! client = singleClient
        let! lights = lights client

        return lights |> Seq.toList
    } 
}

