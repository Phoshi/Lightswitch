[<AutoOpen>]
module Lightswitch.Queue 
open System.IO
open System.Runtime.Serialization.Formatters.Binary

open RabbitMQ.Client
open RabbitMQ.Client.Events

type Queue =
| StandardCommand
| Query
| Response

type private QueueConnection = {connection: IConnection; model: IModel; queueName: Queue -> string}

let private resolve queue =
    match queue with
    | StandardCommand -> "standard"
    | Query -> "query"
    | Response -> "response"

let private bytes message =
    let formatter = BinaryFormatter()
    use stream = new MemoryStream()
    formatter.Serialize(stream, message)
    stream.ToArray()

let private fromBytes<'T> (message: byte[]) =
    let formatter = BinaryFormatter()
    use stream = new MemoryStream()
    stream.Write(message, 0, message.Length)
    stream.Seek(0L, SeekOrigin.Begin) |> ignore
    unbox<'T>(formatter.Deserialize(stream))
    
let private declare (channel: IModel) queueName =
    channel.QueueDeclare(queueName, true, false, false, null)

let private queueName (model: IModel) queue =
    let queueName = queue |> resolve
    declare model queueName |> ignore
    queueName

let private exchange = ""

let private factory = 
    let host = Settings.QueueHost
    let port = Settings.QueuePort
    let username = Settings.QueueUser
    let password = Settings.QueuePassword
        
    ConnectionFactory(HostName = host, Port = port, UserName = username, Password = password)

let private connectionModel()=
    let connection = factory.CreateConnection()
    let model = connection.CreateModel()
    {connection = connection; model = model; queueName = (queueName model)}

let private disposable qc =
    {new System.IDisposable with 
        member x.Dispose() = 
            match qc with
                {model = model; connection = connection} -> model.Close(); connection.Close()}

let enqueue queue message =
    let serialised = message |> bytes
    let connection = connectionModel()
    use l = disposable connection

    match connection with
    | {model = model; queueName = queueName} -> model.BasicPublish(exchange, queueName queue, null, serialised)

let subscribe<'T> queue f =
    let queueConnection = connectionModel()
    match queueConnection with
    | {model = model; connection = connection; queueName = queneName} ->
        let consumer = EventingBasicConsumer(model)

        consumer.Received.Add((fun msg -> msg.Body |> fromBytes<'T> |> f (disposable queueConnection)))

        model.BasicConsume(queueName model queue, true, consumer) |> ignore

        disposable queueConnection