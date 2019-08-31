namespace Kiwiland

module Formatter =
    type Formattable = City of City | Route of Route

    let private format = function
        |City c -> City.name c
        |Route r -> r |> Route.cities |> Seq.map City.name |> String.concat " -> "

    let route = Route >> format
    let city = City >> format

module Cli = 
    let printRoute = Formatter.route >> printfn "%s"
    let printRoutes = function
    | Some routes -> routes |> Seq.iter (Formatter.route >> printfn "%s")
    | None -> printfn "No routes found!"

module Program =
    let Malmo = City.create "Malmö"
    let Helsignborg = City.create "Helsingborg"
    let Gothemburg = City.create "Göteborg"

    let graph = 
        Graph.empty
        |> Graph.connect Malmo Gothemburg (Distance.ofKm 2)
        |> Graph.connect Malmo Helsignborg (Distance.ofKm 3)
        |> Graph.connect Helsignborg Gothemburg (Distance.ofKm 2)
        |> Graph.connect Gothemburg Malmo (Distance.ofKm 5)
            
    let run () =
        let routes = Graph.traverse Malmo (Guard.lengthIsLessOrEqualTo 3) graph
        Cli.printRoutes (Some routes)

    [<EntryPoint>]
    let main argv =
        run ()
        0 // return an integer exit code