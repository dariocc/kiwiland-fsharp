namespace Kiwiland

open NUnit.Framework
open Swensen.Unquote
open Kiwiland

module Exercise = 
    let (A, B, C, D, E) = City.create "A", City.create "B", City.create "C", City.create "D", City.create "E"
    let graph = 
        Graph.empty
        |> Graph.connect A B (Distance.ofKm 5)
        |> Graph.connect B C (Distance.ofKm 4)
        |> Graph.connect C D (Distance.ofKm 8)
        |> Graph.connect D C (Distance.ofKm 8)
        |> Graph.connect D E (Distance.ofKm 6)
        |> Graph.connect A D (Distance.ofKm 5)
        |> Graph.connect C E (Distance.ofKm 2)
        |> Graph.connect E B (Distance.ofKm 3)
        |> Graph.connect A E (Distance.ofKm 7)

    type RouteQueryResult = Ok of int | NoSuchRoute

    let findRoutes origin filter guard graph = 
        let routes = Graph.traverse origin guard graph |> Seq.filter filter 
        if not <| Seq.isEmpty routes then Some routes else None

    let routeDistance routeCities graph =
        let compare a b = (Seq.toList a) = (Seq.toList b)
        let ``cities are`` x r = compare (Route.cities r) x

        let origin = Seq.head routeCities
        let citiesLength = Seq.length routeCities
        let routes = findRoutes origin (``cities are`` routeCities) (Guard.lengthIsLessOrEqualTo citiesLength) graph 
        match  routes with
        | Some routes -> Seq.head routes |> Route.distance |> Ok
        | None -> NoSuchRoute

    let shortestDistance origin destination graph =
        let ``destination is`` d r = Route.destination r = Some d
        let routes = findRoutes origin (``destination is`` destination) Guard.acyclic graph
        match routes with
        | Some routes -> routes |> Seq.map Route.distance |> Seq.min |> Ok
        | None -> NoSuchRoute

    let numberOfRoutes origin filter guard graph =
        let routes = findRoutes origin filter guard graph
        match routes with
        | Some routes -> Seq.length routes |> Ok
        | None -> NoSuchRoute

    [<Test>]
    let ``1. The distance of the route ABC is 9`` () =
        test <@ routeDistance ([A;B;C] |> Seq.ofList) graph = Ok 9 @>

    [<Test>]
    let ``2. The distance of the route AD is 5`` () = 
        test <@ routeDistance ([A;D] |> Seq.ofList) graph = Ok 5 @>

    [<Test>]
    let ``3. The distance of the route ADC is 13`` () =
        test <@ routeDistance ([A;D;C] |> Seq.ofList) graph = Ok 13 @>

    [<Test>]
    let ``4. The distance of the route AEBCD is 22`` () = 
        test <@ routeDistance ([A;E;B;C;D] |> Seq.ofList) graph = Ok 22 @>

    [<Test>]
    let ``5. The distance of the route AED is `NO SUCH ROUTE` `` () =
        test <@ routeDistance ([A;E;D] |> Seq.ofList) graph = NoSuchRoute @>

    [<Test>]
    let ``6. The number of trips starting at C and ending at C with a maximum of 3 stops is 2 (CDC and CEBC)`` () = 
        let ``ending in C with maximum of 3 stops`` route = (Route.destination route = Some C) && (Route.numberOfStops route <= 3)
        test <@ numberOfRoutes A ``ending in C with maximum of 3 stops`` (Guard.lengthIsLessOrEqualTo 3) graph = Ok 2 @>

    [<Test>]
    let ``7. The number of trips starting at A and ending at C with exactly 4 stops is 3 (ABCDC, ADCDC and ADEBC`` () = 
        let ``ending in C with exactly 4 stops`` route = (Route.destination route = Some C) && (Route.numberOfStops route = 4)
        test <@ numberOfRoutes A ``ending in C with exactly 4 stops`` (Guard.lengthIsLessOrEqualTo 5) graph = Ok 3 @>

    [<Test>]
    let ``8. The length of the shortest route (in terms of distance to travel) from A to C is 9.`` () =
        test <@ shortestDistance A C graph = Ok 9 @>

    [<Test>]
    let ``9. The length of the shortest route (in terms of distance to travel) from B to B is 9.`` () =
        test <@ shortestDistance B B graph = Ok 9 @>

    [<Test>]
    let ``10. The number of different routes from C to C with a distance of less than 30 is 7 (CDC, CEBC, CEBCDC, CDCEBC, CDEBC, CEBCEBC, CEBCEBCEBC)`` () =
        let ``ending in C`` route = Route.destination route = Some C
        test <@ numberOfRoutes C ``ending in C`` (Guard.distanceIsLessThan 30) graph = Ok 7 @>