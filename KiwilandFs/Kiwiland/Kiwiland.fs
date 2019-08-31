[<AutoOpen>]
module Kiwiland.Core

type City = private T of string
module City =
    let name (T name) = name
    let create name = T name

type Link = private T of origin:City * destination:City
module Link = 
    let origin (T (origin, _)) = origin
    let destination (T (_, destination)) = destination
    let create origin destination = T (origin, destination)

type Distance = private T of int
module Distance =
    let ofKm n = T n
    let toKm (T n) = n

type Route = private T of origin:City * stops: (City * Distance) list
module Route =
    let origin (T (origin, _)) = origin
    let destination (T (_, stops)) = if Seq.isEmpty stops then None else fst (Seq.last stops) |> Some
    let stops (T (_, stops)) = Seq.map (fun (city, _) -> city) stops
    let cities r = Seq.append (Seq.singleton (origin r)) (stops r)
    let lastCity r = Seq.last (cities r)
    let links r = (Seq.windowed 2 (cities r)) |> Seq.map (fun x -> Link.create x.[0] x.[1])
    let distance (T (_, stops)) = Seq.sumBy (fun (_, distance) -> Distance.toKm distance) stops
    let numberOfStops r = Seq.length (stops r)
    let length r = numberOfStops r + 1
    let create origin = T (origin, [])
    let extend city distance (T (origin,stops)) = 
        T (origin, stops@[(city, distance)])

type Guard = private LengthIsLessOrEqualTo of int | DistanceIsLessThan of int | Acyclic
module Guard = 
    let lengthIsLessOrEqualTo x = LengthIsLessOrEqualTo x
    let distanceIsLessThan x = DistanceIsLessThan x
    let acyclic = Acyclic

    let eval guard route whenFulfilled whenNotFulfilled = 
        let result =
            match guard with
            | LengthIsLessOrEqualTo x -> (Route.length route) <= x
            | DistanceIsLessThan x -> (Route.distance route) < x
            | Acyclic ->
                let links = route |> Route.links |> Seq.rev
                if not <| Seq.isEmpty links then
                    let head, tail = Seq.head links, Seq.tail links
                    Seq.tryFind(fun l -> l = head) tail |> Option.isNone
                else true

        if result then whenFulfilled route else whenNotFulfilled

type Graph = private T of seq<Link * Distance>
module Graph = 
    let empty = T (Seq.empty<Link * Distance>)
    let connect origin destination distance (T elements) = 
        let link = Link.create origin destination
        T (Seq.append elements (Seq.singleton (link, distance)))

    let traverse origin guard (T graph) =
        let rec traverse route guard graph =
            let elements = Seq.filter (fun (link, _) -> Link.origin link = Route.lastCity route) graph
            let foldFunc routes (link, distance) =
                let whenTrue r = Seq.append (Seq.append routes (Seq.singleton r)) (traverse r guard graph)
                let whenFalse = Seq.empty
                Guard.eval guard (Route.extend (Link.destination link) distance route) whenTrue whenFalse

            Seq.fold foldFunc Seq.empty elements

        traverse (Route.create origin) guard graph
