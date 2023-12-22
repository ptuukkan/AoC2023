namespace AoC2023

open System
open System.Collections.Generic
open FSharpx.Collections

module Day20 =
    type 'a RoseTree =
        | Leaf of 'a
        | Node of 'a * 'a RoseTree list

    type Pulse =
        | High
        | Low

    type ModuleType =
        | FlipFlop of bool
        | Conjunction of (string * Pulse) list
        | Broadcaster
        | Button

    type Module =
        { Label: string
          Type: ModuleType
          Connections: string list }

    let parse (input: string) =
        let split = input.Split("->", StringSplitOptions.TrimEntries)

        let connections =
            split[1].Split(",", StringSplitOptions.TrimEntries) |> List.ofArray

        match split[0] with
        | "broadcaster" ->
            "broadcaster",
            { Label = "broadcaster"
              Type = Broadcaster
              Connections = connections }
        | _ ->
            match split[0].Substring(0, 1) with
            | "%" ->
                split[0].Substring(1),
                { Label = split[0].Substring(1)
                  Type = FlipFlop(false)
                  Connections = connections }
            | _ ->
                split[0].Substring(1),
                { Label = split[0].Substring(1)
                  Type = Conjunction([])
                  Connections = connections }

    let addConnections (modules: Map<string, Module>) =
        let conjunctionModules =
            modules.Values
            |> Seq.filter (fun x -> x.Type = Conjunction([]))
            |> Seq.map (fun m ->
                let connected =
                    modules.Values
                    |> Seq.filter (fun x -> x.Connections |> List.contains m.Label)
                    |> Seq.map (fun x -> x.Label, Low)
                    |> List.ofSeq

                { m with Type = Conjunction(connected) })

        (modules, conjunctionModules)
        ||> Seq.fold (fun mods conj -> mods.Change(conj.Label, (fun p -> Some(conj))))

    let processPulse (m: Module) (pulse: Pulse) (from: string) : Module * (string * string * Pulse) list =
        match m.Type with
        | FlipFlop b ->
            match pulse with
            | High -> m, []
            | Low ->
                if b then
                    { m with Type = FlipFlop(false) }, m.Connections |> List.map (fun c -> m.Label, c, Low)
                else
                    { m with Type = FlipFlop(true) }, m.Connections |> List.map (fun c -> m.Label, c, High)

        | Conjunction memory ->
            let updatedMemory =
                memory
                |> List.map (fun (label, value) -> if label = from then label, pulse else label, value)

            if updatedMemory |> List.forall (fun (label, value) -> value = High) then

                { m with
                    Type = Conjunction(updatedMemory) },
                m.Connections |> List.map (fun c -> m.Label, c, Low)
            else
                { m with
                    Type = Conjunction(updatedMemory) },
                m.Connections |> List.map (fun c -> m.Label, c, High)
        | Broadcaster -> m, m.Connections |> List.map (fun c -> m.Label, c, pulse)
        | Button -> m, m.Connections |> List.map (fun c -> m.Label, c, Low)

    let rec loop (queue: Queue<string * string * Pulse>) (modules: Map<string, Module>) (processed: Pulse list) =
        match queue |> Queue.tryUncons with
        | None -> processed |> List.partition (fun x -> x = Low), modules
        | Some((src, dst, pulse), q) ->
            if (modules |> Map.containsKey dst) then
                let new_m, messages = processPulse modules[dst] pulse src
                let new_modules = modules |> Map.change dst (fun x -> Some(new_m))

                let new_queue =
                    (q, messages) ||> List.fold (fun new_q message -> new_q |> Queue.conj message)

                loop new_queue new_modules (pulse :: processed)
            else
                loop q modules (pulse :: processed)

    let pushButton (modules: Map<string, Module>) =
        let queue =
            Queue.empty<string * string * Pulse>
            |> Queue.conj ("button", "broadcaster", Low)

        loop queue modules []

    let getCycles (modules: Map<string, Module>) =
        let mutable mods = modules
        let mutable cycles = 1
        let mutable flipFlops = Dictionary<string, int>()

        let flipFlopCount =
            modules.Values |> Seq.filter (fun x -> x.Type = FlipFlop(false)) |> Seq.length

        while flipFlops.Count <> flipFlopCount do
            let (_l, _h), new_modules = pushButton mods
            mods <- new_modules

            new_modules.Values
            |> Seq.filter (fun x -> x.Type = FlipFlop(true))
            |> Seq.iter (fun x ->
                if flipFlops.ContainsKey(x.Label) |> not then
                    flipFlops.Add(x.Label, cycles)
                    ())

            cycles <- cycles + 1

        flipFlops

    let rec resolveTarget (modules: Map<string, Module>) (target: string) =
        if modules.ContainsKey(target) then
            match modules.Item(target).Type with
            | FlipFlop _ -> Leaf(modules.Item(target).Label)
            | _ ->
                let children =
                    modules.Values
                    |> Seq.filter (fun x -> x.Connections |> List.contains target)
                    |> Seq.map (fun x -> resolveTarget modules x.Label)
                    |> List.ofSeq

                Node(modules.Item(target).Label, children)
        else
            modules.Values
            |> Seq.filter (fun x -> x.Connections |> List.contains target)
            |> Seq.map (fun x -> resolveTarget modules x.Label)
            |> List.ofSeq
            |> (fun children -> Node(target, children))

    let rec calculateCycles (flipflops: Dictionary<string, int>) (tree: string RoseTree) =
        match tree with
        | Leaf s -> uint64 flipflops[s]
        | Node(s, roseTrees) ->
            if
                roseTrees
                |> List.forall (fun x ->
                    match x with
                    | Leaf s -> true
                    | Node(s, roseTrees) -> false)
            then
                roseTrees |> List.map (calculateCycles flipflops) |> List.sum
            else
                roseTrees |> List.map (calculateCycles flipflops) |> List.reduce (*)

    let part1 (input: string seq) =
        let modules =
            input
            |> Seq.map parse
            |> Map.ofSeq
            |> (_.Add(
                "button",
                { Label = "button"
                  Type = Button
                  Connections = [ "broadcaster" ] }
            ))
            |> addConnections

        ((0, 0, modules), [ 1..1000 ])
        ||> List.fold (fun (lows, highs, m) _i ->
            let (l, h), new_modules = pushButton m
            lows + l.Length, highs + h.Length, new_modules)
        |> (fun (lows, highs, _m) -> lows * highs)

    let part2 (input: string seq) =
        let modules =
            input
            |> Seq.map parse
            |> Map.ofSeq
            |> (_.Add(
                "button",
                { Label = "button"
                  Type = Button
                  Connections = [ "broadcaster" ] }
            ))
            |> addConnections

        resolveTarget modules "rx" |> calculateCycles (getCycles modules)
