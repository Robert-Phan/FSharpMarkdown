namespace Markdown

module Markdown =
    [<AbstractClass>]
    type INode() = 
        abstract AsString: unit -> string
        override this.ToString() = this.AsString()

    [<AbstractClass>]
    type ValNode() = 
        inherit INode()
        override this.AsString() = this.Value
        abstract Value: string
    
    [<AbstractClass>]
    type RecNode(inner) =
        inherit INode()
        override this.AsString() = 
                this.Inner 
                |> List.map (fun node -> node.AsString()) 
                |> String.concat ""
                |> this.StringFormat
        
        abstract StringFormat: innerStr: string -> string
        member this.Inner: INode list = inner

    type RecPlain(inner) = 
        inherit RecNode(inner)
        override this.StringFormat innerStr = innerStr
        new() = RecPlain []
    
    type Emph(inner) =
        inherit RecNode(inner)
        override this.StringFormat innerStr = 
            $"<em>{innerStr}</em>"
        
        new(p: RecNode) = Emph p.Inner
    
    type StrongEmph(inner) =
        inherit RecNode(inner)
        override this.StringFormat innerStr = 
            $"<strong>{innerStr}</strong>"

        new(p: RecNode) = StrongEmph p.Inner

    type Plain(baseText) =
        inherit ValNode() 
        override this.Value = baseText

    [<AbstractClass>]
    type DelNode() =
        inherit ValNode()

    [<AbstractClass>]
    type AstOrUnd(n) = 
        inherit DelNode()
        override this.Value = String.replicate this.Length this.Char

        member this.Length = n
        abstract Char: string
    
    type Asterisk(n) =
        inherit AstOrUnd(n)
        override this.Char = "*"

    type Underscore(n) =
        inherit AstOrUnd(n)
        override this.Char = "_"
        
    type DelimeterPotential =
        | PotentialOpener
        | PotentialCloser
        | Both
    
    type Delimiter = {
        Node: DelNode
        Active: bool
        Potential: DelimeterPotential
    }

    let rec makeDelListHelper (delList: list<Delimiter>) (nodeList: list<INode>) = 
        let getDelNode (node: INode) = 
            match node with
            | :? DelNode as del -> 
                [{  Node = del
                    Active = false
                    Potential = Both }]
            | _ -> []

        if nodeList.IsEmpty then
            delList
        else
            let node = nodeList.Head
            let newDelList = 
                delList @ getDelNode node
            
            makeDelListHelper newDelList nodeList.Tail
    
    // TODO this also
    let adjustPotentials (delList: Delimiter list) = 
        delList
    
    let makeDelList = makeDelListHelper [] >> adjustPotentials

    let rec findMatchingOpener 
        (closerNode: AstOrUnd) 
        (bottoms : int * int)
        (delList: Delimiter list)
        (idx: int) =
            let bot1, bot2 = bottoms
            if idx < 0 || idx < bot1 || idx < bot2 then None else 

            let potOpener = delList[idx]

            if closerNode.GetType() = potOpener.Node.GetType() then
                delList[idx] |> Some
            else 
                findMatchingOpener closerNode bottoms delList (idx-1)

            // match closer, potOpener.Node with
            // | :? Asterisk, :? Asterisk -> delList[idx] |> Some
            // | :? Underscore, :? Underscore -> delList[idx] |> Some
            // | _ -> findMatchingOpener closer delList (idx-1)
    
    exception OpenerCloserException

    let rec processEmphasisHelper 
        (stackBot: int) 
        (openersBot: int) 
        (curPos: int) 
        (nodeList: INode list)
        (delList: Delimiter list) = 
            if curPos >= delList.Length then nodeList else

            let del = delList[curPos]

            match del.Node with
            | :? AstOrUnd as closerNode -> 
                let idx = curPos - 1
                let potOpener = findMatchingOpener closerNode (stackBot, openersBot) delList idx
                
                match potOpener with
                | None -> 
                    let openersBot = curPos - 1
                    let nextPos = curPos + 1
                    processEmphasisHelper stackBot openersBot nextPos
                        nodeList delList
                | Some opener -> 
                    let closer = del
                    openerFound (stackBot, openersBot) closer opener curPos nodeList delList
            | _ ->
                let nextPos =  curPos + 1
                processEmphasisHelper stackBot openersBot nextPos
                    nodeList delList
    and openerFound
        (bottoms: int * int)
        (closer: Delimiter) 
        (opener: Delimiter)
        (curPos: int)
        (nodeList: INode list)
        (delList: Delimiter list) = 
            let mutable curPos = curPos

            let isStrongEmph = 
                match closer.Node, opener.Node with
                | (:? AstOrUnd as a), (:? AstOrUnd as b) 
                    when a.Length >= 2 && b.Length >= 2
                    -> true
                | _ -> false

            let createEmphNode (inner: INode list): RecNode  =
                if isStrongEmph then StrongEmph inner else Emph inner

            let splitNodeList = 
                let openerI = nodeList |> List.findIndex (fun text -> text = opener.Node)
                let closerI = nodeList |> List.findIndex (fun text -> text = closer.Node)
                nodeList[0..openerI-1], nodeList[openerI+1..closerI-1], nodeList[closerI+1..]

            let before, middle, after = splitNodeList
            let emphNode = middle |> createEmphNode

            let removeDelLen = if isStrongEmph then -2 else -1
            let makeNewDelNode (delNode: DelNode) : AstOrUnd option = 
                match delNode with
                | :? Underscore as node -> 
                    let newLen = node.Length + removeDelLen
                    if newLen = 0 then None else 
                    Underscore newLen :> AstOrUnd |> Some
                | :? Asterisk as node -> 
                    let newLen = node.Length + removeDelLen
                    if newLen = 0 then None else 
                    Asterisk newLen :> AstOrUnd |> Some
                | _ -> raise OpenerCloserException
            
            let makeNewDelimeter (del: Delimiter) = 
                let potNewDelNode = makeNewDelNode del.Node

                potNewDelNode |> Option.map (fun delnode -> 
                    { Node = delnode
                      Active = del.Active
                      Potential = del.Potential })
            
            let delimeterToList del = 
                [del |> Option.map (fun del -> del.Node :> INode)] |> List.choose id
            
            let potNewOpener = makeNewDelimeter opener 
            let potNewCloser = makeNewDelimeter closer
            let before = before @ delimeterToList potNewOpener
            let after = delimeterToList potNewCloser @ after

            let newNodeList = before @ [emphNode] @ after

            let openerDelI = delList |> List.findIndex (fun text -> text = opener)
            let closerDelI = delList |> List.findIndex (fun text -> text = closer)

            let rec makeNewDelList 
                (curPos : int)
                (idxDelList : (int * Delimiter) list) =
                if idxDelList.IsEmpty then [], curPos else
                
                let newCurPos = curPos - 1
                let tail = idxDelList.Tail
                let idx, del = idxDelList.Head

                if idx > openerDelI && idx < closerDelI then
                    makeNewDelList newCurPos tail 
                else if idx = openerDelI then
                    match potNewOpener with
                    | None -> makeNewDelList newCurPos tail 
                    | Some opener -> 
                        let finTail, finCurPos = makeNewDelList curPos tail
                        opener :: finTail, finCurPos
                else if idx = closerDelI then
                    match potNewCloser with
                    // if closing node removed then curPos shifts to the next element
                    // but the elements afterwards shifts back once
                    // so no change is made to curPos
                    | None -> makeNewDelList curPos  tail
                    | Some closer -> 
                        let finTail, finCurPos = makeNewDelList curPos tail
                        closer :: finTail, finCurPos
                else 
                    let finTail, finCurPos = makeNewDelList curPos tail
                    del :: finTail, finCurPos
            
            let newDelList, newCurPos = delList |> List.indexed |> makeNewDelList curPos
            let stackBot, openersBot = bottoms

            processEmphasisHelper stackBot openersBot newCurPos newNodeList newDelList

    let processEmphasis nodeList = 
        let delList = makeDelList nodeList
        processEmphasisHelper -1 -1 0 nodeList delList
