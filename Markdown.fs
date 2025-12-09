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
    type AstOrUnd(n, potOpen, potClose) = 
        inherit DelNode()
        override this.Value = String.replicate this.Length this.Char
        abstract Char: string

        member this.Length = n
        member this.IsPotentialOpener: bool = potOpen
        member this.IsPotentialCloser: bool = potClose
        new n = AstOrUnd(n, true, true)
    
    type Asterisk(n, potOpen, potClose) =
        inherit AstOrUnd(n, potOpen, potClose)
        new n = Asterisk(n, true, true)
        override this.Char = "*"

    type Underscore(n, potOpen, potClose) =
        inherit AstOrUnd(n, potOpen, potClose)
        new n = Underscore(n, true, true)
        override this.Char = "_"

    exception AstOrUndException

    // each property stores 6 values.
    // the 1st param has 3 slots being the mod length (0, 1, 2 mod 3)
    // the 2nd param has 2 slots being both-opener-or-not (1 = also opener, 0 = not)
    type OpenersBottomTables = {
        Ast: int[,]
        Und: int[,]
    }

    type EmphBottoms = 
        {
            Stack: int
            Openers: OpenersBottomTables
        }
    
    type Delimiter = 
        {   Node: DelNode
            Active: bool }

    let rec makeDelListHelper (delList: list<Delimiter>) (nodeList: list<INode>) = 
        let getDelNode (node: INode) = 
            match node with
            | :? DelNode as del -> 
                [{  Node = del
                    Active = false }]
            | _ -> []

        if nodeList.IsEmpty then
            delList
        else
            let node = nodeList.Head
            let newDelList = 
                delList @ getDelNode node
            
            makeDelListHelper newDelList nodeList.Tail
    
    let makeDelList = makeDelListHelper []

    let rec processEmphasisHelper 
        (bottoms: EmphBottoms)
        (curPos: int) 
        (nodeList: INode list)
        (delList: Delimiter list) = 
            if curPos >= delList.Length then nodeList else

            let del = delList[curPos]

            let makeNewBottoms (closerNode: AstOrUnd) (idx: int) =
                let newTable (opBotTable: int[,]) =
                    let alsoOpener = 
                        if closerNode.IsPotentialOpener
                        then 1 else 0
                    
                    let modLength = closerNode.Length % 3

                    opBotTable[modLength,alsoOpener] <- idx
                    opBotTable

                match closerNode with
                | :? Underscore -> {
                        bottoms 
                        with Openers = { 
                            bottoms.Openers 
                            with Und = newTable bottoms.Openers.Und 
                        }
                    }
                | :? Asterisk -> {
                        bottoms 
                        with Openers = { 
                            bottoms.Openers 
                            with Ast = newTable bottoms.Openers.Ast 
                        }
                    }
                | _ -> raise AstOrUndException

            let rec findMatchingOpener 
                (closerNode: AstOrUnd) 
                (bottoms : EmphBottoms)
                (idx: int) =
                    if idx < 0 || idx <= bottoms.Stack then None else 

                    let potOpener = delList[idx]
                    
                    if closerNode.GetType() <> potOpener.Node.GetType() 
                    then None else

                    let reachedBottom (openerNode: AstOrUnd) =
                        let modLen = openerNode.Length % 3
                        let alsoOpener = 
                            if openerNode.IsPotentialOpener then 1 else 0

                        match openerNode with
                        | :? Underscore -> idx <= bottoms.Openers.Und[modLen, alsoOpener]
                        | :? Asterisk -> idx <= bottoms.Openers.Ast[modLen, alsoOpener]
                        | _ -> raise AstOrUndException
                    
                    match potOpener.Node with
                    | :? AstOrUnd as openerNode ->
                        if reachedBottom openerNode then None
                        else potOpener |> Some
                    | _ -> findMatchingOpener closerNode bottoms (idx-1)

            match del.Node with
            | :? AstOrUnd as closerNode when closerNode.IsPotentialCloser -> 
                let idx = curPos - 1
                let opener = 
                    findMatchingOpener closerNode bottoms idx
                
                match opener with
                | None -> 
                    let newBottoms = makeNewBottoms closerNode idx
                    let nextPos = curPos + 1
                    processEmphasisHelper newBottoms nextPos nodeList delList
                | Some opener' -> 
                    let closer = del
                    openerFound closer opener' bottoms curPos nodeList delList
            | _ ->
                let nextPos =  curPos + 1
                processEmphasisHelper bottoms nextPos nodeList delList
    // and noOpenersFound 
    //     ()
    and openerFound
        (closer: Delimiter) 
        (opener: Delimiter)
        (bottoms: EmphBottoms)
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
                let helper (node: AstOrUnd) (isAst: bool) =
                    let newLen = node.Length + removeDelLen
                    if newLen = 0 then None else 
                    let newDelNode: AstOrUnd = 
                        if isAst then 
                            Asterisk(newLen, node.IsPotentialOpener, node.IsPotentialCloser)
                        else 
                            Underscore(newLen, node.IsPotentialOpener, node.IsPotentialCloser)
                    newDelNode |> Some

                match delNode with
                | :? Underscore as node -> 
                    helper node false
                | :? Asterisk as node -> 
                    helper node true
                | _ -> raise AstOrUndException
            
            let makeNewDelimeter (del: Delimiter) = 
                let newDelNode = makeNewDelNode del.Node

                newDelNode |> Option.map (fun delNode -> 
                    { Node = delNode
                      Active = del.Active })
            
            let delimeterToList del = 
                [del |> Option.map (fun del' -> del'.Node :> INode)] 
                |> List.choose id
            
            let newOpener = makeNewDelimeter opener 
            let newCloser = makeNewDelimeter closer
            let before = before @ delimeterToList newOpener
            let after = delimeterToList newCloser @ after

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
                    match newOpener with
                    | None -> makeNewDelList newCurPos tail 
                    | Some opener -> 
                        let finTail, finCurPos = makeNewDelList curPos tail
                        opener :: finTail, finCurPos
                else if idx = closerDelI then
                    match newCloser with
                    // if closing node removed then curPos shifts to the next element
                    // but the elements afterwards shifts back once
                    // so no change is made to curPos
                    | None -> makeNewDelList curPos tail
                    | Some closer -> 
                        let finTail, finCurPos = makeNewDelList curPos tail
                        closer :: finTail, finCurPos
                else 
                    let finTail, finCurPos = makeNewDelList curPos tail
                    del :: finTail, finCurPos
            
            let newDelList, newCurPos = delList |> List.indexed |> makeNewDelList curPos

            processEmphasisHelper bottoms newCurPos newNodeList newDelList

    let processEmphasis nodeList = 
        let delList = makeDelList nodeList
        let bottoms = {
            Stack = -1
            Openers = {
                Ast = Array2D.create 3 2 -1
                Und = Array2D.create 3 2 -1
            }
        }
        processEmphasisHelper bottoms 0 nodeList delList
