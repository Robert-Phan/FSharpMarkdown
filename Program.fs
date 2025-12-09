namespace Markdown

open System
open Markdown

module Test = 
    // let basic = Collatz.Collatz([|0.5, 0; 3, 1|], 2)

    [<EntryPoint>]
    let main args =
        let nodeList: list<INode> = 
            [ Asterisk 1; Plain "test0"; Underscore 2; Plain "test1"; Underscore 2; Plain "test2"; Asterisk 1]

        // let delList = makeDelList textList
        let newNodeList = processEmphasis nodeList

        printfn $"{newNodeList[0]}"
        // printfn $"%A{textList[2..4]}"
        // printfn $"%A{textList[4..6]}"
        0