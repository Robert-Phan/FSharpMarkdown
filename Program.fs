namespace Markdown

open System
open Markdown

module Test = 
    // let basic = Collatz.Collatz([|0.5, 0; 3, 1|], 2)

    [<EntryPoint>]
    let main args =
        let textList: list<INode> = [Asterisk 1; Plain "test"; Asterisk 1; 
                                     Underscore 2; Plain "tester"; Underscore 2]

        let delList = makeDelList textList

        printfn $"%A{textList[0..2]}"
        printfn $"%A{textList[2..4]}"
        printfn $"%A{textList[4..6]}"
        0