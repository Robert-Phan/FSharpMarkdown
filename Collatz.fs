namespace Markdown

open System
    
module Collatz =
    type Collatz(ab_param: (float * float) array, p: int) = 
        [<TailCall>]
        let rec run (n: int) (ls: list<int>) = 
            if List.exists (fun x -> x = n) ls then 
                ls @ [n]
            else
                let index = 
                    [| 0..p-1 |] 
                    |> Array.filter (fun i -> n % p = i) 
                    |> Array.item 0

                let a, b = ab_param[index]
                let n_new = a * Convert.ToDouble n + b |> Convert.ToInt32
                
                run n_new (ls @ [n])
        
        member this.Run(n: int) = run n []
