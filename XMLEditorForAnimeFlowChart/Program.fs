// Learn more about F# at http://fsharp.org

open System
open BasicData
open PrintData
open BasicData



let modifyDecision state =
  printDecisionsTree 0 (state.treebase::[])
  state



let updateState (state:state) input = 
  match input with 
  | "modifydecision" -> 
    (true, modifyDecision state)
  | "end" -> 
    (false,state)
  | _ -> 
    (true,state)


[<EntryPoint>]
let main argv =
  let beginningQuestion:decisionText.T = decisionText.create "Do you want to watch anime"  
  let mutable cont = true
  let baseNode = decisionTree.createDecisionFromEmpty beginningQuestion
  let mutable state = {treebase = baseNode; animeHashList = []}
  while cont do
    (cont,state) <- updateState state (Console.ReadLine())
    
    
  0 // return an integer exit code
