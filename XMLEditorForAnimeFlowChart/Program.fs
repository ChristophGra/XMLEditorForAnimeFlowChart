// Learn more about F# at http://fsharp.org

open System
open BasicData
open PrintData



let modifyDecision state =
  printDecisionsTree 0 (state.treebase::[])
  state



let updateState (state:state) input = 
  match input with 
  | "modifydecision" -> 
    modifyDecision state
  | "end" -> 
    state
  | _ -> 
    state


[<EntryPoint>]
let main argv =
  let beginningQuestion:decisionText.T = decisionText.create "Do you want to watch anime"  
  let baseNode = {text = beginningQuestion; result = []}
  let mutable state = {cont = true; treebase = baseNode; animeHashList = []}
  while state.cont do
    state <- updateState state (Console.ReadLine())
    
    
  0 // return an integer exit code
