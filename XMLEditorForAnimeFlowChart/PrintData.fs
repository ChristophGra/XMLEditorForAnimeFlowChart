module PrintData
open BasicData
open System
open BasicData.decisionTree

let rec extractDecisions decisionResults =
  match decisionResults with
  | [] -> []
  | head :: tail ->
    match head with 
    | decisionResult.Decision y ->  y :: extractDecisions tail
    | _ -> extractDecisions tail


let rec printDecisionsTree layer decisions  = 
  match decisions with 
  | [] -> ()
  | head :: tail -> 
    Console.WriteLine $"""{String.replicate layer " "}{head.text}"""
    printDecisionsTree (layer + 1) (extractDecisions head.result)
    printDecisionsTree layer tail 