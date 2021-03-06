module BasicData

open System.Security.Cryptography


module animeTitle =
  type T = AnimeTitle of string
  let create (s:string) = 
    AnimeTitle s

module animeDescription =
  type T = AnimeDescription of string
  let create (s:string) =
    AnimeDescription s
module imagepath =
  type T = Imagepath of string
  let create (s:string) = 
    Imagepath s

type anime = {title:animeTitle.T; description:animeDescription.T; imagepath:imagepath.T}


module animeHash =
  type T = Hash of int
  let create (s:anime) =
    Hash (s.title.GetHashCode())
  

type animeWithHash = {hash:animeHash.T; anime:anime}

module decisionText = 
  type T = DecisionText of string
  let create (s:string) =
    DecisionText s
  


module decisionTree =
  type decisionResult = 
    | Anime of animeHash.T
    | Decision of decision
  and decision = {text:decisionText.T; result:decisionResult list}
  let createDecisionResultFromAnimeHash (a:animeHash.T) =
    Anime a
  let createDecisionResultFromDecision (d:decision) =
    Decision d


type state = {cont: bool; treebase: decisionTree.decision; animeHashList: animeWithHash list}
