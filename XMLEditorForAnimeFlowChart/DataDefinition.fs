module DataDefinition

type anime = {title:string; description:string; imagepath:string}
type animeHash =
  | Hash of string
type animeWithHash = {animeHash:animeHash; anime:anime}
type result = 
  | Anime of animeHash
  | Decision of decision
and decision = {text:string; results: result list}
type state = {root:decision; animeHashList: animeWithHash list}