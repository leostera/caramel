type 'a pair = { fst: 'a; snd: 'a }

let pair x y = { fst=x; snd=y }

let fst { fst } = fst
let snd { snd } = snd

(* FIXME: compilation error! record field expressions not yet supported *)
(* let swap p = { fst=p.snd; snd=p.fst } *)

let map f g { fst; snd } = { fst=(f fst); snd=(g snd) }
