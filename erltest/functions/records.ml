type 'a pair = { fst: 'a; snd: 'a }

let pair x y = { fst=x; snd=y }

let fst { fst } = fst
let snd { snd } = snd

let swap p = { fst=p.snd; snd=p.fst }

let map f p = { fst=(f p); snd=(f p) }
