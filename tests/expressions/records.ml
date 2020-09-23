type 'a pair = { fst : 'a; snd : 'a }

let pair x y = { fst = x; snd = y }

let fst { fst } = fst

let snd { snd } = snd

let swap p = { fst = p.snd; snd = p.fst }

let map f g { fst; snd } = { fst = f fst; snd = g snd }

let swap_from_expr p f = { fst = (f p).snd; snd = (f p).fst }

let flatten_first p = { fst = p.fst.fst.fst; snd = p.fst.fst.snd }
