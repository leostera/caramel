let time hint f =
  let t0 = Mtime_clock.now () in
  let res = f () in
  let t1 = Mtime_clock.now () in
  Logs.debug (fun f -> f "%s - %a" hint Mtime.Span.pp (Mtime.span t1 t0));
  res
