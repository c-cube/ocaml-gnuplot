open Core.Std
open Gnuplot

let () =
  let generate_bars ~num_bars =
    let next_bar tm cl =
      let tm = Time.add tm Time.Span.day in
      let op = cl +. (Random.float 1. -. 0.5) /. 2. in
      let hi = op +. Random.float 1. /. 5. in
      let lo = op -. Random.float 1. /. 5. in
      let cl = (lo +. hi) /. 2. in
      (tm, op, hi, lo, cl)
    in
    let rec loop n_bars bars ((tm, _, _, _, cl) as bar) =
      if n_bars = 0 then
        bars
      else
        loop (n_bars - 1) (bar :: bars) (next_bar tm cl)
    in
    let tm = Time.(sub (now ()) (Span.(scale day (float num_bars)))) in
    let op = 100. in
    let hi = op +. Random.float 1. /. 5. in
    let lo = op -. Random.float 1. /. 5. in
    let cl = (lo +. hi) /. 2. in
    loop num_bars [] (tm, op, hi, lo, cl) |> List.rev
  in
  let gp = Gp.create () in
  Gp.set ~output:(Output.create ~font:"arial" `Wxt) gp;
  (* Plot a random series of candle sticks. *)
  Gp.plot gp (Series.candlesticks (generate_bars ~num_bars:100));
  Gp.close gp
;;
