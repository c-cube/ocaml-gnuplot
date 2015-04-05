open Core.Std
open Gnuplot

let () =
  let gen_times ~num_bars =
    let now = Time.now () in
    List.init num_bars ~f:(fun i ->
      Time.sub now Time.Span.(scale day (float i)))
  in
  (* Generate random candlestick bars. *)
  let gen_bars ~num_bars =
    let next_bar cl =
      let op = cl +. (Random.float 1. -. 0.5) /. 2. in
      let hi = op +. Random.float 1. /. 5. in
      let lo = op -. Random.float 1. /. 5. in
      let cl = (lo +. hi) /. 2. in
      op, hi, lo, cl
    in
    let rec loop n_bars bars (( _, _, _, cl) as bar) =
      if n_bars = 0 then
        bars
      else
        loop (n_bars - 1) (bar :: bars) (next_bar cl)
    in
    let op = 100. in
    let hi = op +. Random.float 1. /. 5. in
    let lo = op -. Random.float 1. /. 5. in
    let cl = (lo +. hi) /. 2. in
    loop num_bars [] (op, hi, lo, cl)
  in
  let gen_data () =
    List.zip_exn (gen_times ~num_bars:100) (gen_bars ~num_bars:100)
  in
  let gp = Gp.create () in
  (* Plot a random candlestick chart. *)
  Gp.plot gp (Series.candlesticks (gen_data ()) ~title:"chart");
  Gp.close gp
;;
