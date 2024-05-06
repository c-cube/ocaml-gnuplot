module Gp = Gnuplot

let normal =
  let pi = 4. *. atan 1. in
  let saved = ref None in
  fun st ->
    match !saved with
    | Some (r, t) ->
      saved := None;
      r *. sin t
    | None ->
      let u1 = Random.State.float st 1. in
      let u2 = Random.State.float st 1. in
      let r = sqrt (-2. *. log u1) in
      let t = 2. *. pi *. u2 in
      saved := Some (r, t);
      r *. cos t

let () =
  let generate_noise () =
    let st = Random.State.make_self_init () in
    (* Box-Muller transform *)
    Base.List.init 1000 ~f:(fun _ -> normal st, normal st, normal st)
  in
  let gp = Gp.create () in
  (* Scatter plot of a bivariate normal distribution. *)
  Gp.set gp ~use_grid:true ~title:"3D Gaussian noise";
  Gp.splot gp
    (Gp.Splots.points_xyz (generate_noise ()))
    ~range:(Gp.XYZ (-4., 4., -4., 4., -4., 4.));
  Unix.sleep 10;
  Gp.close gp
