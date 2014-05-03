open Core.Std
open Gnuplot

let _ =

  (* create gnuplot process *)
  let gp = Gp.create () in
  Gp.set ~output:(Output.create ~font:"arial" `Wxt) gp;

  (* draw simple function specified as a string *)
  Gp.plot_func gp "sin(x)";

  (* draw plot using a single series of data *)
  Gp.plot gp (Series.lines [2.; 1.; 2.; 5.]);

  (* specify additional properties of the series *)
  Gp.plot gp (
    Series.lines ~title:"Some plot" ~color:`Blue ~weight:3 [2.; 1.; 2.; 5.]
  );

  (* Plot multiple different series into a single plot *)
  Gp.plot_many gp
    [ Series.func ~title:"Sinus" ~color:`Yellow ~weight:3 "sin(x)"
    ; Series.lines [2.; 1.; 2.; 5.]
      ~title:"Some plot" ~color:`Green ~weight:3
    ; Series.histogram [2.; 1.; 2.; 5.]
      ~title:"Some plot" ~fill:`Solid ~color:`Blue ~weight:3 ];

  (* specify range of the plot using 'range' named parameter *)
  Gp.plot gp ~range:(Range.Y (-10., 10.)) (Series.lines [2.; 1.; 2.; 5.]);

  (* specify a range of the plot using 'range' named parameter *)
  Gp.plot_many gp
    ~style:`Solid
    ~range:(Range.XY (-0.5, 3.7, 0.0, 6.0))
    [ Series.histogram [2.0; 1.0; 2.0; 5.0] ~color:`Green
    ; Series.histogram [1.5; 2.0; 2.5; 4.5] ~color:`Blue ];

  (* we can also change global properties using the 'set' method *)
  Gp.set gp ~style:`Solid ~range:(Range.XY (-0.5, 3.7, 0.0, 6.0));
  (* and change the output to a 'png' file *)
  Gp.set gp ~output:(Output.create (`Png "test1.png"));

  (* the setting is then used by all plots *)
  Gp.plot_many gp
    [ Series.histogram [2.0; 1.0; 2.0; 5.0] ~color:`Green
    ; Series.histogram [1.5; 2.0; 2.5; 4.5] ~color:`Blue ];

  Gp.set gp ~output:(Output.create ~font:"arial" `Wxt)
