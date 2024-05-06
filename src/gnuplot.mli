(*
   Gnuplot-OCaml - Simple interface to Gnuplot

   Copyright (C) 2014-  Oliver Gu
   email: gu.oliver@yahoo.com

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** {1 Simple interface to Gnuplot} *)

(** {2 Auxiliary types} *)

module Color : sig
  type t =
    [ `Black
    | `Red
    | `Green
    | `Yellow
    | `Blue
    | `Magenta
    | `Cyan
    | `White
    | `Rgb of int * int * int
    ]
  (** Possible colors of a plot. *)
end

type date = float
type time = float
type timezone = float

module Range : sig
  (** Used for constructing ranges for the X axis, Y axis or both. *)
  type t =
    | X of float * float
    | Y of float * float
    | XY of float * float * float * float  (** arguments are [x1, x2, y1, y2] *)
    | XYZ of float * float * float * float * float * float
        (** arguments are [x1, x2, y1, y2, z1, z2] *)
    | Date of date * date
    | Time of time * time * timezone
    | Local_time of time * time  (** Time range in local time zone. *)
end

type range = Range.t =
  | X of float * float
  | Y of float * float
  | XY of float * float * float * float  (** arguments are [x1, x2, y1, y2] *)
  | XYZ of float * float * float * float * float * float
      (** arguments are [x1, x2, y1, y2, z1, z2] *)
  | Date of date * date
  | Time of time * time * timezone
  | Local_time of time * time  (** Time range in local time zone. *)

module Filling : sig
  type t =
    [ `Solid  (** Fill the plot with a solid fill. *)
    | `Pattern of int  (** Fill the plot with a pre-defined Gnuplot pattern. *)
    ]
  (** Represents possible fillings of a plot. *)
end

module Output : sig
  type t
  (** Specifies the output type for Gnuplot. *)

  val create :
    ?font:string ->
    ?size:int * int ->
    ?params:string ->
    [ `Wxt  (** Wxt terminal device generates output in a separate window. *)
    | `X11  (** X11 terminal device for use with X servers. *)
    | `Qt  (** Qt  terminal device generates output in a separate window. *)
    | `Png of string  (** For saving charts to a PNG file. *)
    | `Png_cairo of string
      (** Same as [`Png], but uses Cairo libs for rendering. *)
    | `Eps of string  (** For saving charts to an EPS file. *)
    ] ->
    t
  (** [create ?font output] creates an output type with optional [font]
      parameter, and other custom parameters [params]. *)
end

module Labels : sig
  type t
  (** Specifies labels for the X and Y axes. *)

  val create : ?x:string -> ?y:string -> unit -> t
end

(** The representation of data-arrays (cf. the {!Series} module). *)
type dim2 = Dim2

type dim3 = Dim3

type 'dim data =
  | Data_Y : float list -> dim2 data
  | Data_XY : (float * float) list -> dim2 data
  | Data_XYZ : (float * float * float) list -> dim3 data
  | Data_TimeY : (time * float) list * timezone -> dim2 data
  | Data_DateY : (date * float) list -> dim2 data
  | Data_TimeOHLC :
      (time * (float * float * float * float)) list * timezone
      -> dim2 data
  | Data_DateOHLC : (date * (float * float * float * float)) list -> dim2 data
  | Func : string -> dim2 data

module Series : sig
  type t
  (** Represents a series of data for the plot functions in the [Gp] module. *)

  val lines : ?title:string -> ?color:Color.t -> ?weight:int -> float list -> t
  (** [lines data] creates a data series for a line plot of Y values. *)

  val lines_xy :
    ?title:string -> ?color:Color.t -> ?weight:int -> (float * float) list -> t
  (** [lines_xy data] creates a data series for a line plot of X and Y
      values. *)

  val lines_timey :
    ?title:string ->
    ?color:Color.t ->
    ?weight:int ->
    zone:timezone ->
    (time * float) list ->
    t
  (** [lines_timey ~zone data] creates a data series for a line plot with time
      axis in the given time [zone]. *)

  val lines_datey :
    ?title:string -> ?color:Color.t -> ?weight:int -> (date * float) list -> t
  (** [lines_datey data] creates a data series for a line plot of date and Y
      values. *)

  val lines_func : ?title:string -> ?color:Color.t -> ?weight:int -> string -> t
  (** [lines_func f] creates a data series for a line plot of the values given
      by a function [f] specified in the Gnuplot format, eg `sin(x)`.  The X
      values come from the range object that was supplied to one of the
      plot functions in the [Gp] module. *)

  val points : ?title:string -> ?color:Color.t -> ?weight:int -> float list -> t
  (** [points data] creates a data series for a point plot of Y values. *)

  val points_xy :
    ?title:string -> ?color:Color.t -> ?weight:int -> (float * float) list -> t
  (** [points_xy data] creates a data series for a point plot of X and Y
      values. *)

  val points_timey :
    ?title:string ->
    ?color:Color.t ->
    ?weight:int ->
    zone:timezone ->
    (time * float) list ->
    t
  (** [points_timey ~zone data] creates a data series for a point plot with time
      axis in the given time [zone]. *)

  val points_datey :
    ?title:string -> ?color:Color.t -> ?weight:int -> (date * float) list -> t
  (** [points_datey data] creates a data series for a point plot of date and Y
      values. *)

  val points_func :
    ?title:string -> ?color:Color.t -> ?weight:int -> string -> t
  (** [points_func f] creates a data series for a point plot of the values given
      by a function [f] specified in the Gnuplot format, eg `sin(x)`.  The X
      values come from the range object that was supplied to one of the plot
      functions in the [Gp] module below. *)

  val linespoints :
    ?title:string -> ?color:Color.t -> ?weight:int -> float list -> t
  (** [linespoints data] creates a data series for a lines and points plot of Y
      values. *)

  val linespoints_xy :
    ?title:string -> ?color:Color.t -> ?weight:int -> (float * float) list -> t
  (** [linespoints_xy data] creates a data series for a lines and points plot of
      X and Y values. *)

  val linespoints_timey :
    ?title:string ->
    ?color:Color.t ->
    ?weight:int ->
    zone:timezone ->
    (time * float) list ->
    t
  (** [linespoints_timey ~zone data] creates a data series for a lines and
      points plot with time axis in the given time [zone]. *)

  val linespoints_datey :
    ?title:string -> ?color:Color.t -> ?weight:int -> (date * float) list -> t
  (** [linespoints_datey data] creates a data series for a lines and points plot
      of date and Y values. *)

  val linespoints_func :
    ?title:string -> ?color:Color.t -> ?weight:int -> string -> t
  (** [linespoints_func f] creates a data series for a lines and points plot of
      the values given by a function [f] specified in the Gnuplot format, eg
      `sin(x)`.  The X values come from the range object that was supplied to
      one of the plot functions in the [Gp] module below. *)

  val steps : ?title:string -> ?color:Color.t -> ?weight:int -> float list -> t
  (** [steps data] creates a data series for a step function of Y values. *)

  val steps_xy :
    ?title:string -> ?color:Color.t -> ?weight:int -> (float * float) list -> t
  (** [steps_xy data] creates a data series for a step function of X and Y
      values. *)

  val steps_timey :
    ?title:string ->
    ?color:Color.t ->
    ?weight:int ->
    zone:timezone ->
    (time * float) list ->
    t
  (** [steps_timey ~zone data] creates a data series for a step function with
      time axis in the given time [zone]. *)

  val steps_datey :
    ?title:string -> ?color:Color.t -> ?weight:int -> (date * float) list -> t
  (** [steps_datey data] creates a data series for a step function of date and Y
      values. *)

  val histogram :
    ?title:string ->
    ?color:Color.t ->
    ?weight:int ->
    ?fill:Filling.t ->
    float list ->
    t
  (** [histogram data] creates a data series for a histogram of Y values. *)

  val candles_time_ohlc :
    ?title:string ->
    ?color:Color.t ->
    ?weight:int ->
    ?fill:Filling.t ->
    zone:timezone ->
    (time * (float * float * float * float)) list ->
    t
  (** [candles_time_ohlc ~zone data] creates a data series for a candlestick
      chart with time axis in the given time [zone]. *)

  val candles_date_ohlc :
    ?title:string ->
    ?color:Color.t ->
    ?weight:int ->
    ?fill:Filling.t ->
    (date * (float * float * float * float)) list ->
    t
  (** [candles_date_ohlc data] creates a data series for a candlestick chart
      indexed by date. *)

  val custom : string -> dim2 data -> t
  (** Low-level, unsafe, interface, please see implementation. *)
end

module Splots : sig
  type t
  (** Represents the data for the splot functions in the [Gp] module. *)

  val lines_xyz :
    ?title:string ->
    ?color:Color.t ->
    ?weight:int ->
    (float * float * float) list ->
    t
  (** [lines_xyz] creates a XYZ line plot. *)

  val points_xyz :
    ?title:string ->
    ?color:Color.t ->
    ?weight:int ->
    (float * float * float) list ->
    t
  (** [points_xyz] creates a scatter plot. *)

  val linespoints_xyz :
    ?title:string ->
    ?color:Color.t ->
    ?weight:int ->
    (float * float * float) list ->
    t
  (** [points_xyz] creates a XYZ points and lines plot. *)

  val custom : string -> dim3 data -> t
  (** Low-level, unsafe, interface, please see implementation. *)
end

(** {2 Main interface} *)

type t
(** A wrapper for calling Gnuplot from OCaml. *)

val create :
  ?verbose:bool (* defaults to false  *) ->
  ?path:string (* defaults to `gnuplot` *) ->
  unit ->
  t
(** [create ?verbose ?path ()] creates a channel to a Gnuplot process with the
    executable given by [path].  If [verbose] is true then plotting commands
    print debug information on standard output. *)

val close : t -> unit
(** [close t] closes the channel to the Gnuplot process. *)

val with_ :
  ?verbose:bool (* defaults to false  *) ->
  ?path:string (* defaults to `gnuplot` *) ->
  (t -> 'a) ->
  'a
(** [with_ ?verbose ?path f] creates a channel to a Gnuplot process,
    using {!create}. Then it calls [f] with this channel, and makes sure
    to {!close} the channel once [f] is done. *)

val set :
  ?output:Output.t (* Uses Gnuplot's default terminal if not set *) ->
  ?title:string ->
  ?use_grid:bool (* Defaults to false *) ->
  ?fill:Filling.t ->
  ?labels:Labels.t ->
  ?custom:(string * string) list ->
  t ->
  unit
(** [set ?output ?title ?fill ?labels ?custom t] sets parameters of the Gnuplot
    session.
    @param custom since 0.7 to specify other settings (set/unset pairs)
*)

val unset :
  ?fill:Filling.t ->
  ?labels:Labels.t ->
  ?custom:(string * string) list ->
  t ->
  unit
(** [unset ?fill ?labels t] resets parameters of the Gnuplot session. *)

val plot :
  ?output:Output.t (* Uses Gnuplot's default terminal if not set *) ->
  ?title:string ->
  ?use_grid:bool (* Defaults to false *) ->
  ?fill:Filling.t ->
  ?range:Range.t ->
  ?labels:Labels.t ->
  ?format:string ->
  ?logscale:string * int option ->
  ?custom:(string * string) list ->
  t ->
  Series.t ->
  unit
(** [plot t series] plots a single data [series].  The parameters for filling,
    range, etc are optional. *)

val plot_many :
  ?output:Output.t (* Uses Gnuplot's default terminal if not set *) ->
  ?title:string ->
  ?use_grid:bool (* Defaults to false *) ->
  ?fill:Filling.t ->
  ?range:Range.t ->
  ?labels:Labels.t ->
  ?format:string ->
  ?logscale:string * int option ->
  ?custom:(string * string) list ->
  t ->
  Series.t list ->
  unit
(** [plot_many t series] creates a plot of multiple data [series].  The
    parameters for filling, range, etc are optional. *)

val plot_func :
  ?output:Output.t (* Uses Gnuplot's default terminal if not set *) ->
  ?title:string ->
  ?use_grid:bool (* Defaults to false *) ->
  ?fill:Filling.t ->
  ?range:Range.t ->
  ?labels:Labels.t ->
  ?logscale:string * int option ->
  ?custom:(string * string) list ->
  t ->
  string ->
  unit
(** [plot_func t f] draws a graph of the function [f] given as a string.
    The function [f] has to be specified in the Gnuplot format, eg `sin(x)`.
    The parameters for the filling, range, etc are optional. *)

val splot :
  ?output:Output.t ->
  ?title:string ->
  ?use_grid:bool ->
  ?fill:Filling.t ->
  ?range:Range.t ->
  ?labels:Labels.t ->
  ?logscale:string * int option ->
  ?custom:(string * string) list ->
  t ->
  Splots.t ->
  unit
(** [splot t s] creates a 3d plot for [s]. The parameters for filling,
    range, etc are optional. *)

val splot_many :
  ?output:Output.t ->
  ?title:string ->
  ?use_grid:bool ->
  ?fill:Filling.t ->
  ?range:Range.t ->
  ?labels:Labels.t ->
  ?logscale:string * int option ->
  ?custom:(string * string) list ->
  t ->
  Splots.t list ->
  unit
(** [splot_many t ls] creates a 3d plot for the list of data [ls].
    The parameters for filling, range, etc are optional. *)
