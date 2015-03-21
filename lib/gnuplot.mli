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

(** Simple interface to Gnuplot *)

open Core.Std

module Color : sig
  type t = [
  | `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White
  | `Rgb of int * int * int
  ]
end

module Range : sig
  type t =
  | X  of float * float
  | Y  of float * float
  | XY of float * float * float * float
end

module Style : sig
  type t = [
  | `Solid
  | `Pattern of int
  ]
end

module Output : sig
  type t

  val create
    :  ?font:string
    -> [ `Wxt | `X11 | `Png of string | `Eps of string ]
    -> t
end

module Titles : sig
  type t

  val create
    :  ?x:string list
    -> ?xrotate:int
    -> ?y:string list
    -> ?yrotate:int
    -> unit
    -> t
end

module Series : sig
  type t

  val lines
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> float list
    -> t

  val lines_xy
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> (float * float) list
    -> t

  val lines_timey
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> (Time.t * float) list
    -> t

  val lines_func
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> ?fill:Style.t
    -> string
    -> t

  val points
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> float list
    -> t

  val points_xy
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> (float * float) list
    -> t

  val points_timey
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> (Time.t * float) list
    -> t

  val points_func
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> ?fill:Style.t
    -> string
    -> t

  val steps
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> float list
    -> t

  val steps_xy
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> (float * float) list
    -> t

  val steps_timey
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> (Time.t * float) list
    -> t

  val histogram
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> ?fill:Style.t
    -> float list
    -> t

  val candlesticks
    :  ?title:string
    -> ?color:Color.t
    -> ?weight:int
    -> ?fill:Style.t
    -> (Time.t * float * float * float * float) list
    -> t
end

module Gp : sig
  type t

  val create : ?path:string -> unit -> t

  val close : t -> unit

  val set
    :  ?style:Style.t
    -> ?range:Range.t
    -> ?output:Output.t
    -> ?titles:Titles.t
    -> t
    -> unit

  val unset
    :  ?style:Style.t
    -> ?range:Range.t
    -> t
    -> unit

  val plot
    :  ?style:Style.t
    -> ?range:Range.t
    -> ?output:Output.t
    -> ?titles:Titles.t
    -> t
    -> Series.t
    -> unit

  val plot_many
    :  ?style:Style.t
    -> ?range:Range.t
    -> ?output:Output.t
    -> ?titles:Titles.t
    -> t
    -> Series.t list
    -> unit

  val plot_func
    :  ?style:Style.t
    -> ?range:Range.t
    -> ?output:Output.t
    -> ?titles:Titles.t
    -> t
    -> string
    -> unit
end
