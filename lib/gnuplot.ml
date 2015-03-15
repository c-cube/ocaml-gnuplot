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

open Core.Std
open Printf

module Color = struct
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

  let to_rgb = function
    | `Black -> 0, 0, 0
    | `Red -> 255, 0, 0
    | `Green -> 0, 128, 0
    | `Yellow -> 255, 255, 0
    | `Blue -> 0, 0, 255
    | `Magenta -> 255, 0, 255
    | `Cyan -> 0, 255, 255
    | `White -> 255, 255, 255
    | `Rgb (r, g, b) -> r, g, b
end

let format_style = function
  | `Solid -> " fs solid"
  | `Pattern n -> sprintf " fs pattern %d" n

let format_num = sprintf "%f"

module Internal_format = struct

  let format_arg f a_opt =
    match a_opt with
    | None -> ""
    | Some a -> f a

  let format_title = format_arg (sprintf " t '%s'")

  let format_color s = format_arg (fun (color : Color.t) ->
    let r, g, b = Color.to_rgb color in
    sprintf " %s rgb '#%02x%02x%02x'" s r g b)

  let format_num_arg s = format_arg (sprintf " %s %d" s)

  let format_fill = format_arg format_style

  let format_rotate s = format_arg (sprintf "set %s rotate by %d" s)

  let format_titles tics = format_arg (fun list ->
    let titles =
      List.mapi list ~f:(fun n t -> sprintf "\"%s\" %d" t n)
      |> String.concat ~sep:","
    in
    sprintf "set %s (%s)" tics titles)
end
open Internal_format

module Command = struct
  type t = {
    command : string;
    cleanup : string;
  }
end

module Range = struct
  type t =
  | X  of float * float
  | Y  of float * float
  | XY of float * float * float * float

  let range ?xspec ?yspec () =
    let xspec = format_arg (sprintf "set xrange %s\n") xspec in
    let yspec = format_arg (sprintf "set yrange %s\n") yspec in
    { Command.
      command = xspec ^ yspec;
      cleanup = "set autoscale xy";
    }

  let to_cmd_list t =
    let command =
      match t with
      | X (fx, tx) ->
        range
          ~xspec:(sprintf "[%s:%s]" (format_num fx) (format_num tx))
          ()
      | Y (fy, ty) ->
        range
          ~yspec:(sprintf "[%s:%s]" (format_num fy) (format_num ty))
          ()
      | XY (fx, tx, fy, ty) ->
        range
          ~xspec:(sprintf "[%s:%s]" (format_num fx) (format_num tx))
          ~yspec:(sprintf "[%s:%s]" (format_num fy) (format_num ty))
          ()
    in
    [command]
end

module Style = struct
  type t = [
  | `Solid
  | `Pattern of int
  ]

  let to_cmd_list t =
    let cmd =
      format_style t
      |> sprintf "set style %s\n"
    in
    let command = {
      Command.
      command = cmd;
      cleanup = "";
    }
    in
    [command]
end

module Output_type = struct
  type t = [
  | `Wxt
  | `X11
  | `Png of string
  | `Eps of string
  ]
end

module Output = struct
  type t = {
    font : string option;
    output : Output_type.t;
  }

  let create ?font output = { font; output; }

  let to_cmd_list t =
    let font = t.font |> format_arg (sprintf " font '%s'") in
    let output =
      match t.output with
      | `Wxt ->
        "set term wxt persist"^font
      | `X11 ->
        "set term x11 persist"^font
      | `Png s ->
        sprintf "set term png%s\nset output '%s'" font s
      | `Eps s ->
        sprintf "set term postscript eps enhanced%s\nset output '%s'" font s
    in
    let command = {
      Command.
      command = output;
      cleanup = "set term x11";
    }
    in
    [command]
end

module Titles = struct
  type t = {
    x : string list option;
    y : string list option;
    xrotate : int option;
    yrotate : int option;
  }

  let create ?x ?xrotate ?y ?yrotate () =
    { x; y; xrotate; yrotate; }

  let to_cmd_list t =
    let cmd =
      [ format_rotate "xtic" t.xrotate, "set xtic rotate by 0"
      ; format_rotate "ytic" t.yrotate, "set ytic rotate by 0"
      ; format_titles "xtics" t.x, "set xtics auto"
      ; format_titles "ytics" t.y, "set ytics auto"
      ] |> List.filter ~f:(fun (s, _) -> s <> "")
    in
    let command = {
      Command.
      command = cmd |> List.map ~f:fst |> String.concat ~sep:"\n";
      cleanup = cmd |> List.map ~f:snd |> String.concat ~sep:"\n";
    }
    in
    [command]
end

type data =
| Data of float list
| Func of string

module Series = struct
  type t = {
    cmd : string;
    data : data;
  }

  let create ?title ?color ?weight ?fill plot data =
    let cmd =
      String.concat [
        (match data with
        | Data _ -> " '-' using 1 with " ^ plot
        | Func f -> f)
        ; format_title title
        ; format_num_arg "lw" weight
        ; format_color "lc" color
        ; format_fill fill ]
    in
    { cmd; data; }

  let lines ?title ?color ?weight data =
    create ?title ?color ?weight "lines" (Data data)

  let histogram ?title ?color ?weight ?fill data =
    create ?title ?color ?weight ?fill "histogram" (Data data)

  let func ?title ?color ?weight ?fill f =
    create ?title ?color ?weight ?fill "" (Func f)
end

module Gp = struct
  type t = out_channel

  let create ?path () =
    let path = Option.value path ~default:"gnuplot" in
    let t = Unix.open_process_out path in
    t

  let send_cmd t cmd = output_string t (cmd^"\n")

  let close t = ignore (Unix.close_process_out t)

  let send_data t data =
    match data with
    | Data data ->
      List.iter data ~f:(fun x -> send_cmd t (Float.to_string x));
      send_cmd t "e"
    | _ -> ()

  let set ?style ?range ?output ?titles t =
    let commands =
      List.concat
        [ Option.value_map style  ~default:[] ~f:Style. to_cmd_list
        ; Option.value_map range  ~default:[] ~f:Range. to_cmd_list
        ; Option.value_map output ~default:[] ~f:Output.to_cmd_list
        ; Option.value_map titles ~default:[] ~f:Titles.to_cmd_list ]
    in
    List.iter commands ~f:(fun cmd ->
      printf "Setting:\n%s\n%!" cmd.Command.command;
      send_cmd t cmd.Command.command)

  let unset ?style ?range t =
    let commands =
      List.concat
        [ Option.value_map style ~default:[] ~f:Style.to_cmd_list
        ; Option.value_map range ~default:[] ~f:Range.to_cmd_list ]
    in
    List.iter commands ~f:(fun cmd ->
      if cmd.Command.cleanup <> "" then send_cmd t cmd.Command.cleanup)

  let plot_many ?style ?range ?output ?titles t data =
    set ?style ?range ?output ?titles t;
    let cmd =
      "plot \\\n" ^
      (List.map data ~f:(fun s -> s.Series.cmd) |> String.concat ~sep:", \\\n")
    in
    printf "Command: %s\n%!" cmd;
    send_cmd t cmd;
    List.iter data ~f:(fun s -> send_data t s.Series.data);
    unset ?style ?range t;
    flush t

  let plot ?style ?range ?output ?titles t data =
    plot_many ?style ?range ?output ?titles t [data]

  let plot_func ?style ?range ?output ?titles t func =
    plot_many ?style ?range ?output ?titles t [Series.func func]
end
