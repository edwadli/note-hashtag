open Core.Std

type log_level = 
  | Error (* Compile error *)
  | Warn  (* Compile warning *)
  | Info  (* Informational message (-v) *)
  | Debug (* Low-level information (-vv) *)

val string_of_level : log_level -> string

type color = Bold | Reset | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

val set_min_level : log_level -> unit

val error : ('a, out_channel, unit) format -> 'a
val warn  : ('a, out_channel, unit) format -> 'a
val info  : ('a, out_channel, unit) format -> 'a
val debug : ('a, out_channel, unit) format -> 'a
