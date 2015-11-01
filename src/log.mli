type log_level = 
  | Fatal (* A problem we can't recover from, e.g. invalid arguments *)
  | Error (* Compile error *)
  | Warn  (* Compile warning *)
  | Info  (* Informational message (-v) *)
  | Debug (* Low-level information (-vv) *)

type color = Bold | Reset | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

val set_min_level : log_level -> unit

val fatal : ('a, out_channel, unit) format -> 'a
val error : ('a, out_channel, unit) format -> 'a
val warn  : ('a, out_channel, unit) format -> 'a
val info  : ('a, out_channel, unit) format -> 'a
val debug : ('a, out_channel, unit) format -> 'a
