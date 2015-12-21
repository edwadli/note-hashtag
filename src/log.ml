open Core.Std
open Printf

type log_level = Error | Warn | Info | Debug

(* For comparing logging levels *)
let int_of_level = function
  | Error -> 100
  | Warn -> 80
  | Info -> 60
  | Debug -> 40

let string_of_level = function
  | Error -> "error"
  | Warn -> "warning"
  | Info -> "info"
  | Debug -> "debug"

type color = Bold | Reset | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

let color_of_level = function
  | Error -> Red
  | Warn -> Yellow
  | Info -> Blue
  | Debug -> Cyan

let string_of_color color = 
  let escape_of_color = function
    | Reset   ->  0
    | Bold    ->  1
    | Black   -> 30
    | Red     -> 31
    | Green   -> 32
    | Yellow  -> 33
    | Blue    -> 34
    | Magenta -> 35
    | Cyan    -> 36
    | White   -> 37
  in
  sprintf "\027[%dm" (escape_of_color color)

let min_level = ref Warn

let set_min_level l =
  min_level := l

let print level fmt =
  let prefix = (string_of_color (color_of_level level)) ^ (string_of_color Bold) ^
    (string_of_level level) ^ ":" ^ (string_of_color Reset) in
  let printer = if int_of_level level >= int_of_level !min_level then fprintf else ifprintf in
  printer stderr ("%s " ^^ fmt ^^ "\n%!") prefix

let error fmt = print Error fmt
let warn  fmt = print Warn  fmt
let info  fmt = print Info  fmt
let debug fmt = print Debug fmt
