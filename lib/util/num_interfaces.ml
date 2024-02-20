(****************************************************)
(*                   数值类型接口                     *)
(*            sig 关键字用来定义一个接口(模块)          *)
(* `type t` 定义了模块中使用的泛型                      *)
(* `val ( + ) : t -> t -> t` 定义了模块中的一个函数,    *)
(* `+` 两边的空格一定要有                              *)
(****************************************************)

module type Infix = sig
  type t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( * ) : t -> t -> t
end

module type Compare = sig
  type t

  val ( = ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
end

module type BasicNumLike = sig
  type t

  module Infix : Infix with type t := t
  module Compare : Compare with type t := t

  val zero : t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val lognot : t -> t
  val neg : t -> t
  val rem : t -> t -> t
end

module type NumLike = sig
  include BasicNumLike

  val of_int32 : int32 -> t
  val to_int32 : t -> int32
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_int : int -> t
  val to_int : t -> int
  val to_float : t -> float

  (* 字符串转换/格式化 *)
  val of_string : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end




