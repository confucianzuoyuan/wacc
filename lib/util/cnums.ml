include Num_interfaces
(* 等价于 Python 中的 `from Batteries import *` *)
open Batteries

module Float = struct
  include Batteries.Float

  (* 将某一行关闭测试覆盖 *)
  [@@@coverage off]

  type t = float [@@deriving show, eq]

  [@@@coverage on]
end

(* Int8 实现了 NumLike 接口 *)
module Int8 : NumLike = struct
  [@@@coverage off]

  type t = int32 [@@deriving show, eq, ord]

  [@@@coverage on]

  let zero = Int32.zero
  let lognot = Int32.lognot
  let rem = Int32.rem

  (* 处理有符号8位整型数据 *)
  let reset_upper_bytes x =
    if Int32.logand x 128l = Int32.zero then
      let bitmask = 0x000000ffl in
      Int32.logand x bitmask
    else
      let bitmask = 0xffffff00l in
      Int32.logor x bitmask

  (* `%` 是 batteries 库里面自定义的函数 *)
  (* f % g x 表示 f (g x) *)
  let neg = reset_upper_bytes % Int32.neg

  module Infix = struct
    let ( + ) x y = reset_upper_bytes Int32.Infix.(x + y)
    let ( - ) x y = reset_upper_bytes Int32.Infix.(x - y)
    let ( / ) = Int32.Infix.( / )
    let ( * ) x y = reset_upper_bytes Int32.Infix.(x * y)
  end

  module Compare = Int32.Compare

  let check_range x =
    if x > 127l || x < -128l then failwith "溢出了8位有符号整型的范围: (-128, 127) ." else x

  let of_int i =
    let x = Int32.of_int i in
    check_range x

  let to_int = Int32.to_int
  let of_int32 x = reset_upper_bytes x
  let to_int32 x = x
  let of_int64 = reset_upper_bytes % Int32.of_int64
  let to_int64 = Int32.to_int64
  let to_float = Int32.to_float

  let of_string x =
    let result = Int32.of_string x in
    check_range result

  let to_string = Int32.to_string
end

(* MakeCompare 模块接收一个模块 C 作为参数, 传进来的模块需要有一个 compare 方法 *)
(* `:` 表示 MakeCompare 实现了 Compare 接口 *)
module MakeCompare (C : sig
  type t

  val compare : t -> t -> int
end) : Compare with type t = C.t = struct
  type t = C.t

  let ( = ) x y = C.compare x y = 0
  let ( <> ) x y = C.compare x y <> 0
  let ( > ) x y = C.compare x y > 0
  let ( >= ) x y = C.compare x y >= 0
  let ( < ) x y = C.compare x y < 0
  let ( <= ) x y = C.compare x y <= 0
end

module UInt8 : NumLike = struct
  [@@@coverage off]

  type t = int32 [@@deriving show, eq]

  [@@@coverage on]

  let zero = Int32.zero
  let lognot = Int32.lognot
  let compare = Int32.unsigned_compare
  let rem = Int32.unsigned_rem

  let reset_upper_bytes x =
    let bitmask = 0x000000ffl in
    Int32.logand x bitmask

  let neg = reset_upper_bytes % Int32.neg

  module Infix = struct
    let ( + ) x y = reset_upper_bytes Int32.Infix.(x + y)
    let ( - ) x y = reset_upper_bytes Int32.Infix.(x - y)
    let ( / ) x y = Int32.unsigned_div x y
    let ( * ) x y = reset_upper_bytes Int32.Infix.(x * y)
  end

  module Compare = MakeCompare (struct
    type t = int32

    let compare = Int32.unsigned_compare
  end)

  let of_int i =
    let x = Int32.of_int i in
    reset_upper_bytes x

  let to_int = Int32.to_int
  let of_int32 x = reset_upper_bytes x
  let to_int32 x = x
  let of_int64 = reset_upper_bytes % Int32.of_int64
  let to_int64 = Int32.to_int64
  let to_float = Int32.to_float

  let of_string x =
    let result = Int32.of_string x in
    if result > 255l || result < 0l then failwith "溢出了8位无符号整型的范围: (0, 255) ." else result

  let to_string = Int32.to_string
end

module UInt32 : NumLike = struct
  [@@@coverage off]

  type t = int32 [@@deriving show, eq]

  [@@@coverage on]

  let zero = Int32.zero
  let compare = Int32.unsigned_compare
  let rem = Int32.unsigned_rem
  let neg = Int32.neg
  let lognot = Int32.lognot

  module Infix = struct
    let ( + ) x y = Int32.Infix.(x + y)
    let ( - ) x y = Int32.Infix.(x - y)
    let ( / ) = Int32.unsigned_div
    let ( * ) x y = Int32.Infix.(x * y)
  end

  module Compare = MakeCompare (struct
    type t = int32

    let compare = Int32.unsigned_compare
  end)

  let of_int = Int32.of_int

  let to_int x =
    match Int32.unsigned_to_int x with
    | Some i -> i
    | None -> failwith "这个不应该发生!" [@coverage off]

  let to_float = Int.to_float % to_int
  let of_int32 x = x
  let to_int32 x = x
  let of_int64 = Int32.of_int64

  let to_int64 x =
    let i64 = Int32.to_int64 x in
    let bitmask = 0x00000000ffffffffL in
    Int64.logand i64 bitmask

  let of_string s = Int32.of_string ("0u" ^ s)
  let to_string x = Printf.sprintf "%lu" x
end

(* `let%test` 和 `pp_inline_test` 配合使用, 是扩展语法, 用来定义测试用例 *)
let%test "uint_type_conversion" =
  UInt32.of_int64 4294967295L |> UInt32.to_int32 = -1l

module UInt64 : NumLike = struct
  [@@@coverage off]

  type t = int64 [@@deriving show, eq]

  [@@@coverage on]

  let zero = Int64.zero
  let compare = Int64.unsigned_compare
  let neg = Int64.neg
  let lognot = Int64.lognot
  let rem = Int64.unsigned_rem

  module Infix = struct
    let ( + ) x y = Int64.Infix.(x + y)
    let ( - ) x y = Int64.Infix.(x - y)
    let ( / ) = Int64.unsigned_div
    let ( * ) x y = Int64.Infix.(x * y)
  end

  module Compare = MakeCompare (struct
    type t = int64

    let compare = Int64.unsigned_compare
  end)

  let of_int = Int64.of_int

  let to_int x =
    match Int64.unsigned_to_int x with
    | Some i -> i
    | None -> failwith "溢出错误."

  let to_float x =
    if Int64.compare x Int64.zero >= 0 then Int64.to_float x
    else
      let open Big_int.Infix in
      let bi = Big_int.big_int_of_int64 x in
      let bigint_as_uint = bi + (Big_int.of_int 2 ** Big_int.of_int 64) in
      Big_int.float_of_big_int bigint_as_uint

  let of_int32 = Int64.of_int32
  let to_int32 = Int64.to_int32
  let of_int64 x = x
  let to_int64 x = x
  let of_string s = Int64.of_string ("0u" ^ s)
  let to_string x = Printf.sprintf "%Lu" x
end