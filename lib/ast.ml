[@@@coverage exclude_file]

(* 无类型的抽象语法树和有类型注释的抽象语法树的公共模块 *)
(* 词法分析之后, 现将tokens转换成无类型的抽象语法树 *)
(* 语义分析之后, 将无类型的抽象语法树加上类型注解 *)
module CommonAst = struct
  type unary_operator = Complement | Negate | Not [@@deriving show]

  type binary_operator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Mod
    | And
    | Or
    | Equal
    | NotEqual
    | LessThan
    | LessOrEqual
    | GreaterThan
    | GreaterOrEqual
  [@@deriving show]

  (* C语言中的存储类型 *)
  type storage_class = Static | Extern [@@deriving show]

  (* 结构体中的成员声明, 成员名字和成员类型 *)
  type member_declaration = { member_name : string; member_type : Types.t }
  [@@deriving show]

  (* 结构体, tag 是结构体的名字. 还有成员列表 *)
  type struct_declaration = { tag : string; members : member_declaration list }
  [@@deriving show]

  (* 变量声明包含四个信息: 变量名, 变量类型, 初始值(可选), 存储类型(可选) *)
  type 'init_t variable_declaration = {
    name : string;
    var_type : Types.t;
    init : 'init_t option;
    storage_class : storage_class option;
  }
  [@@deriving show]

  (* for 循环的初始化有两种情况: `int i = 0; i < 10; i++` 和 `i = 0; i < 10; i++` 以及 `; i < 10; i++` *)
  type ('init_t, 'exp_t) for_init =
    | InitDecl of 'init_t variable_declaration
    | InitExp of 'exp_t option
  [@@deriving show]

  type ('init_t, 'exp_t) statement =
    | Return of 'exp_t option (* return 语句 *)
    | Expression of 'exp_t (* 表达式语句, 例如: `0;` *)
    | If of {
        (* if 语句 *)
        condition : 'exp_t;
        then_clause : ('init_t, 'exp_t) statement;
        else_clause : ('init_t, 'exp_t) statement option;
      }
    | Compound of ('init_t, 'exp_t) block (* 复合语句, 例如由花括号包含的一系列语句 *)
    | Break of string
    | Continue of string
    | While of {
        condition : 'exp_t;
        body : ('init_t, 'exp_t) statement; (* 标注 body 类型时, 需要加上泛型 *)
        id : string;
      }
    | DoWhile of {
        body : ('init_t, 'exp_t) statement;
        condition : 'exp_t;
        id : string;
      }
    | For of {
        init : ('init_t, 'exp_t) for_init; (* `int i = 0;` *)
        condition : 'exp_t option; (* `i < 10;`    *)
        post : 'exp_t option; (* `i++`        *)
        body : ('init_t, 'exp_t) statement;
        id : string;
      }
    | Null
  [@@deriving show]

  (* 块作用域, 花括号括住的每一项可能是声明, 也可能是语句 *)
  and ('init_t, 'exp_t) block_item =
    | S of ('init_t, 'exp_t) statement
    | D of ('init_t, 'exp_t) declaration
  [@@deriving show]

  (* 块作用域, 花括号包含了1个块作用域. 由 block_item 的列表组成 *)
  and ('init_t, 'exp_t) block = Block of ('init_t, 'exp_t) block_item list

  (* 函数声明 *)
  and ('init_t, 'exp_t) function_declaration = {
    name : string; (* 函数名 *)
    fun_type : Types.t; (* 函数返回值类型 *)
    params : string list; (* 参数列表 *)
    body : ('init_t, 'exp_t) block option; (* 函数体 *)
    storage_class : storage_class option; (* 存储类型 *)
  }
  [@@deriving show]

  (* 声明: 函数声明, 变量声明, 结构体声明 *)
  (* TODO: 联合体声明? *)
  and ('init_t, 'exp_t) declaration =
    | FunDecl of ('init_t, 'exp_t) function_declaration
    | VarDecl of 'init_t variable_declaration
    | StructDecl of struct_declaration

  (* C语言程序由一系列声明组成 *)
  type ('init_t, 'exp_t) prog_t =
    | Program of ('init_t, 'exp_t) declaration list
  [@@deriving show]
end

(* 无类型抽象语法树 *)
module Untyped = struct
  include CommonAst

  type exp =
    | Constant of Const.t (* 常量表达式 *)
    | Var of string (* 变量表达式 *)
    | String of string (* 字符串表达式 *)
    | Cast of { target_type : Types.t; e : exp } (* 强制类型转换 *)
    | Unary of unary_operator * exp (* 一元表达式 *)
    | Binary of binary_operator * exp * exp (* 二元表达式 *)
    | Assignment of exp * exp (* 赋值表达式 *)
    | Conditional of { condition : exp; then_result : exp; else_result : exp }
      (* a == b ? 1 : 0 *)
    | FunCall of { f : string; args : exp list }
    | Dereference of exp (* 解引用, `*a` *)
    | AddrOf of exp (* 取地址运算: &a *)
    | Subscript of { ptr : exp; index : exp } (* 数组下标: a[1] 等价于 *(a + 1) *)
    | SizeOf of exp
    | SizeOfT of Types.t
    | Dot of { strct : exp; member : string } (* 结构体实例通过`.`取出成员: `strct.member` *)
    | Arrow of { strct : exp; member : string } (* 指向结构体实例的指针使用箭头取出成员: "strct->member" 等价于 "(*strct).member" *)
  [@@deriving show]

  type initializr = SingleInit of exp | CompoundInit of initializr list
  [@@deriving show]

  type t = (initializr, exp) CommonAst.prog_t
end

(* 带类型注解的表达式 *)
module Typed = struct
  include CommonAst

  type inner_exp =
    | Constant of Const.t
    | Var of string
    | String of string
    | Cast of { target_type : Types.t; e : exp }
    | Unary of unary_operator * exp
    | Binary of binary_operator * exp * exp
    | Assignment of exp * exp
    | Condition of { condition : exp; then_result : exp; else_result : exp }
    | FunCall of { f : string; args : exp list }
    | Dereference of exp
    | AddrOf of exp
    | Subscript of { ptr : exp; index : exp }
    | SizeOf of exp
    | SizeOfT of exp
    | Dot of { strct : exp; member : string }
    | Arrow of { strct : exp; member : string }
  [@@deriving show]

  (* 表达式和表达式的求值类型 *)
  and exp = { e : inner_exp; t : Types.t }

  type initializr =
    | SingleInit of exp
    | CompoundInit of Types.t * initializr list
  [@@deriving show]

  type t = (initializr, exp) CommonAst.prog_t [@@deriving show]
end

[@@@coverage exclude_file]
