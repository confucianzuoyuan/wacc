open Batteries

module Big_int = struct
  include Extended_big_int
end

exception ParseError of string

type expected = Tok of Tokens.t | Name of string

let pp_expected fmt = function
  | Tok tk -> Tokens.pp fmt tk
  | Name s -> Format.pp_print_string fmt s

let raise_error ~expected ~actual =
  let msg =
    Format.asprintf "Expected %a but found %a" pp_expected expected Tokens.pp
      actual
  in
  raise (ParseError msg)

(* 一些帮助函数 *)

let peek tokens =
  match Stream.peek tokens with
  (* 非空token流 *)
  | Some t -> t
  (* token流已经空了, 抛出异常, 我们会在顶层捕获这个异常. *)
  | None -> raise Stream.Failure

let expect expected tokens =
  let actual = Stream.next tokens in
  if actual <> expected then raise_error ~expected:(Tok expected) ~actual
  else ()

(* 二元运算符的优先级, 其它符号的优先级一律返回 None *)
let get_precedence = function
  | Tokens.Star | Tokens.Slash | Tokens.Percent -> Some 50
  | Tokens.Plus | Tokens.Hyphen -> Some 45
  | Tokens.LessThan | Tokens.LessOrEqual | Tokens.GreaterThan
  | Tokens.GreaterOrEqual ->
      Some 35
  | Tokens.DoubleEqual | Tokens.NotEqual -> Some 30
  | Tokens.LogicalAnd -> Some 10
  | Tokens.LogicalOr -> Some 5
  | Tokens.QuestionMark -> Some 3
  | Tokens.EqualSign -> Some 1
  | _ -> None

let unescape s =
  let rec f = function
    | [] -> []
    | '\\' :: '\'' :: rest -> '\'' :: f rest
    | '\\' :: '"' :: rest -> '"' :: f rest
    | '\\' :: '?' :: rest -> '?' :: f rest
    | '\\' :: '\\' :: rest -> '\\' :: f rest
    | '\\' :: 'a' :: rest -> Char.chr 7 :: f rest
    | '\\' :: 'b' :: rest -> '\b' :: f rest
    | '\\' :: 'f' :: rest -> Char.chr 12 :: f rest
    | '\\' :: 'n' :: rest -> '\n' :: f rest
    | '\\' :: 't' :: rest -> '\t' :: f rest
    | '\\' :: 'v' :: rest -> Char.chr 11 :: f rest
    | '\\' :: _ ->
        failwith
          "Internal error: not a valid escape sequence; should have been \
           rejected during lexing" [@coverage off]
    | x :: rest -> x :: f rest
  in
  String.implode (f (String.explode s))

(* 获取 specifier 列表 *)

let is_type_specifier = function
  | Tokens.KWInt | Tokens.KWLong | Tokens.KWUnsigned | Tokens.KWSigned
  | Tokens.KWDouble | Tokens.KWChar | Tokens.KWVoid | KWStruct ->
      true
  | _ -> false

let is_specifier = function
  | Tokens.KWStatic | Tokens.KWExtern -> true
  | other -> is_type_specifier other

let parse_type_specifier tokens =
  match peek tokens with
  | Tokens.KWStruct -> (
      Stream.junk tokens;
      match Stream.next tokens with
      | Tokens.Identifier _ as tag -> tag
      | t -> raise_error ~expected:(Name "a structure tag") ~actual:t)
  | t when is_type_specifier t ->
      Stream.junk tokens;
      t
  | t ->
      failwith
        ("Internal error: called parse_type_specifier on non-type specifier \
          token: "
        ^ Tokens.show t) [@coverage off]

let parse_specifier tokens =
  match peek tokens with
  | (Tokens.KWStatic | Tokens.KWExtern) as spec ->
      Stream.junk tokens;
      spec
  | _ -> parse_type_specifier tokens

let rec parse_type_specifier_list tokens =
  if is_type_specifier (peek tokens) then
    let spec = parse_type_specifier tokens in
    spec :: parse_type_specifier_list tokens
  else []

let rec parse_specifier_list tokens =
  if is_specifier (peek tokens) then
    let spec = parse_specifier tokens in
    spec :: parse_specifier_list tokens
  else []

let parse_storage_class = function
  | Tokens.KWExtern -> Ast.Untyped.Extern
  | Tokens.KWStatic -> Ast.Untyped.Static
  | _ -> failwith "Internal error: bad storage class" [@coverage off]

let parse_type specifier_list =
  let specifier_list = List.sort Tokens.compare specifier_list in
  let is_ident = function Tokens.Identifier _ -> true | _ -> false in
  match specifier_list with
  | [ Tokens.Identifier tag ] -> Types.Structure tag
  | [ Tokens.KWVoid ] -> Types.Void
  | [ Tokens.KWDouble ] -> Types.Double
  | [ Tokens.KWChar ] -> Types.Char
  | [ Tokens.KWChar; Tokens.KWSigned ] -> Types.SChar
  | [ Tokens.KWChar; Tokens.KWUnsigned ] -> Types.UChar
  | _ ->
      if
        specifier_list = []
        || List.length specifier_list
           <> List.length (List.unique specifier_list)
        || List.mem Tokens.KWDouble specifier_list
        || List.mem Tokens.KWChar specifier_list
        || List.mem Tokens.KWVoid specifier_list
        || List.exists is_ident specifier_list
        || List.mem Tokens.KWSigned specifier_list
           && List.mem Tokens.KWUnsigned specifier_list
      then failwith "Invalid type specifier"
      else if
        List.mem Tokens.KWUnsigned specifier_list
        && List.mem Tokens.KWLong specifier_list
      then Types.ULong
      else if List.mem Tokens.KWUnsigned specifier_list then Types.UInt
      else if List.mem Tokens.KWLong specifier_list then Types.Long
      else Types.Int

let parse_type_and_storage_class specifier_list =
  let storage_classes, types =
    List.partition
      (fun tok -> tok = Tokens.KWExtern || tok = Tokens.KWStatic)
      specifier_list
  in
  let typ = parse_type types in
  let storage_class =
    match storage_classes with
    | [] -> None
    | [ sc ] -> Some (parse_storage_class sc)
    | _ :: _ -> failwith "Invalid storage class"
  in
  (typ, storage_class)

(* 解析语法符号 *)

(* <identifier> ::= ? An identifier token ? *)
let parse_id tokens =
  match Stream.next tokens with
  | Tokens.Identifier x -> x
  | other -> raise_error ~expected:(Name "an identifier") ~actual:other

(* 解析常量 *)

let parse_constant tokens =
  try
    match Stream.next tokens with
    | Tokens.ConstChar s ->
        let s' = unescape s in
        if String.length s' = 1 then Const.ConstInt (Int32.of_byte s'.[0])
        else
          failwith
            "Internal error: Character token contains multiple characters, \
             lexer should have rejected this" [@coverage off]
    | Tokens.ConstDouble d -> Const.ConstDouble d
    | Tokens.ConstInt c -> (
        match Big_int.int32_of_big_int_opt c with
        | Some i32 -> Const.ConstInt i32
        | None -> Const.ConstLong (Big_int.int64_of_big_int c))
    | Tokens.ConstLong c -> Const.ConstLong (Big_int.int64_of_big_int c)
    | Tokens.ConstUInt c -> (
        match Big_int.uint32_of_big_int_opt c with
        | Some ui32 -> Const.ConstUInt ui32
        | None -> Const.ConstULong (Big_int.uint64_of_big_int c))
    | tok -> raise_error ~expected:(Name "a constant") ~actual:tok
  with Failure _ ->
    raise
      (ParseError
         "Constant is too large to fit in an int or long with given signedness")

(* 解析声明符 *)

(* 首先将声明符解析成这种类型, 再转换成AST *)
type declarator =
  | Ident of string
  | PointerDeclarator of declarator
  | ArrayDeclarator of declarator * Const.t
  | FunDeclarator of param_info list * declarator

and param_info = Param of Types.t * declarator

(* { "[" <const> "]" }+ *)
let rec parse_array_dimensions tokens =
  match peek tokens with
  | Tokens.OpenBracket ->
      Stream.junk tokens;
      let dim = parse_constant tokens in
      expect Tokens.CloseBracket tokens;
      dim :: parse_array_dimensions tokens
  | _ -> []

(* <simple-declarator> ::= <identifier> | "(" <declarator> ")" *)
let rec parse_simple_declarator tokens =
  let next_tok = Stream.next tokens in
  match next_tok with
  | Tokens.OpenParen ->
      let decl = parse_declarator tokens in
      expect Tokens.CloseParen tokens;
      decl
  | Tokens.Identifier id -> Ident id
  | other -> raise_error ~expected:(Name "a simple declarator") ~actual:other

(* <declarator> ::= "*" <declarator> | <direct-declarator> *)
and parse_declarator tokens =
  match peek tokens with
  | Tokens.Star ->
      Stream.junk tokens;
      let inner = parse_declarator tokens in
      PointerDeclarator inner
  | _ -> parse_direct_declarator tokens

and parse_direct_declarator tokens =
  let simple_dec = parse_simple_declarator tokens in
  match peek tokens with
  | Tokens.OpenBracket ->
      let array_dimensions = parse_array_dimensions tokens in
      List.fold_left
        (fun decl dim -> ArrayDeclarator (decl, dim))
        simple_dec array_dimensions
  | Tokens.OpenParen ->
      let params = parse_param_list tokens in
      FunDeclarator (params, simple_dec)
  | _ -> simple_dec

and parse_param_list tokens =
  expect Tokens.OpenParen tokens;
  let params =
    match Stream.npeek 2 tokens with
    | [ Tokens.KWVoid; Tokens.CloseParen ] ->
        Stream.junk tokens;
        []
    | _ -> param_loop tokens
  in
  expect Tokens.CloseParen tokens;
  params

and param_loop tokens =
  let p = parse_param tokens in
  match peek tokens with
  | Tokens.Comma ->
      Stream.junk tokens;
      p :: param_loop tokens
  | _ -> [ p ]

and parse_param tokens =
  let specifiers = parse_type_specifier_list tokens in
  let param_type = parse_type specifiers in
  let param_decl = parse_declarator tokens in
  Param (param_type, param_decl)

(* 将常量转换成整型, 然后检查是否是合法的数组维度: 必须大于0 *)
let const_to_dim c =
  let i =
    match c with
    | Const.ConstInt i -> Int32.to_int i
    | Const.ConstLong l -> Int64.to_int l
    | Const.ConstUInt u -> Cnums.UInt32.to_int u
    | Const.ConstULong ul -> Cnums.UInt64.to_int ul
    | Const.ConstDouble _ -> failwith "不合法的数组维度"
    | Const.ConstChar _ | Const.ConstUChar _ ->
        failwith "Internal error, we're not using these yet" [@coverage off]
  in
  if i > 0 then i else failwith "数组维度必须大于0"

let rec process_declarator decl base_type =
  match decl with
  | Ident s -> (s, base_type, [])
  | PointerDeclarator d ->
      let derived_type = Types.Pointer base_type in
      process_declarator d derived_type
  | ArrayDeclarator (inner, constant) ->
      let size = const_to_dim constant in
      let derived_type = Types.Array { elem_type = base_type; size } in
      process_declarator inner derived_type
  | FunDeclarator (params, Ident s) ->
      let process_param (Param (p_base_type, p_decl)) =
        let param_name, param_t, _ = process_declarator p_decl p_base_type in
        (match param_t with
        | Types.FunType _ -> raise (ParseError "不支持参数中的函数指针")
        | _ -> ());
        (param_name, param_t)
      in
      let param_names, param_types =
        List.split (List.map process_param params)
      in
      let fun_type = Types.FunType { param_types; ret_type = base_type } in
      (s, fun_type, param_names)
  | FunDeclarator _ -> raise (ParseError "对函数声明符无法应用额外的类型推导")

(* abstract declarators, 抽象声明符 *)
(* "int func(int*, int [10], int* [10])" *)
type abstract_declarator =
  | AbstractPointer of abstract_declarator
  | AbstractArray of abstract_declarator * Const.t
  | AbstractBase

(* <abstract-declarator> ::= "*" [ <abstract-declarator> ]
                           | <direct-abstract-declarator>
*)
let rec parse_abstract_declarator tokens =
  match peek tokens with
  | Tokens.Star ->
      (* 吃掉指针声明符号 *)
      Stream.junk tokens;
      let inner =
        match peek tokens with
        | Tokens.Star | Tokens.OpenParen | Tokens.OpenBracket ->
            (* 有一个内部声明符号 *)
            parse_abstract_declarator tokens
        | Tokens.CloseParen -> AbstractBase
        | other ->
            raise_error ~expected:(Name "an abstract declarator") ~actual:other
      in
      AbstractPointer inner
  | _ -> parse_direct_abstract_declarator tokens

(* <direct-abstract-declarator> ::= "(" <abstract-declarator> ")" { "[" <const> "]" }
                                  | { "[" <const> "]" }+
*)
and parse_direct_abstract_declarator tokens =
  match peek tokens with
  | Tokens.OpenParen ->
      Stream.junk tokens;
      let abstr_decl = parse_abstract_declarator tokens in
      expect Tokens.CloseParen tokens;
      let array_dimensions = parse_array_dimensions tokens in
      List.fold_left
        (fun decl dim -> AbstractArray (decl, dim))
        abstr_decl array_dimensions
  | Tokens.OpenBracket ->
      let array_dimensions = parse_array_dimensions tokens in
      List.fold_left
        (fun decl dim -> AbstractArray (decl, dim))
        AbstractBase array_dimensions
  | other ->
      raise_error ~expected:(Name "an abstract direct declarator") ~actual:other

let rec process_abstract_declarator decl base_type =
  match decl with
  | AbstractBase -> base_type
  | AbstractArray (inner, constant) ->
      let dim = const_to_dim constant in
      let derived_type = Types.Array { elem_type = base_type; size = dim } in
      process_abstract_declarator inner derived_type
  | AbstractPointer inner ->
      let derived_type = Types.Pointer base_type in
      process_abstract_declarator inner derived_type

(* <type-name> ::= { <type-specifier> }+ [ <abstract-declarator> ] *)
let parse_type_name tokens =
  let type_specifiers = parse_type_specifier_list tokens in
  let base_type = parse_type type_specifiers in
  match peek tokens with
  | Tokens.CloseParen -> base_type
  | _ ->
      let abstr_decl = parse_abstract_declarator tokens in
      process_abstract_declarator abstr_decl base_type

(* <unop> ::= "-" | "~" *)
let parse_unop tokens =
  match Stream.next tokens with
  | Tokens.Tilde -> Ast.Untyped.Complement
  | Tokens.Hyphen -> Ast.Untyped.Negate
  | Tokens.Bang -> Ast.Untyped.Not
  | _ ->
      raise (ParseError "Internal error when parsing unary operator")
      [@coverage off]

(* <binop> ::= "-" | "+" | "*" | "/" | "%" *)
let parse_binop tokens =
  match Stream.next tokens with
  | Tokens.Plus -> Ast.Untyped.Add
  | Tokens.Hyphen -> Ast.Untyped.Subtract
  | Tokens.Star -> Ast.Untyped.Multiply
  | Tokens.Slash -> Ast.Untyped.Divide
  | Tokens.Percent -> Ast.Untyped.Mod
  | Tokens.LogicalAnd -> Ast.Untyped.And
  | Tokens.LogicalOr -> Ast.Untyped.Or
  | Tokens.DoubleEqual -> Ast.Untyped.Equal
  | Tokens.NotEqual -> Ast.Untyped.NotEqual
  | Tokens.LessThan -> Ast.Untyped.LessThan
  | Tokens.LessOrEqual -> Ast.Untyped.LessOrEqual
  | Tokens.GreaterThan -> Ast.Untyped.GreaterThan
  | Tokens.GreaterOrEqual -> Ast.Untyped.GreaterOrEqual
  | _ ->
      raise (ParseError "Internal error when parsing binary operator")
      [@coverage off]

let rec parse_string_literals tokens =
  match peek tokens with
  | Tokens.StringLiteral s ->
      Stream.junk tokens;
      unescape s ^ parse_string_literals tokens
  | _ -> ""

(*
<primary-exp> ::= <const> | <identifier> | "(" <exp> ")" | { <string> }+
                | <identifier> "(" [ <argument-list> ] ")"   
*)
let rec parse_primary_expression tokens =
  let next_token = peek tokens in
  match next_token with
  (* constant *)
  | Tokens.ConstChar _ | Tokens.ConstInt _ | Tokens.ConstLong _
  | Tokens.ConstUInt _ | Tokens.ConstULong _ | Tokens.ConstDouble _ ->
      Ast.Untyped.Constant (parse_constant tokens)
  (* identifier *)
  | Tokens.Identifier _ -> (
      let id = parse_id tokens in
      match peek tokens with
      | Tokens.OpenParen ->
          let args = parse_optional_arg_list tokens in
          Ast.Untyped.FunCall { f = id; args }
      | _ -> Ast.Untyped.Var id)
  | Tokens.StringLiteral _ ->
      let string_exp = parse_string_literals tokens in
      Ast.Untyped.String string_exp
  | Tokens.OpenParen ->
      Stream.junk tokens;
      let e = parse_expression 0 tokens in
      expect Tokens.CloseParen tokens;
      e
  | other -> raise_error ~expected:(Name "a primary expression") ~actual:other

(* <postfix-exp> ::= <primary-exp> { <postfix-op> } *)
and parse_postfix_expression tokens =
  let primary = parse_primary_expression tokens in
  postfix_helper primary tokens

(*
<postfix-op> ::= "[" <exp> "]"
               | "." <identifier>
               | "->" <identifier>   
*)
and postfix_helper primary tokens =
  match peek tokens with
  | Tokens.OpenBracket ->
      Stream.junk tokens;
      let index = parse_expression 0 tokens in
      expect Tokens.CloseBracket tokens;
      let subscript_exp = Ast.Untyped.Subscript { ptr = primary; index } in
      postfix_helper subscript_exp tokens
  | Tokens.Dot ->
      Stream.junk tokens;
      let member = parse_id tokens in
      let member_exp = Ast.Untyped.Dot { strct = primary; member } in
      postfix_helper member_exp tokens
  | Tokens.Arrow ->
      Stream.junk tokens;
      let member = parse_id tokens in
      let arrow_exp = Ast.Untyped.Arrow { strct = primary; member } in
      postfix_helper arrow_exp tokens
  | _ -> primary

(*
<cast-exp> ::= "(" <type-name> ")" <cast-exp>
             | <unary-exp>  
*)
and parse_cast_expression tokens =
  match Stream.npeek 2 tokens with
  | Tokens.OpenParen :: t :: _ when is_type_specifier t ->
      Stream.junk tokens;
      let target_type = parse_type_name tokens in
      expect Tokens.CloseParen tokens;
      let inner_exp = parse_cast_expression tokens in
      Ast.Untyped.Cast { target_type; e = inner_exp }
  | _ -> parse_unary_expression tokens

(*
<unary-exp> ::= <unop> <cast-exp>
              | "sizeof" <unary-exp>
              | "sizeof" "(" <type-name> ")"
              | <postfix-exp>   
*)
and parse_unary_expression tokens =
  match Stream.npeek 3 tokens with
  | Tokens.Star :: _ ->
      Stream.junk tokens;
      let inner_exp = parse_cast_expression tokens in
      Ast.Untyped.Dereference inner_exp
  | Tokens.Ampersand :: _ ->
      Stream.junk tokens;
      let inner_exp = parse_cast_expression tokens in
      Ast.Untyped.AddrOf inner_exp
  | (Tokens.Hyphen | Tokens.Tilde | Tokens.Bang) :: _ ->
      let operator = parse_unop tokens in
      let inner_exp = parse_cast_expression tokens in
      Ast.Untyped.Unary (operator, inner_exp)
  | Tokens.KWSizeOf :: Tokens.OpenParen :: t :: _ when is_type_specifier t ->
      Stream.junk tokens;
      Stream.junk tokens;
      let target_type = parse_type_name tokens in
      expect Tokens.CloseParen tokens;
      Ast.Untyped.SizeOfT target_type
  | Tokens.KWSizeOf :: _ ->
      Stream.junk tokens;
      let inner_exp = parse_unary_expression tokens in
      Ast.Untyped.SizeOf inner_exp
  | _ -> parse_postfix_expression tokens

(* "(" [ <argument-list> ] ")" *)
and parse_optional_arg_list tokens =
  expect Tokens.OpenParen tokens;
  let args =
    match peek tokens with
    | Tokens.CloseParen -> []
    | _ -> parse_arg_list tokens
  in
  expect Tokens.CloseParen tokens;
  args

(* <argument-list> ::= <exp> { "," <exp> } *)
and parse_arg_list tokens =
  let arg = parse_expression 0 tokens in
  match peek tokens with
  | Tokens.Comma ->
      Stream.junk tokens;
      arg :: parse_arg_list tokens
  | _ -> [ arg ]

(* "?" <exp> ";" *)
and parse_conditional_middle tokens =
  let _ = expect Tokens.QuestionMark tokens in
  let e = parse_expression 0 tokens in
  let _ = expect Tokens.Colon tokens in
  e

(* <exp> ::= <factor> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp> *)
and parse_expression min_prec tokens =
  let initial_factor = parse_cast_expression tokens in
  let next_token = peek tokens in
  let rec parse_exp_loop left next =
    match get_precedence next with
    | Some prec when prec >= min_prec -> (
        match next with
        | Tokens.EqualSign ->
            let _ = Stream.junk tokens in
            let right = parse_expression prec tokens in
            let left = Ast.Untyped.Assignment (left, right) in
            parse_exp_loop left (peek tokens)
        | Tokens.QuestionMark ->
            let middle = parse_conditional_middle tokens in
            let right = parse_expression prec tokens in
            let left =
              Ast.Untyped.Conditional
                { condition = left; then_result = middle; else_result = right }
            in
            parse_exp_loop left (peek tokens)
        | _ ->
            let operator = parse_binop tokens in
            let right = parse_expression (prec + 1) tokens in
            let left = Ast.Untyped.Binary (operator, left, right) in
            parse_exp_loop left (peek tokens))
    | _ -> left
  in
  parse_exp_loop initial_factor next_token

(* parse an optional expression followed by a delimiter *)
let parse_optional_expression delim tokens =
  if peek tokens = delim then (
    Stream.junk tokens;
    None)
  else
    let e = parse_expression 0 tokens in
    expect delim tokens;
    Some e

(* <initializer> ::= <exp> | "{" <initializer> { "," <initializer> } [ "," ] "}" *)
let rec parse_initializer tokens =
  if peek tokens = Tokens.OpenBrace then (
    Stream.junk tokens;
    let init_list = parse_init_list tokens in
    expect Tokens.CloseBrace tokens;
    Ast.Untyped.CompoundInit init_list)
  else
    let e = parse_expression 0 tokens in
    Ast.Untyped.SingleInit e

and parse_init_list tokens =
  let next_init = parse_initializer tokens in
  match Stream.npeek 2 tokens with
  (* trailing comma - consume it and return *)
  | Tokens.Comma :: Tokens.CloseBrace :: _ ->
      Stream.junk tokens;
      [ next_init ]
  (* comma that isn't followed by a brace means there's one more element *)
  | Tokens.Comma :: _ ->
      Stream.junk tokens;
      next_init :: parse_init_list tokens
  | _ -> [ next_init ]

(*
<statement> ::= "return" <exp> ";"
              | <exp> ";"
              | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
              | <block>
              | "break" ";"
              | "continue" ";"
              | "while" "(" <exp> ")" <statement>
              | "do" <statement> "while" "(" <exp> ")" ";"
              | "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement>
              | ";"   
*)
let rec parse_statement tokens =
  match peek tokens with
  | Tokens.KWIf -> parse_if_statement tokens
  | Tokens.OpenBrace -> Ast.Untyped.Compound (parse_block tokens)
  | Tokens.KWDo -> parse_do_loop tokens
  | Tokens.KWWhile -> parse_while_loop tokens
  | Tokens.KWFor -> parse_for_loop tokens
  | Tokens.KWBreak ->
      Stream.junk tokens;
      expect Tokens.Semicolon tokens;
      Ast.Untyped.Break ""
  | Tokens.KWContinue ->
      Stream.junk tokens;
      expect Tokens.Semicolon tokens;
      Ast.Untyped.Continue ""
  | Tokens.KWReturn ->
      (* 吃掉return关键字 *)
      let _ = Stream.junk tokens in
      let exp = parse_optional_expression Tokens.Semicolon tokens in
      Ast.Untyped.Return exp
  | _ -> (
      let opt_exp = parse_optional_expression Tokens.Semicolon tokens in
      match opt_exp with
      | Some exp -> Ast.Untyped.Expression exp
      | None -> Ast.Untyped.Null)

(* "if" "(" <exp> ")" <statement> [ "else" <statement> ] *)
and parse_if_statement tokens =
  let _ = expect Tokens.KWIf tokens in
  let _ = expect Tokens.OpenParen tokens in
  let condition = parse_expression 0 tokens in
  let _ = expect Tokens.CloseParen tokens in
  let then_clause = parse_statement tokens in
  let else_clause =
    match peek tokens with
    | KWElse ->
        let _ = Stream.junk tokens in
        Some (parse_statement tokens)
    | _ -> None
  in
  Ast.Untyped.If { condition; then_clause; else_clause }

(* "do" <statement> "while" "(" <exp> ")" ";" *)
and parse_do_loop tokens =
  expect Tokens.KWDo tokens;
  let body = parse_statement tokens in
  expect Tokens.KWWhile tokens;
  expect Tokens.OpenParen tokens;
  let condition = parse_expression 0 tokens in
  expect Tokens.CloseParen tokens;
  expect Tokens.Semicolon tokens;
  Ast.Untyped.DoWhile { body; condition; id = "" }

(* "while" "(" <exp> ")" <statement> *)
and parse_while_loop tokens =
  expect Tokens.KWWhile tokens;
  expect Tokens.OpenParen tokens;
  let condition = parse_expression 0 tokens in
  expect Tokens.CloseParen tokens;
  let body = parse_statement tokens in
  Ast.Untyped.While { condition; body; id = "" }

(* "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement> *)
and parse_for_loop tokens =
  expect Tokens.KWFor tokens;
  expect Tokens.OpenParen tokens;
  let init = parse_for_init tokens in
  let condition = parse_optional_expression Tokens.Semicolon tokens in
  let post = parse_optional_expression Tokens.CloseParen tokens in
  let body = parse_statement tokens in
  Ast.Untyped.For { init; condition; post; body; id = "" }

(* <block-item> ::= <statement> | <declaration> *)
and parse_block_item tokens =
  if is_specifier (peek tokens) then Ast.Untyped.D (parse_declaration tokens)
  else Ast.Untyped.S (parse_statement tokens)

(* helper function to parse list of block items, stopping when we hit a close brace *)
and parse_block_item_list tokens =
  match peek tokens with
  | Tokens.CloseBrace -> []
  | _ ->
      let next_block_item = parse_block_item tokens in
      next_block_item :: parse_block_item_list tokens

(* <block> ::= "{" { <block-item> } "}" *)
and parse_block tokens =
  expect Tokens.OpenBrace tokens;
  let block_items = parse_block_item_list tokens in
  expect Tokens.CloseBrace tokens;
  Ast.Untyped.Block block_items

(*
<function-declaration> ::= { <specifier> }+ <declarator> ( <block> | ";" )
我们已经解析过 { <specifier> }+ <declarator>   
*)
and finish_parsing_function_declaration fun_type storage_class name params
    tokens =
  let body =
    match peek tokens with
    | Tokens.OpenBrace -> Some (parse_block tokens)
    | Tokens.Semicolon ->
        Stream.junk tokens;
        None
    | other ->
        raise_error ~expected:(Name "function body or semicolon") ~actual:other
  in
  Ast.Untyped.{ name; fun_type; storage_class; params; body }

(* <variable-declaration> ::= { <specifier> }+ <declarator> [ "=" <exp> ] ";"
   我们已经解析过 { <specifier> }+ <declarator>
*)
and finish_parsing_variable_declaration var_type storage_class name tokens =
  match Stream.next tokens with
  | Tokens.Semicolon ->
      Ast.Untyped.{ name; var_type; storage_class; init = None }
  | Tokens.EqualSign ->
      let init = parse_initializer tokens in
      expect Tokens.Semicolon tokens;
      { name; var_type; storage_class; init = Some init }
  | other ->
      raise_error ~expected:(Name "An initializer or semicolon") ~actual:other

(* <declaration> ::= <variable-declaration> | <function-declaration> | <struct-declaration> *)
and parse_declaration tokens =
  (* 首先确定是否是一个结构体声明 *)
  match Stream.npeek 3 tokens with
  | [
   Tokens.KWStruct; Tokens.Identifier _; (Tokens.OpenBrace | Tokens.Semicolon);
  ] ->
      parse_structure_declaration tokens
  | _ -> (
      let specifiers = parse_specifier_list tokens in
      let base_typ, storage_class = parse_type_and_storage_class specifiers in
      let declarator = parse_declarator tokens in
      let name, typ, params = process_declarator declarator base_typ in
      match typ with
      | Types.FunType _ ->
          Ast.Untyped.FunDecl
            (finish_parsing_function_declaration typ storage_class name params
               tokens)
      | _ ->
          if params = [] then
            Ast.Untyped.VarDecl
              (finish_parsing_variable_declaration typ storage_class name tokens)
          else
            failwith "Internal error: declarator hs parameters but object type"
            [@coverage off])

(* <struct-declaration> ::= "struct" <identifier> [ "{" { <member-declaration> }+ "}" ] ";" *)
and parse_structure_declaration tokens =
  expect Tokens.KWStruct tokens;
  let tag = parse_id tokens in
  let members =
    match Stream.next tokens with
    | Tokens.Semicolon -> []
    | Tokens.OpenBrace ->
        let members = parse_member_list tokens in
        expect Tokens.CloseBrace tokens;
        expect Tokens.Semicolon tokens;
        members
    | _ ->
        failwith
          "Internal error: shouldn't haev called parse_structure_declaration \
           here" [@coverage off]
  in
  Ast.Untyped.StructDecl { tag; members }

(* parse a non-empty member list *)
and parse_member_list tokens =
  let m = parse_member tokens in
  match peek tokens with
  | Tokens.CloseBrace -> [ m ]
  | _ -> m :: parse_member_list tokens

(* <member-declaration> ::= { <type-specifier> }+ <declarator> ";" *)
and parse_member tokens =
  let specifiers = parse_type_specifier_list tokens in
  let t = parse_type specifiers in
  let member_decl = parse_declarator tokens in
  match member_decl with
  | FunDeclarator _ ->
      raise (ParseError "found function declarator in struct member list")
  | _ ->
      expect Tokens.Semicolon tokens;
      let member_name, member_type, _params =
        process_declarator member_decl t
      in
      Ast.Untyped.{ member_name; member_type }

(* helper function to accept variable declarations and reject function declarations *)
and parse_variable_declaration tokens =
  match parse_declaration tokens with
  | Ast.Untyped.VarDecl vd -> vd
  | Ast.Untyped.FunDecl _ | Ast.Untyped.StructDecl _ ->
      raise
        (ParseError
           "Expected variable declaration but found function or structure \
            declaration")

(* <for-init> ::= <declaration> | [ <exp> ] ";" *)
and parse_for_init tokens =
  if is_specifier (peek tokens) then
    Ast.Untyped.InitDecl (parse_variable_declaration tokens)
  else
    let opt_e = parse_optional_expression Tokens.Semicolon tokens in
    Ast.Untyped.InitExp opt_e

(* { <function-declaration> } *)
let rec parse_declaration_list tokens =
  match Stream.peek tokens with
  | None -> (* 我们已经到达tokens流的结尾 *) []
  | Some _ ->
      let next_decl = parse_declaration tokens in
      next_decl :: parse_declaration_list tokens

(* <program> ::= { <function-declaration> } *)
let parse tokens =
  try
    let token_stream = Stream.of_list tokens in
    let declarations = parse_declaration_list token_stream in
    Ast.Untyped.Program declarations
  with Stream.Failure -> raise (ParseError "Unexpected end of file")
