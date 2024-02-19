open Batteries

(* 标识符的正则表达式，`\b`表示单词边界 *)
let id_regexp = Str.regexp {|[A-Za-z_][A-Za-z0-9_]*\b|}

(* ^是否定字符 *)
let int_regexp = Str.regexp {|\([0-9]+\)[^A-Za-z0-9_.]|}
let long_regexp = Str.regexp {|\([0-9]+[lL]\)[^A-Za-z0-9_.]|}
let uint_regexp = Str.regexp {|\([0-9]+[uU]\)[^A-Za-z0-9_.]|}
let ulong_regexp = Str.regexp {|\([0-9]+\([uU][lL]\|[lL][uU]\)\)[^A-Za-z0-9_.]|}

(* 浮点数的正则表达式 *)
let double_regexp =
  Str.regexp
    {|\(\([0-9]*\.[0-9]+\|[0-9]+\.?\)[Ee][+-]?[0-9]+\|[0-9]*\.[0-9]+\|[0-9]+\.\)[^A-Za-z0-9_.]|}

(* 单字符正则表达式 *)
let char_regexp = Str.regexp {|'\([^'\\
    ]\|\\['"?\\abfnrtv]\)'|}

(* 字符串正则表达式 *)
let string_regexp = Str.regexp {|"\([^"\\
    ]\|\\['"\\?abfnrtv]\)*"|}

let id_to_tok = function
  | "int" -> Tokens.KWInt
  | "return" -> Tokens.KWReturn
  | "void" -> Tokens.KWVoid
  | "if" -> Tokens.KWIf
  | "else" -> Tokens.KWElse
  | "do" -> Tokens.KWDo
  | "while" -> Tokens.KWWhile
  | "for" -> Tokens.KWFor
  | "break" -> Tokens.KWBreak
  | "continue" -> Tokens.KWContinue
  | "static" -> Tokens.KWStatic
  | "extern" -> Tokens.KWExtern
  | "long" -> Tokens.KWLong
  | "unsigned" -> Tokens.KWUnsigned
  | "signed" -> Tokens.KWSigned
  | "double" -> Tokens.KWDouble
  | "char" -> Tokens.KWChar
  | "sizeof" -> Tokens.KWSizeof
  | "struct" -> Tokens.KWStruct
  | other -> Tokens.Identifier other

let rec lex_helper chars =
  match chars with
  | [] -> []
  (* 模式匹配一定要先匹配双字符，因为是按顺序往下匹配的 *)
  | '&' :: '&' :: rest -> Tokens.LogicalAnd :: lex_helper rest
  | '|' :: '|' :: rest -> Tokens.LogicalOr :: lex_helper rest
  | '=' :: '=' :: rest -> Tokens.DoubleEqual :: lex_helper rest
  | '!' :: '=' :: rest -> Tokens.NotEqual :: lex_helper rest
  | '<' :: '=' :: rest -> Tokens.LessOrEqual :: lex_helper rest
  | '>' :: '=' :: rest -> Tokens.GreaterOrEqual :: lex_helper rest
  | '<' :: rest -> Tokens.LessThan :: lex_helper rest
  | '>' :: rest -> Tokens.GreaterThan :: lex_helper rest
  | '!' :: rest -> Tokens.Bang :: lex_helper rest
  | '=' :: rest -> Tokens.EqualSign :: lex_helper rest
  | '{' :: rest -> Tokens.OpenBrace :: lex_helper rest
  | '}' :: rest -> Tokens.CloseBrace :: lex_helper rest
  | '(' :: rest -> Tokens.OpenParen :: lex_helper rest
  | ')' :: rest -> Tokens.CloseParen :: lex_helper rest
  | ';' :: rest -> Tokens.Semicolon :: lex_helper rest
  | '-' :: '-' :: rest -> Tokens.DoubleHyphen :: lex_helper rest
  | '-' :: '>' :: rest -> Tokens.Arrow :: lex_helper rest
  | '-' :: rest -> Tokens.Hyphen :: lex_helper rest
  | '~' :: rest -> Tokens.Tilde :: lex_helper rest
  | '+' :: rest -> Tokens.Plus :: lex_helper rest
  | '*' :: rest -> Tokens.Star :: lex_helper rest
  | '/' :: rest -> Tokens.Slash :: lex_helper rest
  | '%' :: rest -> Tokens.Percent :: lex_helper rest
  | '?' :: rest -> Tokens.QuestionMark :: lex_helper rest
  | ':' :: rest -> Tokens.Colon :: lex_helper rest
  | ',' :: rest -> Tokens.Comma :: lex_helper rest
  | '&' :: rest -> Tokens.Ampersand :: lex_helper rest
  | '[' :: rest -> Tokens.OpenBracket :: lex_helper rest
  | ']' :: rest -> Tokens.CloseBracket :: lex_helper rest
  | '\'' :: _ -> lex_character chars
  | '"' :: _ -> lex_string chars
  | '.' :: (c :: _ as rest) when not (Char.is_digit c) ->
      Tokens.Dot :: lex_helper rest
  | c :: rest when Char.is_whitespace c -> lex_helper rest
  | c :: _ when Char.is_digit c || c = '.' -> lex_constant chars
  | _ -> lex_idenfitier chars

and lex_character input_chars =
  let input = String.implode input_chars in
  if Str.string_match char_regexp input 0 then
    let ch = Str.matched_string input |> String.rchop |> String.lchop in
    let tok = Tokens.ConstChar ch in
    let remaining = Str.string_after input (Str.match_end ()) in
    tok :: lex_helper (String.explode remaining)
  else failwith ("输入从`'`开始的，单不是一个合法的字符token：" ^ input)

and lex_string input_chars =
  let input = String.implode input_chars in
  if Str.string_match string_regexp input 0 then
    let str = Str.matched_string input |> String.rchop |> String.lchop in
    let tok = Tokens.StringLiteral str in
    let remaining = Str.string_after input (Str.match_end ()) in
    tok :: lex_helper (String.explode remaining)
  else failwith ("输入从\"开始，但不是一个合法的字符串字面量：" ^ input)

and lex_constant input_chars =
  let input = String.implode input_chars in
  let tok =
    if Str.string_match long_regexp input 0 then
      let const_str = String.rchop (Str.matched_group 1 input) in
      Tokens.ConstLong (Big_int.of_string const_str)
    else if Str.string_match int_regexp input 0 then
      let const_str = Str.matched_group 1 input in
      Tokens.ConstInt (Big_int.of_string const_str)
    else if Str.string_match uint_regexp input 0 then
      (* remove "u" suffix *)
      let const_str = String.rchop (Str.matched_group 1 input) in
      Tokens.ConstUInt (Big_int.of_string const_str)
    else if Str.string_match ulong_regexp input 0 then
      (* remove ul/lu suffix *)
      let const_str = String.rchop ~n:2 (Str.matched_group 1 input) in
      Tokens.ConstULong (Big_int.of_string const_str)
    else if Str.string_match double_regexp input 0 then
      (* remove ul/lu suffix *)
      let const_str = Str.matched_group 1 input in
      Tokens.ConstDouble (Float.of_string const_str)
    else failwith ("词法分析错误：输入从一个数字开始，但不是一个常量：" ^ input)
  in
  let remaining = Str.string_after input (Str.group_end 1) in
  tok :: lex_helper (String.explode remaining)

and lex_idenfitier input_chars =
  let input = String.implode input_chars in
  if Str.string_match id_regexp input 0 then
    let id_str = Str.matched_string input in
    let tok = id_to_tok id_str in
    let remaining = Str.string_after input (Str.match_end ()) in
    tok :: lex_helper (String.explode remaining)
  else failwith ("词法分析错误：输入和标识符正则表达式不匹配：" ^ input)

let lex input =
  let input = String.trim input in
  lex_helper (String.explode input)
