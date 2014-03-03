(* p3pre.ml *)

open P3_lib
open P3_everything

(* command line args *)
type ty_cl_args = { input:string; debug:bool }
let cl0 = { 
  input="-"; (* default to stdin *)
  debug=false
}
  
(* precedence to earlier args *)
let rec parse_CL = 
  let open P1_lib.P1_core.BasicParsers in
  fun i -> (
  let f1 (f,xs) cl = (match (f,xs) with
    | ("-f",[a]) -> {cl with input=a }
    | ("-debug",[]) -> {cl with debug=true } 
    | _ -> (failwith ("parse_CL: unrecognized flag/arg combination: "^f^" "^(String.concat " " xs))))
  in
  let sep = a "\x00" in
  (((listof parse_FLARGS sep) **> parse_EOF) >> (fun (xs,_) -> itlist f1 xs cl0))) i

let args = get_args parse_CL Sys.argv

(* fix debug at this point *)
let _ = (debugging:=args.debug)

(* our terminal parsers must take a token and check whether it is of the form required; e.g. the terminal parser ?DOTDOT? must check that the token is DOTDOT *)

(* tokens with arguments are treated slightly differently *)

open Parser

let tokint_of_token tok = (match tok with
  | AMPERAMPER              ->     1
  | AMPERSAND               ->     2
  | AND                     ->     3
  | AS                      ->     4
  | ASSERT                  ->     5
  | BACKQUOTE               ->     6
  | BANG                    ->     7
  | BAR                     ->     8
  | BARBAR                  ->     9
  | BARRBRACKET             ->    10
  | BEGIN                   ->    11
  | CHAR(_)                 ->    12
  | CLASS                   ->    13
  | COLON                   ->    14
  | COLONCOLON              ->    15
  | COLONEQUAL              ->    16
  | COLONGREATER            ->    17
  | COMMA                   ->    18
  | CONSTRAINT              ->    19
  | DO                      ->    20
  | DONE                    ->    21
  | DOT                     ->    22
  | DOTDOT                  ->    23
  | DOWNTO                  ->    24
  | ELSE                    ->    25
  | END                     ->    26
  | EOF                     ->    27
  | EQUAL                   ->    28
  | EXCEPTION               ->    29
  | EXTERNAL                ->    30
  | FALSE                   ->    31
  | FLOAT(_)                ->    32
  | FOR                     ->    33
  | FUN                     ->    34
  | FUNCTION                ->    35
  | FUNCTOR                 ->    36
  | GREATER                 ->    37
  | GREATERRBRACE           ->    38
  | GREATERRBRACKET         ->    39
  | IF                      ->    40
  | IN                      ->    41
  | INCLUDE                 ->    42
  | INFIXOP0(_)             ->    43
  | INFIXOP1(_)             ->    44
  | INFIXOP2(_)             ->    45
  | INFIXOP3(_)             ->    46
  | INFIXOP4(_)             ->    47
  | INHERIT                 ->    48
  | INITIALIZER             ->    49
  | INT (_)                 ->    50
  | INT32 (_)               ->    51
  | INT64 (_)               ->    52
  | LABEL (_)               ->    53
  | LAZY                    ->    54
  | LBRACE                  ->    55
  | LBRACELESS              ->    56
  | LBRACKET                ->    57
  | LBRACKETBAR             ->    58
  | LBRACKETLESS            ->    59
  | LBRACKETGREATER         ->    60
(*  | LBRACKETPERCENT         ->    61
  | LBRACKETPERCENTPERCENT  ->    62 *)
  | LESS                    ->    63
  | LESSMINUS               ->    64
  | LET                     ->    65
  | LIDENT (_)              ->    66
  | LPAREN                  ->    67
(*  | LBRACKETAT              ->    68
  | LBRACKETATAT            ->    69 *)
  | MATCH                   ->    70
  | METHOD                  ->    71
  | MINUS                   ->    72
  | MINUSDOT                ->    73
  | MINUSGREATER            ->    74
  | MODULE                  ->    75
  | MUTABLE                 ->    76
  | NATIVEINT (_)           ->    77
  | NEW                     ->    78
  | OBJECT                  ->    79
  | OF                      ->    80
  | OPEN                    ->    81
  | OPTLABEL (_)            ->    82
  | OR                      ->    83
(*  | PERCENT                 ->    84 *)
  | PLUS                    ->    85
  | PLUSDOT                 ->    86
  | PREFIXOP (_)            ->    87
  | PRIVATE                 ->    88
  | QUESTION                ->    89
  | QUOTE                   ->    90
  | RBRACE                  ->    91
  | RBRACKET                ->    92
  | REC                     ->    93
  | RPAREN                  ->    94
  | SEMI                    ->    95
  | SEMISEMI                ->    96
  | SHARP                   ->    97
  | SIG                     ->    98
  | STAR                    ->    99
  | STRING (_)              ->   100
  | STRUCT                  ->   101
  | THEN                    ->   102
  | TILDE                   ->   103
  | TO                      ->   104
  | TRUE                    ->   105
  | TRY                     ->   106
  | TYPE                    ->   107
  | UIDENT (_)              ->   108
  | UNDERSCORE              ->   109
  | VAL                     ->   110
  | VIRTUAL                 ->   111
  | WHEN                    ->   112
  | WHILE                   ->   113
  | WITH                    ->   114
  | COMMENT (_)             ->   115)

let tokint_of_string s = (match s with
  | "?AMPERAMPER?"          ->     1
  | "?AMPERSAND?"           ->     2
  | "?AND?"                 ->     3
  | "?AS?"                  ->     4
  | "?ASSERT?"              ->     5
  | "?BACKQUOTE?"           ->     6
  | "?BANG?"                ->     7
  | "?BAR?"                 ->     8
  | "?BARBAR?"              ->     9
  | "?BARRBRACKET?"         ->    10
  | "?BEGIN?"               ->    11
  | "?CHAR?"                ->    12
  | "?CLASS?"               ->    13
  | "?COLON?"               ->    14
  | "?COLONCOLON?"          ->    15
  | "?COLONEQUAL?"          ->    16
  | "?COLONGREATER?"        ->    17
  | "?COMMA?"               ->    18
  | "?CONSTRAINT?"          ->    19
  | "?DO?"                  ->    20
  | "?DONE?"                ->    21
  | "?DOT?"                 ->    22
  | "?DOTDOT?"              ->    23
  | "?DOWNTO?"              ->    24
  | "?ELSE?"                ->    25
  | "?END?"                 ->    26
  | "?EOF?"                 ->    27
  | "?EQUAL?"               ->    28
  | "?EXCEPTION?"           ->    29
  | "?EXTERNAL?"            ->    30
  | "?FALSE?"               ->    31
  | "?FLOAT?"               ->    32
  | "?FOR?"                 ->    33
  | "?FUN?"                 ->    34
  | "?FUNCTION?"            ->    35
  | "?FUNCTOR?"             ->    36
  | "?GREATER?"             ->    37
  | "?GREATERRBRACE?"       ->    38
  | "?GREATERRBRACKET?"     ->    39
  | "?IF?"                  ->    40
  | "?IN?"                  ->    41
  | "?INCLUDE?"             ->    42
  | "?INFIXOP0?"            ->    43
  | "?INFIXOP1?"            ->    44
  | "?INFIXOP2?"            ->    45
  | "?INFIXOP3?"            ->    46
  | "?INFIXOP4?"            ->    47
  | "?INHERIT?"             ->    48
  | "?INITIALIZER?"         ->    49
  | "?INT?"                 ->    50
  | "?INT32?"               ->    51
  | "?INT64?"               ->    52
  | "?LABEL?"               ->    53
  | "?LAZY?"                ->    54
  | "?LBRACE?"              ->    55
  | "?LBRACELESS?"          ->    56
  | "?LBRACKET?"            ->    57
  | "?LBRACKETBAR?"         ->    58
  | "?LBRACKETLESS?"        ->    59
  | "?LBRACKETGREATER?"     ->    60
  | "?LBRACKETPERCENT?"     ->    61
  | "?LBRACKETPERCENTPERCENT?" -> 62
  | "?LESS?"                ->    63
  | "?LESSMINUS?"           ->    64
  | "?LET?"                 ->    65
  | "?LIDENT?"              ->    66
  | "?LPAREN?"              ->    67
  | "?LBRACKETAT?"          ->    68
  | "?LBRACKETATAT?"        ->    69
  | "?MATCH?"               ->    70
  | "?METHOD?"              ->    71
  | "?MINUS?"               ->    72
  | "?MINUSDOT?"            ->    73
  | "?MINUSGREATER?"        ->    74
  | "?MODULE?"              ->    75
  | "?MUTABLE?"             ->    76
  | "?NATIVEINT?"           ->    77
  | "?NEW?"                 ->    78
  | "?OBJECT?"              ->    79
  | "?OF?"                  ->    80
  | "?OPEN?"                ->    81
  | "?OPTLABEL?"            ->    82
  | "?OR?"                  ->    83
  | "?PERCENT?"             ->    84
  | "?PLUS?"                ->    85
  | "?PLUSDOT?"             ->    86
  | "?PREFIXOP?"            ->    87
  | "?PRIVATE?"             ->    88
  | "?QUESTION?"            ->    89
  | "?QUOTE?"               ->    90
  | "?RBRACE?"              ->    91
  | "?RBRACKET?"            ->    92
  | "?REC?"                 ->    93
  | "?RPAREN?"              ->    94
  | "?SEMI?"                ->    95
  | "?SEMISEMI?"            ->    96
  | "?SHARP?"               ->    97
  | "?SIG?"                 ->    98
  | "?STAR?"                ->    99
  | "?STRING?"              ->   100
  | "?STRUCT?"              ->   101
  | "?THEN?"                ->   102
  | "?TILDE?"               ->   103
  | "?TO?"                  ->   104
  | "?TRUE?"                ->   105
  | "?TRY?"                 ->   106
  | "?TYPE?"                ->   107
  | "?UIDENT?"              ->   108
  | "?UNDERSCORE?"          ->   109
  | "?VAL?"                 ->   110
  | "?VIRTUAL?"             ->   111
  | "?WHEN?"                ->   112
  | "?WHILE?"               ->   113
  | "?WITH?"                ->   114
  | "?COMMENT?"             ->   115
  | _ -> (failwith ("tokint_of_string: unknown terminal: "^s)))

let mk_term_parser s0 = (
  fun (SS(s,i,j)) -> (
    if s0 = "?eps?" then [SS(s,i,i)] else
    if s0 = "?FIXME?" then [] else
    let m = tokint_of_string s0 in
    if i<j then (
      let n = tokint_of_token (Array.get s i) in
      if (m=n) then 
        [SS(s,i,i+1)]
      else
        [])
    else
      []))

let term_to_parser s = mktmparser s (mk_term_parser s)

