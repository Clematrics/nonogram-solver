(* Lexer for the input file describing a nonogram puzzle *)
{
	open Parser
}

let comment_start = "//"
let rows_mark = "ROWS"
let cols_mark = "COLS"
let blanks = [' ' '\t']+
let num = [ '0'-'9' ]+

rule lex = parse
| blanks     { lex lexbuf }
| comment_start [^'\n']* ('\n' | eof)
	{ lex lexbuf }
| rows_mark '\n'+  { Rows }
| cols_mark '\n'+  { Cols }
| num as i   { Number (int_of_string i) }
| '\n'+      { Newline }
| eof        { Eof }
