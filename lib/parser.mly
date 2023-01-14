(* Parser for the input file describing levels *)
%{

open Puzzle

%}

%token Rows
%token Cols
%token <int> Number
%token Newline
%token Eof

%start file
%type <puzzle> file

%%

let file :=
|	Newline*; puzzle=puzzle; Eof; < >

let puzzle :=
|	Rows; rows=list(row_or_col);
	Cols; cols=list(row_or_col);
	{
		{ rows = Array.of_list rows; cols = Array.of_list cols }
	}
|	Cols; cols=list(row_or_col);
	Rows; rows=list(row_or_col);
	{
		{ rows = Array.of_list rows; cols = Array.of_list cols }
	}

let row_or_col :=
|	nums=list(Number); Newline; < >
