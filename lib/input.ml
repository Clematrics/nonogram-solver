let read_file filepath =
  let chn = open_in filepath in
  let lexbuf = Lexing.from_channel chn in
  Parser.file Lexer.lex lexbuf
