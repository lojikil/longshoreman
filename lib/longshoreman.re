type lex_t = 
    | LString(string, int, int)
    | LComment(string, int, int)
    | LSymbol(string, int, int)
    | LSet(string, string, int, int) /* something=somethingelse */
    | LVar(string, int, int)

type from_image = {
    platform:string,
    image:string,
    tag:string,
    digest:string,
    name:string
}

type t = 
    | Comment(string)
    | From(from_image)
    | Arg(string, string)
    | RunCommand(string)
    | RunExec(list(string))
    | CmdArray(list(string)) /* either exec form or arg list to ENTRYPOINT */
    | CmdCommand(list(string))
    | Env(list((string, string)))

let is_numeric = (c:char):bool => {
    let r = Char.compare('0', c);
    r >= -9 && r <= 0
}

let is_whitespace = (c:char):bool => {
    Char.compare(c, ' ') == 0 || Char.compare(c, '\t') == 0 || Char.compare(c, '\n') == 0 || Char.compare(c, '\r') == 0
}

let is_bracket = (c:char):bool => {
    Char.compare(c, '{') == 0 || Char.compare(c, '}') == 0 || Char.compare(c, '[') == 0 || Char.compare(c, ']') == 0
}

let is_break = (c:char):bool => {
    Char.compare(c, ',') == 0 || Char.compare(c, ':') == 0 || is_whitespace(c) || is_bracket(c)
}

let rec take_while_string = (src:string, start:int, offset:int, skip_escape:bool): lex_state => {
    switch(String.get(src, offset)) {
        | _ when skip_escape => take_while_string(src, start, offset + 1, false)
        | n when n == '"' => LexString(String.sub(src, start + 1, offset - start - 1), start, offset + 1)
        | e when e == '\\' => take_while_string(src, start, offset + 1, true)
        | _ => take_while_string(src, start, offset + 1, false)
    }
}

let rec take_until_not_white = (src:string, offset:int):int => {
    switch(String.get(src, offset)) {
        | n when is_whitespace(n) => take_until_not_white(src, offset + 1)
        | _ => offset
    }
}

let string_of_docker = fun
    | Comment(c) => "# " ++ c
    | Arg(k, v) => "ARG " ++ k ++ "=" ++ v
    | RunCommand(rc) => "RUN " ++ rc
    | RunExec(re) => "RUN [" ++ String.concat(" ", List.map((x) => { "\"" ++ String.escaped(x) ++ "\"" }, re)) ++ "]"
    | _ => "# unimplemented"
