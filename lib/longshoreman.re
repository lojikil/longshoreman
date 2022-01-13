type lex_t = 
    | LString(string, int, int)
    | LAString(string, int, int)
    | LComment(string, int, int)
    | LSymbol(string, int, int)
    | LSet(string, string, int, int) /* something=somethingelse */
    | LVar(string, int, int)
    | LCompoundVar(string, int, int) /* ${something...} */
    | LArrayStart(int, int)
    | LArrayEnd(int, int)
    | LComma(int, int)
    | LEOF(int)
    | LError(string, int)

type from_image = {
    platform:string,
    image:string,
    tag:string,
    digest:string,
    name:string
}

/*
 * this is a little bit more verbose than what's going on in Ocaml-Dockerfile,
 * but it's also a little easier to interact with; you don't have to introspect
 * second level types (`from_image` aside) in order to know what type of COPY,
 * ADD, &c you are working with.
 */

type t = 
    | Comment(string)
    | From(from_image)
    | Arg(string, string)
    | RunCommand(string)
    | RunExec(list(string))
    | CmdArray(list(string)) /* either exec form or arg list to ENTRYPOINT */
    | CmdCommand(list(string))
    | Env(list((string, string)))
    | Expose(list(int), list(string))
    | User(string)
    | Workdir(string)
    | Label(list(string, string))
    | Shell(list(string))
    | Onbuild(t)
    | Maintainer(string)
    | EntrypointCommand(string)
    | EntrypointExec(list(string))
    | AddList(string, list(string), string)
    | Add(string, list(string), string)
    | CopyList(string, list(string), string)
    | Copy(string, list(string), string)

/*
 * a dockerfile is just an ordered list of
 * docker entries
 */

type dockerfile = list(t)

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

let is_alpha = (c:char):bool => {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
}

let rec take_while_string = (src:string, start:int, offset:int, skip_escape:bool): lex_t => {
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

let make_from_image = (~platform:string="", ~tag:string="", ~digest:string="",~name:string="", image:string):from_image => {
    {
        platform:platform,
        image:image,
        tag:tag,
        digest:digest,
        name:name
    }
}

let consume_line = (~escape:bool=false, ~delimiter:char='\n', src:string, offset:int):(string, int) => {
    /*
     * we want to read the whole rest of the line, and nothing else
     * there is an analog for things that we want to allow to escape as well...
     */
    let rec int_c_l = (ioffset:int):(string, int) => {
        switch(String.get(src, ioffset)) {
            | d when d == delimiter => {
                (String.sub(src, offset, (ioffset - offset)), ioffset + 1)
            }
            | '\\' when escape => int_c_l(ioffset + 2)
            | _ => int_c_l(ioffset + 1)
            | exception Invalid_argument(_) => {
                (String.sub(src, offset, (ioffset - offset)), ioffset - 1)
            }
        }
    }
    int_c_l(offset)
}

let consume_compound_var = (src:string, offset:int):(string, string, string, int) => {
    ("", "", "", offset + 1)
}

let consume_symbol = (src:string, offset:int):(string, int) => {
    ("", 0)
}

let rec next = (src:string, offset:int):lex_t => {
    switch(String.get(src, offset)) {
        | c when iswhitespace(c) => next(src, offset + 1)
        | a when is_alpha(a) => {
            let (s, o) = consume_symbol(src, offset)
            LSymbol(s, String.length(s), o)
        }
        | '#' => {
            /*
             * a comment, read the rest of the current line, return it
             * need to actually test these because they can include
             * syntactic directives, but not doing that just yet...
             */
            let (cline, coffset) = consume_line(src, offset + 1)
            LComment(cline, String.length(cline), coffset)
        }
        | '$' => {
            /* need consume variable and the like here, sorta like expect
             *
             * so, we could just add an explicit state above, but that
             * might complicate things here. I think a substate wouldn't
             * be terrible, considering that it is either:
             *
             * . '{' for a compound variable
             * . '[a-zA-Z]+' for a regular variable (as a starting char)
             * . an error of some kind
             */
            switch(String.get(src, ioffset + 1)) {
                | '{' => {
                    let (v, t, a, o) = consume_compound_var(src, ioffset + 2)
                    LCompoundVar(v, t, a, String.length(v), o)
                }
                | n when is_alpha(n) || n == '_' => {
                    let (v, o) = consume_symbol(src, ioffset + 1)
                    LVar(v, String.length(v), o)
                }
                | _
                | exception Invalid_argument(_) => {
                    LError(ioffset)
                }
            }
        }
        | '[' => {
            LArrayStart(1, offset + 1)
        }
        | ']' => {
            LArrayEnd(1, offset + 1)
        }
        | ',' => {
            LComma(1, offset + 1)
        }
        | '"' => {
            let (s, o) = consume_line(~escape=true, ~delimiter='"', src, ioffset+1)
            LString(s, String.length(s), o + 1)
        }
        | '\'' => {
            /* need to ensure we're not parsing a JSON-style
             * object in here tho...
             */
            let (s, o) = consume_line(~escape=true, ~delimiter='\'', src, ioffset+1)
            LAString(s, String.length(s), o + 1)
        }
        | exception Invalid_argument(_) => LEOF(offset)
    }
}
/*
 * I probably should use an expect-style system here to
 * keep things a bit cleaner; at the tope level, we can
 * expect either a comment or a symbol to start, and then
 * depending on the type of symbol, we can read the rest
 * what we expect
 * this could be good because we can map it to the grammar
 * nicely, like:
 * "[" ATOM "]" / "," LIST
 */
let docker_of_line = (src:string, offset:int):t => {
    let init = next(src, offset)
    switch(init) {
        | LComment(c, _, o) => Comment(c)
        | LSymbol(cmd, _, o) when String.upper(cmd) == "CMD" => {
            /*
             * this is one way of handling that; it doesn't require me to
             * maintain a state machine for each keyword, and also doesn't
             * introduce a stateful requirement of knowing where we are in
             * the lex stack or accidentally returning a keyword for a user
             * command...
             *
             * it's definitely not my favorite tho
             *
             * so here, what we can do is next, see if we get a list
             * start, if we do, call _get list_ and if not, call
             * _get shell_. I definitely understand why the OCaml
             * maintainer went the direction they did
             *
             * if you really think about it tho, we don't actually need
             * to parse the rest of the line, we can just call a "get rest of line"
             * function, that can break on newline or comment really
             */

        }
        | _ => {
            Comment("unimplemented feature")
        }
    }
}

let string_of_docker = fun
    | Comment(c) => "# " ++ c
    | Arg(k, v) => "ARG " ++ k ++ "=" ++ v
    | RunCommand(rc) => "RUN " ++ rc
    | RunExec(re) => "RUN [" ++ String.concat(" ", List.map((x) => { "\"" ++ String.escaped(x) ++ "\"" }, re)) ++ "]"
    | _ => "# unimplemented"
