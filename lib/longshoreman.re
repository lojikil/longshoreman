type lex_t = 
    | LString(string, int, int)
    | LAString(string, int, int)
    | LComment(string, int, int)
    | LSymbol(string, int, int)
    | LSet(string, string, int, int) /* something=somethingelse */
    | LVar(string, int, int)
    | LCompoundVar(string, string, string, int, int) /* ${something...} */
    | LArrayStart(int, int)
    | LArrayEnd(int, int)
    | LComma(int, int)
    | LEOL(int, int)
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
    | Arg(string, option(string))
    | RunCommand(string)
    | RunExec(list(string))
    | CmdArray(list(string)) /* either exec form or arg list to ENTRYPOINT */
    | CmdCommand(string)
    | Env(string)
    | Expose(list(int), list(string))
    | User(string)
    | Workdir(string)
    | Label(list((string, string)))
    | Shell(list(string))
    | Onbuild(t)
    | Maintainer(string)
    | EntrypointCommand(string)
    | EntrypointExec(list(string))
    | AddList(string, list(string), string) /* these all have an optional "--chown" first */
    | Add(string, list(string), string)
    | CopyList(string, list(string), string)
    | Copy(string, list(string), string)
    | Volume(string)
    | StopSignal(string)
    | Error(string)

/*
 * a dockerfile is just an ordered list of
 * docker entries
 */

type dockerfile = list(t)

let string_of_lexeme = fun
    | LString(s, _, _) => "LString(" ++ s ++ ")"
    | LAString(s, _, _) => "LAString(" ++ s ++ ")"
    | LComment(s, _, _) => "LComment(" ++ s ++ ")"
    | LSymbol(s, _, _) => "LSymbol(" ++ s ++ ")"
    | LSet(s, _, _, _) => "LSet(" ++ s ++ ")"
    | LVar(s, _, _)  => "LVar(" ++ s ++ ")"
    | LCompoundVar(s, _, _, _, _) => "LCompoundVar(" ++ s ++ ")"
    | LArrayStart(_, _) => "LArrayStart()"
    | LArrayEnd(_, _) => "LArrayEnd()"
    | LComma(_, _) => "LComma()"
    | LEOL(_, _) => "LEOL()"
    | LEOF(_) => "LEOF()"
    | LError(s, _) => "LError(" ++ s ++ ")"

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
        | n when n == '"' => LString(String.sub(src, start + 1, offset - start - 1), start, offset + 1)
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

let last = (src:list('a)):'a => {
    let l = List.length(src)
    List.nth(src, l - 1)
}

let butlast = (src:list('a)):list('a) => {
    /* would have been easy with a takewhileindex...
     * also would be easy as a tail recursive function
     * with an optional arg...
     */
    let l = List.length(src);
    let res = ref([])
    List.iteri((idx, x) => { if(idx < (l - 1)) { res := List.append(res^, [x]); } else { () } }, src);
    res^;
};

/*
 * I have to think about how to detect comments. For example,
 * this is obvious:
 *
 * [source]
 * ----
 * # This is a comment, you should ignore \
 * # this will be a separate comment
 * ----
 *
 * However, it gets tricky: we cannot mark *ALL* instances of
 * '#' as a comment; to wit: `"this string contains a # character"`
 * should not influence the consumption of lines...
 *
 * Maybe it *is* easier to just return a noisy version and
 * have clean versions atop this?
 */
let consume_line = (~escape:bool=false,
                    ~delimiter:char='\n',
                    ~comment:bool=false,
                    src:string,
                    offset:int):(string, int) => {
    /*
     * we want to read the whole rest of the line, and nothing else
     * there is an analog for things that we want to allow to escape as well...
     *
     * we need to also make sure that if we're in an in-line comment,
     * that we don't consume the escaped line too...
     * for example:
     *
     * [source]
     * ----
     * FROM debian # this comment shouldn't be continued \
     * CMD ls
     * ----
     *
     * Currently, this would smash the two lines together...
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
    /*
    let rec inner_c_c_v = (state:int, ioffset:int):(string, int) => {
        switch((state, String.get(src, ioffset))) {
            | (0, a) when is_symbolic(a) => {
                /* start state */
            }
            | (0, '}') => {

            }
            | (0, ':') => {

            }
            | 1 => {

            }
        }
    }
    inner_c_c_v(0, offset)
    */
    switch((src, offset)) {
        | ("", _) => ("", "", "", offset + 1)
        | (s, n) => (s, "", "", offset + n)
    }
}

let consume_symbol = (src:string, offset:int):(string, int) => {
    let rec inner_c_s = (ioffset:int):(string, int) => {
        switch(String.get(src, ioffset)) {
            | ws when is_whitespace(ws) => {
                (String.sub(src, offset, (ioffset - offset)), ioffset + 1)
            }
            | _ => {
                inner_c_s(ioffset + 1)
            }
            | exception Invalid_argument(_) => {
                (String.sub(src, offset, (ioffset - offset)), ioffset + 1)
            }
        }
    }
    inner_c_s(offset + 1)
}

let rec next = (src:string, offset:int):lex_t => {
    switch(String.get(src, offset)) {
        | '\\' => next(src, offset + 2)
        | '\n' => LEOL(1, offset + 1)
        | c when is_whitespace(c) => next(src, offset + 1)
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
            switch(String.get(src, offset + 1)) {
                | '{' => {
                    let (v, t, a, o) = consume_compound_var(src, offset + 2)
                    LCompoundVar(v, t, a, String.length(v), o)
                }
                | n when is_alpha(n) || n == '_' => {
                    let (v, o) = consume_symbol(src, offset + 1)
                    LVar(v, String.length(v), o)
                }
                | _
                | exception Invalid_argument(_) => {
                    LError("", offset)
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
            let (s, o) = consume_line(~escape=true, ~delimiter='"', src, offset+1)
            LString(s, String.length(s), o)
        }
        | '\'' => {
            /* need to ensure we're not parsing a JSON-style
             * object in here tho...
             */
            let (s, o) = consume_line(~escape=true, ~delimiter='\'', src, offset+1)
            LAString(s, String.length(s), o + 1)
        }
        | _ => {
            let (s, o) = consume_symbol(src, offset)
            LSymbol(s, String.length(s), o)
        }
        | exception Invalid_argument(_) => LEOF(offset)
    }
}

/*
 * this really should return like an option or either here,
 * with one side being the result of the build up and the
 * other being an error...
 */

let consume_array = (src:string, offset:int):list(string) => {
    /*
     * so we want to build up a list of t, and return
     * it, in a way that should be well formatted...
     */
    let rec inner_c_a = (res:list(string), state:int, offset:int):list(string) => {
        let tok = next(src, offset)
        switch((state, tok)) {
            | (-1, LArrayStart(_, o)) => inner_c_a(res, 0, o)
            | (0, LString(s, _, o)) => inner_c_a(List.append(res, [s]), 1, o)
            | (_, LArrayEnd(_, _)) => res
            //| (1, LComma(int, o)) => inner_c_a(res, 0, o)
            // ^^^ should be an error, isn't caught...
            | (1, LComma(_, o)) => inner_c_a(res, 0, o)
            | _ => {
                print_endline("state: " ++ string_of_int(state));
                print_endline("token: " ++ string_of_lexeme(tok));
                ["We really need to return an error here"]
            }
        }
    }
    inner_c_a([], -1, offset)
}

let consume_quoted_array = (src:string, offset:int):list(string) => {
    /*
     * so we want to build up a list of t, and return
     * it, in a way that should be well formatted...
     */
    let rec inner_c_a = (res:list(string), offset:int):list(string) => {
        let tok = next(src, offset)
        switch(tok) {
            | LString(s, _, o) => inner_c_a(List.append(res, [s]), o)
            | LSymbol(s, _, o) => inner_c_a(List.append(res, [s]), o)
            | LEOF(_)
            | LEOL(_, _) => res
            //| (1, LComma(int, o)) => inner_c_a(res, 0, o)
            // ^^^ should be an error, isn't caught...
            | _ => {
                print_endline("token: " ++ string_of_lexeme(tok));
                ["We really need to return an error here"]
            }
        }
    }
    inner_c_a([], offset)
}

let safe_index = (src:string, needle:char):int => {
    switch(String.index(src, needle)) {
        | n => n
        | exception Not_found => -1
    }
}

let extract_name = (n:string):(string, string, string) => {
    switch((safe_index(n, '@'), safe_index(n, ':'))) {
        | (-1, -1) => (n, "", "")
        | (-1, m) => (String.sub(n, 0, m), String.sub(n, m + 1, String.length(n) - m - 1), "")
        | (a, _) => (String.sub(n, 0, a), "", String.sub(n, a + 1, String.length(n) - a - 1))
    }
}


let build_from = (src:string, offset:int):from_image => {
    let l = consume_quoted_array(src, offset)
    switch(l) {
        | [n] => {
            let (name, tag, digest) = extract_name(n)
            make_from_image(~tag=tag, ~digest=digest, name)
        }
        | [p, n] => {
            let (name, tag, digest) = extract_name(n)
            make_from_image(~platform=p, ~tag=tag, ~digest=digest, name)
        }
        | [n, "AS", nm] => {
            let (name, tag, digest) = extract_name(n)
            make_from_image(~tag=tag, ~digest=digest, ~name=nm, name)
        }
        | [p, n, "AS", nm] => {
            let (name, tag, digest) = extract_name(n)
            make_from_image(~platform=p, ~tag=tag, ~digest=digest, ~name=nm, name)
        }
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
        /*
         * this way is cleaner, but the problem is
         * that the language doesn't _actually_ require
         * that commands are upper case...
         */
        | LSymbol("FROM", _, o) => {
            let members = build_from(src, o)
            From(members)
        }
        | LSymbol("CMD", _, o)  => {
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
            switch(next(src, o)) {
                | LString(s, _, _) => CmdCommand(s)
                | LAString(las, _, _) => CmdCommand(las)
                | LSymbol(_, _, _) => {
                    let (v, o) = consume_line(src, o)
                    CmdCommand(v)
                }
                | LArrayStart(_, _) => CmdArray(consume_array(src, o))
                | _ => Comment("not implemented")
            }
        }
        | LSymbol("RUN", _, o) => {
            switch(next(src, o)) {
                | LString(s, _, _) => RunCommand(s)
                | LAString(las, _, _) => RunCommand(las)
                | LSymbol(_, _, _) => {
                    let (v, o) = consume_line(src, o)
                    RunCommand(v)
                }
                | LArrayStart(_, _) => RunExec(consume_array(src, o))
                | _ => Comment("not implemented")
            }
        }
        | LSymbol("ENTRYPOINT", _, o) => {
            switch(next(src, o)) {
                | LString(s, _, _) => EntrypointCommand(s)
                | LAString(las, _, _) => EntrypointCommand(las)
                | LSymbol(_, _, _) => {
                    let (v, o) = consume_line(src, o)
                    EntrypointCommand(v)
                }
                | LArrayStart(_, _) => EntrypointExec(consume_array(src, o))
                | _ => Comment("not implemented")
            }
        }
        | LSymbol("SHELL", _, o) => {
            switch(next(src, o)) {
                | LArrayStart(_, _) =>  Shell(consume_array(src, o)) 
                | _ => Error("SHELL *must* be followed by an array")
            }
        }
        | LSymbol("USER", _, o) => {
            let u = next(src, o)
            switch(u) {
                | LString(user, _, _) => User(user)
                | LSymbol(usym, _, _) => User(usym)
                | _ => Error("expected string or symbol after USER")
            }
        }
        | LSymbol("WORKDIR", _, o) => {
            let w = next(src, o)
            switch(w) {
                | LString(workdir, _, _) => Workdir(workdir)
                | LAString(lawork, _, _) => Workdir(lawork)
                | LSymbol(wsym, _, _) => Workdir(wsym)
                | _ => Error("expected string or symbol after WORKDIR")
            }
        }
        | LSymbol("VOLUME", _, o) => {
            let v = next(src, o)
            switch(v) {
                | LString(workdir, _, _) => Volume(workdir)
                | LAString(lawork, _, _) => Volume(lawork)
                | LSymbol(wsym, _, _) => Volume(wsym)
                | _ => Error("expected string or symbol after VOLUME")
            }
        }
        | LSymbol("STOPSIGNAL", _, o) => {
            let signal = next(src, o)
            switch(signal) {
                | LSymbol(sigsym, _, _) => StopSignal(sigsym)
                | _ => Error("STOPSIGNAL expects a signal name/number")
            }
        }
        | LSymbol("ARG", _, o) => {
            /*
             * honestly, it seems better to just
             * . consume-line here; ARG doesn't really care about what's going on
             * . do the same in ENV, but attempt to suss out what's been added on the line
             *
             * I've been avoiding making a full AST thus far, but it's really tempting...
             */
            let a = next(src, o)
            switch(a) {
                | LSymbol(asym, _, _) => {
                    switch(String.index(asym, '=')) {
                        | n => {
                            Arg(String.sub(asym, 0, n), Some(String.sub(asym, n + 1, String.length(asym) - n - 1)))
                        }
                        | exception Not_found => {
                            Arg(asym, None)
                        }
                    }
                }
                | _ => Error("expected symbol after ARG")
            }
        }
        | LSymbol("ENV", _, o) => {
            let (env_val, o1) = consume_line(~escape=true, src, o)
            Env(env_val)
        }
        | LSymbol("ADD", _, o) => {
            let s = next(src, o)
            /*
             * so what we really want is:
             * . optional `--chown=...`
             * . either a list of values until EOL
             * . or a bounded array
             *
             * this is where really nice monadic solutions come out
             * on top, because you can sorta optionally read and return;
             * I could do the same here, like "arg or array start or list"
             * really, that wouldn't be terrible...
             */
            switch(s) {
                | LSymbol(ch, _, no) when String.get(ch, 0) == '-' => {
                    /*
                     * we have a chown here...
                     */
                    let ns = next(src, no)
                    switch(ns) {
                        | LSymbol(_, _, _) => {
                            let res = consume_quoted_array(src, no);
                            let add_list = butlast(res);
                            let dest = last(res);
                            Add(ch, add_list, dest)
                        }
                        | LArrayStart(_, _) => {
                            let res = consume_array(src, no);
                            let add_list = butlast(res);
                            let dest = last(res);
                            AddList(ch, add_list, dest)
                        }
                        | _ => {
                            Error("Mismatched type in ADD")
                        }
                    }
                }
                | LSymbol(_, _, _) => {
                    let res = consume_quoted_array(src, o);
                    let add_list = butlast(res);
                    let dest = last(res);
                    Add("", add_list, dest)
                }
                | LArrayStart(_, _) => {
                    let res = consume_array(src, o);
                    let add_list = butlast(res);
                    let dest = last(res);
                    AddList("", add_list, dest)
                }
                | _ => Error("unexpected type in ADD")
            }
        }  
        | LSymbol("COPY", _, o) => {
            /* again, a monadic interface here that
             * captured state of where we are, and then
             * what we should do next would be great, esp
             * because it would completely eliminate this
             * code duplication below...
             */
            let s = next(src, o)
            /*
             * so what we really want is:
             * . optional `--chown=...`
             * . either a list of values until EOL
             * . or a bounded array
             *
             * this is where really nice monadic solutions come out
             * on top, because you can sorta optionally read and return;
             * I could do the same here, like "arg or array start or list"
             * really, that wouldn't be terrible...
             */
            switch(s) {
                | LSymbol(ch, _, no) when String.get(ch, 0) == '-' => {
                    /*
                     * we have a chown here...
                     */
                    let ns = next(src, no)
                    switch(ns) {
                        | LSymbol(_, _, _) => {
                            let res = consume_quoted_array(src, no);
                            let add_list = butlast(res);
                            let dest = last(res);
                            Copy(ch, add_list, dest)
                        }
                        | LArrayStart(_, _) => {
                            let res = consume_array(src, no);
                            let add_list = butlast(res);
                            let dest = last(res);
                            CopyList(ch, add_list, dest)
                        }
                        | _ => {
                            Error("Mismatched type in COPY")
                        }
                    }
                }
                | LSymbol(_, _, _) => {
                    let res = consume_quoted_array(src, o);
                    let add_list = butlast(res);
                    let dest = last(res);
                    Copy("", add_list, dest)
                }
                | LArrayStart(_, _) => {
                    let res = consume_array(src, o);
                    let add_list = butlast(res);
                    let dest = last(res);
                    CopyList("", add_list, dest)
                }
                | _ => Error("unexpected type in COPY")
            }
        }  
        | _ => {
            Comment("unimplemented feature")
        }
    }
}

let string_of_docker = fun
    | Comment(c) => "# " ++ c
    | Arg(k, v) => {
        switch(v) {
            | Some(s) =>  "ARG " ++ k ++ "=" ++ s
            | None => "ARG " ++ k
        }
    }
    | Env(s) => {
        "ENV " ++ s
    }
    | Copy(chown, al, d) => "COPY " ++ chown ++ " " ++ String.concat(" ", al) ++ " " ++ d
    | CopyList(chown, al, d) => "COPY " ++ chown ++ " [" ++ String.concat(" ", List.map((x) => { "\"" ++ String.escaped(x) ++ "\"" }, al)) ++ ", " ++ String.escaped(d) ++ "]"
    | Add(chown, al, d) => "ADD " ++ chown ++ " " ++ String.concat(" ", al) ++ " " ++ d
    | AddList(chown, al, d) => "ADD " ++ chown ++ " [" ++ String.concat(" ", List.map((x) => { "\"" ++ String.escaped(x) ++ "\"" }, al)) ++ ", " ++ String.escaped(d) ++ "]"
    | RunCommand(rc) => "RUN " ++ rc
    | RunExec(re) => "RUN [" ++ String.concat(", ", List.map((x) => { "\"" ++ String.escaped(x) ++ "\"" }, re)) ++ "]"
    | Shell(re) => "SHELL [" ++ String.concat(", ", List.map((x) => { "\"" ++ String.escaped(x) ++ "\"" }, re)) ++ "]"
    | User(u) => "USER " ++ u
    | Volume(v) => "VOLUME " ++ v
    | Workdir(w) => "WORKDIR " ++ w
    | CmdCommand(cmd) => "CMD " ++ cmd
    | CmdArray(ca) => "CMD [" ++ String.concat(", ", List.map((x) => { "\"" ++ String.escaped(x) ++ "\"" }, ca)) ++ "]"
    | _ => "# unimplemented"
