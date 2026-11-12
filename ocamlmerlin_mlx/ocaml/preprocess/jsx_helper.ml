# 1 "mlx/jsx_helper.ml"
open Printf
open Asttypes
open Longident
open Parsetree
open Ast_helper

let make_loc (startpos, endpos) =
  {
    Location.loc_start = startpos;
    Location.loc_end = endpos;
    Location.loc_ghost = false;
  }

let mkexp ~loc d = Exp.mk ~loc:(make_loc loc) d

let mkjsxexp ~loc:loc' e =
  let e = mkexp ~loc:loc' e in
  let loc = make_loc loc' in
  let pexp_attributes = [ Attr.mk ~loc { txt = "JSX"; loc } (PStr []) ] in
  { e with pexp_attributes }

let mkjsxspread ~loc e =
  let loc = make_loc loc in
  let attr = Attr.mk ~loc { txt = "JSX"; loc } (PStr []) in
  { e with pexp_attributes = e.pexp_attributes @ [ attr ] }

let rec equal_longindent a b =
  match a, b with
  | Longident.Lident a, Longident.Lident b -> String.equal a b
  | Ldot (pa, a), Ldot (pb, b) ->
      String.equal a.txt b.txt && equal_longindent pa.txt pb.txt
  | Lapply _, _ | _, Lapply _ -> assert false
  | _ -> false

let jsx_name ~raise segments =
  let syntax_error loc = raise Syntaxerr.(Error (Other (make_loc loc))) in
  let text (segment, loc) =
    match segment with
    | `Word s | `Int (s, None) | `Prefix ("@" as s) -> s
    | `Op (("mod" | "land" | "lor" | "lxor" | "lsl" | "lsr" | "asr") as s) ->
        s
    | `Op s | `Int (s, Some _) | `Prefix s ->
        syntax_error loc;
        s
  in
  let rec check_adjacent = function
    | (_, (_, end_)) :: ((_, ((start, _) as loc)) :: _ as rest) ->
        if end_.Lexing.pos_cnum <> start.Lexing.pos_cnum then syntax_error loc
        else check_adjacent rest
    | _ -> ()
  in
  check_adjacent segments;
  String.concat "" (List.map text segments)

let jsx_label name =
  String.map (function '-' | ':' | '.' | '@' -> '_' | c -> c) name

let with_jsx_name ~loc name expr =
  let loc = make_loc loc in
  let payload =
    PStr [ Str.eval ~loc (Exp.constant ~loc (Const.string ~loc name)) ]
  in
  let attr = Attr.mk ~loc { txt = "mlx.name"; loc } payload in
  { expr with pexp_attributes = expr.pexp_attributes @ [ attr ] }

let make_jsx_element ~raise ~loc:_ ~tag ~end_tag ~props ~children
    ~nested_children () =
  let () =
    match end_tag with
    | None -> ()
    | Some (end_tag, (_, end_loc_e)) ->
        let eq =
          match tag, end_tag with
          | (`Module, _, s), (`Module, _, e) -> equal_longindent s e
          | (`Value, _, s), (`Value, _, e) -> equal_longindent s e
          | _ -> false
        in
        if not eq then
          let _, (end_loc_s, _), _ = end_tag in
          let end_loc = end_loc_s, end_loc_e in
          let _, start_loc, tag = tag in
          let tag = Longident.flatten tag |> String.concat "." in
          raise
            Syntaxerr.(
              Error
                (Unclosed
                   ( make_loc start_loc,
                     sprintf "<%s>" tag,
                     make_loc end_loc,
                     sprintf "</%s>" tag )))
  in
  let () =
    let children_prop =
      List.find_map
        (function
          | loc, (`Prop_punned "children" | `Prop ("children", _)) ->
              Some (loc, "children")
          | loc, (`Prop_opt_punned "children" | `Prop_opt ("children", _)) ->
              Some (loc, "?children")
          | _ -> None)
        props
    in
    match children_prop, nested_children with
    | Some (prop_loc, prop), Some nested_loc ->
        let _, _, tag = tag in
        let tag = Longident.flatten tag |> String.concat "." in
        raise
          (Location.Error
             (Location.errorf ~loc:(make_loc nested_loc)
                ~sub:
                  [
                    Location.msg ~loc:(make_loc prop_loc)
                      "The %a prop is set here." Misc.Style.inline_code prop;
                  ]
                "The element %a has nested children, but its %a prop is \
                 already set.@ Pass the children either as the prop or \
                 between the tags, not both."
                Misc.Style.inline_code (sprintf "<%s>" tag)
                Misc.Style.inline_code prop))
    | _ -> ()
  in
  let tag =
    match tag with
    | `Value, loc, txt -> (
        let ident txt = mkexp ~loc (Pexp_ident { loc = make_loc loc; txt }) in
        match txt with
        | Lident name when not (String.equal (jsx_label name) name) ->
            with_jsx_name ~loc name (ident (Lident (jsx_label name)))
        | _ -> ident txt)
    | `Module, loc, txt ->
        let lloc = make_loc loc in
        let txt =
          Longident.Ldot
            ({ txt; loc = lloc }, { txt = "createElement"; loc = lloc })
        in
        mkexp ~loc (Pexp_ident { loc = lloc; txt })
  in
  let props =
    let prop_exp ~loc name =
      let id = Location.mkloc (Lident name) (make_loc loc) in
      mkexp ~loc (Pexp_ident id)
    in
    let punned ~loc name =
      let label = jsx_label name in
      if not (String.equal label name) then
        raise
          (Location.Error
             (Location.errorf ~loc:(make_loc loc)
                "The prop %a has no value.@ Write %a instead."
                Misc.Style.inline_code name Misc.Style.inline_code
                (name ^ "=...")));
      label, prop_exp ~loc label
    in
    let valued ~loc name expr =
      let label = jsx_label name in
      if String.equal label name then label, expr
      else label, with_jsx_name ~loc name expr
    in
    List.map
      (function
        | loc, `Prop_punned name ->
            let label, expr = punned ~loc name in
            Labelled label, expr
        | loc, `Prop_opt_punned name ->
            let label, expr = punned ~loc name in
            Optional label, expr
        | loc, `Prop (name, expr) ->
            let label, expr = valued ~loc name expr in
            Labelled label, expr
        | loc, `Prop_opt (name, expr) ->
            let label, expr = valued ~loc name expr in
            Optional label, expr)
      props
  in
  let unit =
    Exp.mk ~loc:Location.none
      (Pexp_construct ({ txt = Lident "()"; loc = Location.none }, None))
  in
  let has_children =
    List.exists
      (fun (label, _) ->
        match label with
        | Labelled "children" | Optional "children" -> true
        | _ -> false)
      props
  in
  let props =
    if has_children then props else (Labelled "children", children) :: props
  in
  Pexp_apply (tag, (Nolabel, unit) :: props)
