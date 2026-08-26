type case = Upper of Uchar.t | Lower of Uchar.t

let known_chars : (Uchar.t, case) Hashtbl.t = Hashtbl.create 32

let () =
  List.iter
    (fun (upper, lower) ->
      let upper = Uchar.of_int upper and lower = Uchar.of_int lower in
      Hashtbl.add known_chars upper (Upper lower);
      Hashtbl.add known_chars lower (Lower upper))
    [
      0xc0, 0xe0;
      0xc1, 0xe1;
      0xc2, 0xe2;
      0xc3, 0xe3;
      0xc4, 0xe4;
      0xc5, 0xe5;
      0xc6, 0xe6;
      0xc7, 0xe7;
      0xc8, 0xe8;
      0xc9, 0xe9;
      0xca, 0xea;
      0xcb, 0xeb;
      0xcc, 0xec;
      0xcd, 0xed;
      0xce, 0xee;
      0xcf, 0xef;
      0xd0, 0xf0;
      0xd1, 0xf1;
      0xd2, 0xf2;
      0xd3, 0xf3;
      0xd4, 0xf4;
      0xd5, 0xf5;
      0xd6, 0xf6;
      0xd8, 0xf8;
      0xd9, 0xf9;
      0xda, 0xfa;
      0xdb, 0xfb;
      0xdc, 0xfc;
      0xdd, 0xfd;
      0xde, 0xfe;
      0x160, 0x161;
      0x17d, 0x17e;
      0x152, 0x153;
      0x178, 0xff;
      0x1e9e, 0xdf;
    ]

let known_pairs : (Uchar.t * Uchar.t, Uchar.t) Hashtbl.t =
  Hashtbl.create 64

let () =
  List.iter
    (fun (base, combining, normalized) ->
      Hashtbl.add known_pairs
        (Uchar.of_char base, Uchar.of_int combining)
        (Uchar.of_int normalized))
    [
      'A', 0x300, 0xc0;
      'A', 0x301, 0xc1;
      'A', 0x302, 0xc2;
      'A', 0x303, 0xc3;
      'A', 0x308, 0xc4;
      'A', 0x30a, 0xc5;
      'C', 0x327, 0xc7;
      'E', 0x300, 0xc8;
      'E', 0x301, 0xc9;
      'E', 0x302, 0xca;
      'E', 0x308, 0xcb;
      'I', 0x300, 0xcc;
      'I', 0x301, 0xcd;
      'I', 0x302, 0xce;
      'I', 0x308, 0xcf;
      'N', 0x303, 0xd1;
      'O', 0x300, 0xd2;
      'O', 0x301, 0xd3;
      'O', 0x302, 0xd4;
      'O', 0x303, 0xd5;
      'O', 0x308, 0xd6;
      'U', 0x300, 0xd9;
      'U', 0x301, 0xda;
      'U', 0x302, 0xdb;
      'U', 0x308, 0xdc;
      'Y', 0x301, 0xdd;
      'Y', 0x308, 0x178;
      'S', 0x30c, 0x160;
      'Z', 0x30c, 0x17d;
      'a', 0x300, 0xe0;
      'a', 0x301, 0xe1;
      'a', 0x302, 0xe2;
      'a', 0x303, 0xe3;
      'a', 0x308, 0xe4;
      'a', 0x30a, 0xe5;
      'c', 0x327, 0xe7;
      'e', 0x300, 0xe8;
      'e', 0x301, 0xe9;
      'e', 0x302, 0xea;
      'e', 0x308, 0xeb;
      'i', 0x300, 0xec;
      'i', 0x301, 0xed;
      'i', 0x302, 0xee;
      'i', 0x308, 0xef;
      'n', 0x303, 0xf1;
      'o', 0x300, 0xf2;
      'o', 0x301, 0xf3;
      'o', 0x302, 0xf4;
      'o', 0x303, 0xf5;
      'o', 0x308, 0xf6;
      'u', 0x300, 0xf9;
      'u', 0x301, 0xfa;
      'u', 0x302, 0xfb;
      'u', 0x308, 0xfc;
      'y', 0x301, 0xfd;
      'y', 0x308, 0xff;
      's', 0x30c, 0x161;
      'z', 0x30c, 0x17e;
    ]

let valid_decode decode uchar =
  Uchar.utf_decode_is_valid decode && uchar <> Uchar.rep

let rec normalize_from buffer valid source previous offset =
  if offset >= String.length source then (
    Buffer.add_utf_8_uchar buffer previous;
    valid)
  else
    let decode = String.get_utf_8_uchar source offset in
    let uchar = Uchar.utf_decode_uchar decode in
    let valid = valid && valid_decode decode uchar in
    let next = offset + Uchar.utf_decode_length decode in
    match Hashtbl.find_opt known_pairs (previous, uchar) with
    | Some normalized ->
        normalize_from buffer valid source normalized next
    | None ->
        Buffer.add_utf_8_uchar buffer previous;
        normalize_from buffer valid source uchar next

let normalize source =
  if
    source = ""
    || String.for_all (fun char -> Char.code char < 0x80) source
  then Ok source
  else
    let decode = String.get_utf_8_uchar source 0 in
    let first = Uchar.utf_decode_uchar decode in
    let buffer = Buffer.create (String.length source) in
    let valid = valid_decode decode first in
    let offset = Uchar.utf_decode_length decode in
    let valid = normalize_from buffer valid source first offset in
    let normalized = Buffer.contents buffer in
    if valid then Ok normalized else Error normalized

let uchar_is_uppercase uchar =
  let code = Uchar.to_int uchar in
  if code < 0x80 then code >= Char.code 'A' && code <= Char.code 'Z'
  else
    match Hashtbl.find_opt known_chars uchar with
    | Some (Upper _) -> true
    | Some (Lower _) | None -> false

let is_capitalized source =
  source <> ""
  && uchar_is_uppercase
       (Uchar.utf_decode_uchar (String.get_utf_8_uchar source 0))

let uchar_valid_in_identifier ~with_dot uchar =
  let code = Uchar.to_int uchar in
  if code < 0x80 then
    (code >= Char.code 'a' && code <= Char.code 'z')
    || (code >= Char.code 'A' && code <= Char.code 'Z')
    || (code >= Char.code '0' && code <= Char.code '9')
    || code = Char.code '_'
    || code = Char.code '\''
    || (with_dot && code = Char.code '.')
  else Hashtbl.mem known_chars uchar

let uchar_not_identifier_start uchar =
  let code = Uchar.to_int uchar in
  (code >= Char.code '0' && code <= Char.code '9')
  || code = Char.code '\''

type validation_result =
  | Valid
  | Invalid_character of Uchar.t
  | Invalid_beginning of Uchar.t

let validate_identifier ?(with_dot = false) source =
  let rec check offset =
    if offset >= String.length source then Valid
    else
      let decode = String.get_utf_8_uchar source offset in
      let uchar = Uchar.utf_decode_uchar decode in
      let next = offset + Uchar.utf_decode_length decode in
      if not (uchar_valid_in_identifier ~with_dot uchar) then
        Invalid_character uchar
      else if offset = 0 && uchar_not_identifier_start uchar then
        Invalid_beginning uchar
      else check next
  in
  check 0

let is_lowercase source =
  let rec check offset =
    if offset >= String.length source then true
    else
      let decode = String.get_utf_8_uchar source offset in
      let uchar = Uchar.utf_decode_uchar decode in
      uchar_valid_in_identifier ~with_dot:false uchar
      && (not (uchar_is_uppercase uchar))
      && check (offset + Uchar.utf_decode_length decode)
  in
  check 0
