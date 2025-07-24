open Lwt.Infix
open Cohttp_lwt_unix

let bold = "\027[1m"
let reset = "\027[0m"
let red = "\027[31m"
let green = "\027[32m"
let yellow = "\027[33m"

let rot13_char c =
  let base = if Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' then Char.code 'a'
             else if Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z' then Char.code 'A'
             else -1 in
  if base >= 0 then
    Char.chr (((Char.code c - base + 13) mod 26) + base)
  else
    c

let rot13 str = String.map rot13_char str

let fetch_quote () =
  let uri = Uri.of_string "https://api.quotable.io/random" in
  Client.get uri >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body >|= fun quote_json ->
  let quote = Yojson.Basic.from_string quote_json in
  let content = Yojson.Basic.Util.(quote |> member "content" |> to_string) in
  let author = Yojson.Basic.Util.(quote |> member "author" |> to_string) in
  (content, author)

let check_partial_guess original guess =
  let rec check_words o g partial =
    match o, g with
    | [], [] | [], _ | _, [] -> partial
    | o1::os, g1::gs ->
      if o1 = g1 then check_words os gs true
      else check_words os gs partial
  in
  let o_words = String.split_on_char ' ' original in
  let g_words = String.split_on_char ' ' guess in
  check_words o_words g_words false

let main () =
  Lwt_main.run (
    fetch_quote () >>= fun (content, author) ->
    let encrypted = rot13 content in
    Printf.printf "%sAuthor:%s %s\n" bold reset author;
    Printf.printf "%sEncrypted Quote:%s %s\n\n" bold reset (red ^ encrypted ^ reset);
    print_endline "Can you guess the original quote? (Type your guess or 'give up' to reveal the answer)";

    let rec loop () =
      let guess = read_line () in
      if String.lowercase_ascii guess = "give up" then (
        Printf.printf "%sThe original quote is:%s %s\n" bold reset content;
        Lwt.return ()
      ) else if guess = content then (
        Printf.printf "%sCongratulations! Your guess is correct!%s\n" green reset;
        Lwt.return ()
      ) else if check_partial_guess content guess then (
        Printf.printf "%sPartially Correct! Keep Going.%s\n" yellow reset;
        let regex = Str.regexp (rot13 guess) in
        let partial = Str.replace_first regex guess encrypted in
        Printf.printf "%sPartially Decrypted Quote:%s %s\n\n" bold reset (red ^ partial ^ reset);
        loop ()
      ) else (
        Printf.printf "%sIncorrect. Try again or type 'give up' to reveal the answer.%s\n" red reset;
        loop ()
      )
    in
    loop ()
  )
