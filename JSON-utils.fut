import "lib/github.com/diku-dk/segmented/segmented"
import "JSON-datatype"

--{"foo": 1, "bar": 2, "baz": 3}

type pJSON = #json JSON | #lbracket | #rbracket | #lclamp | #rclamp | #colon | #comma

def to_pJSON (j: JSON) : pJSON = #json j

--print function assumes the input is wellformed
def print_JSON (r:i64, js:[]JSON, keys:[](i64, i64), strheap: []u8) : []u8 =
  let pJ: []pJSON = map to_pJSON js
  let sz (j: pJSON): i64 =
    match j
    case #lbracket -> 1
    case #rbracket -> 1
    case #lclamp -> 1
    case #rclamp -> 1
    case #colon -> 1
    case #comma -> 1
    case #json #null -> 1
    case #json (#num _) -> 1
    case #json (#bool _) -> 1
    case #json (#string (_, _)) -> 1
    case #json (#list (a, b)) ->
      let n = b - a
      in if n == 0
        then 2
        else ((n * 2) - 1) + 2
    case #json (#obj (a, b) (_, _)) ->
      let n = b - a
      in if n == 0
        then 2
        else ((n * 4) - 1) + 2
  let get (j: pJSON) (i: i64): pJSON =
    match j
    case #lbracket -> j
    case #rbracket -> j
    case #lclamp -> j
    case #rclamp -> j
    case #colon -> j
    case #comma -> j
    case #json #null -> j
    case #json (#num _) -> j
    case #json (#bool _) -> j
    case #json (#string (_, _)) -> j
    case #json (#list (a, b)) ->
      let n = b - a
      in if n == 0
        then 
          match i
          case 0 -> #lbracket
          case 1 -> #rbracket
          case _ -> #comma
        else 
          if i == 0
          then #lbracket
        else 
          if i == ((n * 2) - 1) + 1
          then #rbracket
          else 
            if i % 2 == 0
            then #comma
            else pJ[a + ((i - 1) / 2)]
    case #json (#obj (a, b) (c, _)) ->
      let n = b - a in 
      if n == 0
      then 
        match i
          case 0 -> #lclamp
          case 1 -> #rclamp
          case _ -> #comma
        else 
          if i == 0
          then #lclamp
            else 
              if i == ((n * 4) - 1) + 1
              then #rclamp
              else 
                if (i - 1) % 4 == 0
                then #json (#string keys[a + (i / 4)])
                else 
                  if (i - 1) % 4 == 1
                  then #colon
                  else 
                    if (i - 1) % 4 == 2
                    then pJ[c + ((i - 2) / 4)]
                    else #comma
  let (full_pJSON, _) =
    let is_nonleaf (j: pJSON): i64 =
      match j
      case #lbracket -> 0
      case #rbracket -> 0
      case #lclamp -> 0
      case #rclamp -> 0
      case #colon -> 0
      case #comma -> 0
      case #json #null -> 0
      case #json (#num _) -> 0
      case #json (#bool _) -> 0
      case #json (#string (_, _)) -> 0
      case #json (#list (_, _)) -> 1
      case #json (#obj (_, _) (_, _)) -> 1
    in 
      loop (acc, flag) = ([pJ[r]], 1) while flag > 0 do
        let a = expand sz get acc
        let f = reduce_comm (+) 0 (map is_nonleaf a)
        in (a, f)
  let sz_str (j: pJSON): i64 =
    match j
    case #lbracket -> 1
    case #rbracket -> 1
    case #lclamp -> 1
    case #rclamp -> 1
    case #colon -> 1
    case #comma -> 1
    case #json #null -> 4
    case #json (#num n) ->
      if n == 0
      then 1
      else 
        if n < 0
        then i64.f64 (f64.floor (f64.log10 (f64.i64 (i64.abs n)))) + 1 + 1 --extra digit to account for minus sign
        else i64.f64 (f64.floor (f64.log10 (f64.i64 n))) + 1
    case #json (#bool a) -> if a then 4 else 5
    case #json (#string (a, b)) -> (b - a) + 2
    case _ -> 0
  let get_char (j: pJSON) (i: i64): u8 =
    match j
    case #lbracket -> '['
    case #rbracket -> ']'
    case #lclamp -> '{'
    case #rclamp -> '}'
    case #colon -> ':'
    case #comma -> ','
    case #json #null ->
      (match i
      case 0 -> 'n'
      case 1 -> 'u'
      case _ -> 'l')
    case #json (#num n) ->
      let len_n = i64.f64 (f64.floor (f64.log10 (f64.i64 (i64.abs n)))) + 1
      in if n >= 0
        then (u8.i64 (n / (10 ** (len_n - i - 1)) % 10)) + '0'
        else
          if i == 0
          then '-'
          else (u8.i64 ((i64.abs n) / (10 ** (len_n - i)) % 10)) + '0'
    case #json (#bool a) ->
      if a
      then 
        match i
          case 0 -> 't'
          case 1 -> 'r'
          case 2 -> 'u'
          case _ -> 'e'
      else 
        (match i
        case 0 -> 'f'
        case 1 -> 'a'
        case 2 -> 'l'
        case 3 -> 's'
        case _ -> 'e')
    case #json (#string (a, b)) ->
      if i == 0 || i == (b - a) + 1
      then '"'
      else strheap[a + (i - 1)]
    case _ -> ' '
  in expand sz_str get_char full_pJSON

-- test functionality of printer
-- ==
-- entry: test_print
-- input { "1" }
-- output { "1" }
-- input { "12" }
-- output { "12" }
-- input { "123456789" }
-- output { "123456789" }
-- input { "true" }
-- output { "true" }
-- input { "false" }
-- output { "false" }
-- input { "\"test\"" }
-- output { "\"test\"" }
-- input { "\"\"" }
-- output { "\"\"" }
-- input { "null" }
-- output { "null" }
-- input { "[]" }
-- output { "[]" }
-- input { "{}" }
-- output { "{}" }
-- input { "[1]" }
-- output { "[1]" }
-- input { "[1,2]" }
-- output { "[1,2]" }
-- input { "{\"foo\":1}" }
-- output { "{\"foo\":1}" }
-- input { "{\"foo\":1,\"bar\":2}" }
-- output { "{\"foo\":1,\"bar\":2}" }
-- input { "[[]]" }
-- output { "[[]]" }
-- input { "[[1]]" }
-- output { "[[1]]" }
-- input { "[{}]" }
-- output { "[{}]" }
-- input { "[{\"foo\":1}]" }
-- output { "[{\"foo\":1}]" }
-- input { "[1,[]]" }
-- output { "[1,[]]" }
-- input { "[1,[2]]" }
-- output { "[1,[2]]" }
-- input { "[[],1]" }
-- output { "[[],1]" }
-- input { "[[1],2]" }
-- output { "[[1],2]" }
-- input { "[1,{}]" }
-- output { "[1,{}]" }
-- input { "[1,{\"foo\":2}]" }
-- output { "[1,{\"foo\":2}]" }
-- input { "[{},1]" }
-- output { "[{},1]" }
-- input { "[{\"foo\":1},2]" }
-- output { "[{\"foo\":1},2]" }
-- input { "[[],[]]" }
-- output { "[[],[]]" }
-- input { "[[1],[]]" }
-- output { "[[1],[]]" }
-- input { "[[],[1]]" }
-- output { "[[],[1]]" }
-- input { "[[1],[2]]" }
-- output { "[[1],[2]]" }
-- input { "[[],{}]" }
-- output { "[[],{}]" }
-- input { "[[1],{}]" }
-- output { "[[1],{}]" }
-- input { "[[],{\"foo\":1}]" }
-- output { "[[],{\"foo\":1}]" }
-- input { "[[1],{\"foo\":2}]" }
-- output { "[[1],{\"foo\":2}]" }
-- input { "[{},[]]" }
-- output { "[{},[]]" }
-- input { "[{\"foo\":1},[]]" }
-- output { "[{\"foo\":1},[]]" }
-- input { "[{},[1]]" }
-- output { "[{},[1]]" }
-- input { "[{\"foo\":1},[2]]" }
-- output { "[{\"foo\":1},[2]]" }
-- input { "[{},{}]" }
-- output { "[{},{}]" }
-- input { "[{\"foo\":1},{}]" }
-- output { "[{\"foo\":1},{}]" }
-- input { "[{},{\"foo\":1}]" }
-- output { "[{},{\"foo\":1}]" }
-- input { "[{\"foo\":1},{\"bar\":2}]" }
-- output { "[{\"foo\":1},{\"bar\":2}]" }
-- input { "{\"foo\":[]}" }
-- output { "{\"foo\":[]}" }
-- input { "{\"foo\":[1]}" }
-- output { "{\"foo\":[1]}" }
-- input { "{\"foo\":{\"bar\":1}}" }
-- output { "{\"foo\":{\"bar\":1}}" }
-- input { "{\"foo\":1,\"bar\":[]}" }
-- output { "{\"foo\":1,\"bar\":[]}" }
-- input { "{\"foo\":1,\"bar\":[2]}" }
-- output { "{\"foo\":1,\"bar\":[2]}" }
-- input { "{\"foo\":[],\"bar\":1}" }
-- output { "{\"foo\":[],\"bar\":1}" }
-- input { "{\"foo\":[1],\"bar\":2}" }
-- output { "{\"foo\":[1],\"bar\":2}" }
-- input { "{\"foo\":1,\"bar\":{}}" }
-- output { "{\"foo\":1,\"bar\":{}}" }
-- input { "{\"foo\":1,\"bar\":{\"baz\":2}}" }
-- output { "{\"foo\":1,\"bar\":{\"baz\":2}}" }
-- input { "{\"foo\":{},\"bar\":1}" }
-- output { "{\"foo\":{},\"bar\":1}" }
-- input { "{\"foo\":{\"baz\":1},\"bar\":2}" }
-- output { "{\"foo\":{\"baz\":1},\"bar\":2}" }
-- input { "{\"foo\":[],\"bar\":[]}" }
-- output { "{\"foo\":[],\"bar\":[]}" }
-- input { "{\"foo\":[],\"bar\":[1]}" }
-- output { "{\"foo\":[],\"bar\":[1]}" }
-- input { "{\"foo\":[1],\"bar\":[]}" }
-- output { "{\"foo\":[1],\"bar\":[]}" }
-- input { "{\"foo\":[1],\"bar\":[2]}" }
-- output { "{\"foo\":[1],\"bar\":[2]}" }
-- input { "{\"foo\":[],\"bar\":{}}" }
-- output { "{\"foo\":[],\"bar\":{}}" }
-- input { "{\"foo\":[],\"bar\":{\"baz\":1}}" }
-- output { "{\"foo\":[],\"bar\":{\"baz\":1}}" }
-- input { "{\"foo\":[1],\"bar\":{}}" }
-- output { "{\"foo\":[1],\"bar\":{}}" }
-- input { "{\"foo\":[1],\"bar\":{\"baz\":2}}" }
-- output { "{\"foo\":[1],\"bar\":{\"baz\":2}}" }
-- input { "{\"foo\":{},\"bar\":[]}" }
-- output { "{\"foo\":{},\"bar\":[]}" }
-- input { "{\"foo\":{\"baz\":1},\"bar\":[]}" }
-- output { "{\"foo\":{\"baz\":1},\"bar\":[]}" }
-- input { "{\"foo\":{},\"bar\":[1]}" }
-- output { "{\"foo\":{},\"bar\":[1]}" }
-- input { "{\"foo\":{\"baz\":1},\"bar\":[2]}" }
-- output { "{\"foo\":{\"baz\":1},\"bar\":[2]}" }
-- input { "{\"foo\":{},\"bar\":{}}" }
-- output { "{\"foo\":{},\"bar\":{}}" }
-- input { "{\"foo\":{},\"bar\":{\"baz\":1}}" }
-- output { "{\"foo\":{},\"bar\":{\"baz\":1}}" }
-- input { "{\"foo\":{\"baz\":1},\"bar\":{}}" }
-- output { "{\"foo\":{\"baz\":1},\"bar\":{}}" }
-- input { "{\"foo\":{\"baz\":1},\"bar\":{\"qux\":2}}" }
-- output { "{\"foo\":{\"baz\":1},\"bar\":{\"qux\":2}}" }

entry test_print (source: []u8) =
  let JSE = parse_JSON source
  in print_JSON JSE
