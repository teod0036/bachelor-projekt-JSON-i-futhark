import "lib/github.com/diku-dk/sorts/radix_sort"
import "json"

type production = parser.production
type node = parser.node terminal production
type option 'a = #none | #some a

--advanced testjson for later: "[{\"foo\": 1, \"bar\": {\"baz\": true}},{\"qux\": [3, 4, 5]}]"

type JSON = #null | #num i64 | #bool bool | #string i64 | #list (i64, i64) | #obj (i64, i64) (i64, i64)

def testjson : []u8 = "[{\"foo\": \"test\"}, {\"bar\": 2}]"
--def testjson : []u8 = "[{\"a\": 1, \"ab\": {\"abc\": true}},{\"abcd\": [3, 4, 5]}]"

--testjson cst
--#some [(0, #production (#Array)), (0, #production (#Array)), (1, #terminal (#literal_4) (0, 1)),
--      (1, #production (#Array)), (3, #production (#Object)), (4, #production (#Object)),
--      (5, #terminal (#literal_9) (1, 2)), (5, #production (#Object)), (7, #production (#Object)),
--      (8, #terminal (#string) (2, 7)), (8, #terminal (#literal_3) (7, 8)), (8, #production (#Number)),
--      (11, #terminal (#number) (9, 10)), (7, #production (#Object)), (5, #terminal (#literal_10) (10, 11)),
--      (3, #production (#Array)), (15, #terminal (#literal_2) (11, 12)), (15, #production (#Object)),
--      (17, #production (#Object)), (18, #terminal (#literal_9) (13, 14)), (18, #production (#Object)),
--      (20, #production (#Object)), (21, #terminal (#string) (14, 19)), (21, #terminal (#literal_3) (19, 20)),
--      (21, #production (#Number)), (24, #terminal (#number) (21, 22)), (20, #production (#Object)),
--      (18, #terminal (#literal_10) (22, 23)), (15, #production (#Array)), (1, #terminal (#literal_5) (23, 24))]

def preprocess_cst (js: option ([](i64, node))) : [](i64, node) =
  match js
  case #some json -> json
  case #none -> []

def cst_by_depth (ns: [](i64, node)) : [](i64, (i64, (i64, node))) =
  let track_depth (acc: (i64, (i64, node))) (n: (i64, (i64, node))): (i64, (i64, node)) =
    match (acc, n)
    case (((depth, (_, #terminal #literal_9 (_, _))), (_, (parent, nde)))) ->
      (depth + 1, (parent, nde))
    case (((depth, (_, #terminal #literal_4 (_, _))), (_, (parent, nde)))) ->
      (depth + 1, (parent, nde))
    case (((depth, (_, #terminal #literal_5 (_, _))), (_, (parent, nde)))) ->
      (depth - 1, (parent, nde))
    case (((depth, (_, #terminal #literal_10 (_, _))), (_, (parent, nde)))) ->
      (depth - 1, (parent, nde))
    case ((depth, (_, _)), (_, (parent, nde))) ->
      (depth, (parent, nde))
  let depths =
    loop (acc: (i64, (i64, node)), ns: [](i64, (i64, node))) = ((0, (0, #production (#Null))), [])
    for n in (zip (indices ns) ns) do
      let temp = track_depth acc n
      in (temp, concat ns [temp])
  let idepths = zip (indices depths.1) depths.1
  let find_parents (n: (i64, (i64, (i64, node)))): (i64, (i64, (i64, node))) =
    --(index, (depth, (parent, node)))
    match n
    case (index, (depth, (_, nde))) ->
      let parent_n =
        loop (flag: bool, p: i64) = (true, index)
        while flag do
          match idepths[p]
          case (0, _) -> (false, 0)
          case (idx, (d, (_, #terminal #literal_4 _))) ->
            if d < depth then (false, idx) else (true, idx - 1)
          case (idx, (d, (_, #terminal #literal_9 _))) ->
            if d < depth then (false, idx) else (true, idx - 1)
          case (idx, _) -> (true, idx - 1)
      in (index, (depth, (parent_n.1, nde)))
  let parented = map find_parents idepths
  let get_key (n: (i64, (i64, (i64, node)))) =
    match n
    case (_, (depth, (_, _))) -> depth
  in radix_sort_by_key get_key i64.num_bits i64.get_bit parented

-- converts a []u8 (string) to a number
def s_to_num (a: i64, b: i64) (s: []u8) =
  let xs = reverse s[a:b]
  in loop acc = 0
     for i < (b - a) do
       acc * 10 + (i64.u8 xs[(b - a) - 1 - i] - '0')

def sorted_cst_to_JSON (source: []u8) (ns: [](i64, (i64, (i64, node)))) : ([]JSON, [](i64, i64)) =
  let match_relevant (n: (i64, (i64, (i64, node)))): bool =
    --relevant: string, number, literal 4, literal 6, literal 7, literal 8, literal 9
    match n
    case (_, (_, (_, #production _))) -> false
    case (_, (_, (_, #terminal #string (_, b)))) ->
      if b >= length source
      then true
      else if source[b] == 58
      then false
      else true
    case (_, (_, (_, #terminal #number (_, _)))) -> true
    case (_, (_, (_, #terminal #literal_4 _))) -> true
    case (_, (_, (_, #terminal #literal_6 _))) -> true
    case (_, (_, (_, #terminal #literal_7 _))) -> true
    case (_, (_, (_, #terminal #literal_8 _))) -> true
    case (_, (_, (_, #terminal #literal_9 _))) -> true
    case _ -> false
  let final_intermediate_value = filter match_relevant ns
  --signature: (pre-sort index, (depth, (parent as pre-sort index, node)))
  
  let i_str_keys: [](bool, (i64, (i64, i64))) =
  --signature: (iskey, (parent, (span, span)))
    let match_spans (nde: (i64, (i64, (i64, node)))): bool =
      match nde.1.1.1
      case (#terminal #string _) -> true
      case _ -> false
    let construct_intermediate_key (n: (i64, (i64, (i64, node)))): (bool, (i64, (i64, i64))) =
      match n.1.1
      case (p, #terminal #string (a, b)) ->
        if b >= length source
        then (false, (p, (a, b)))
        else if source[b] == 58
        then (true, (p, (a, b)))
        else (false, (p, (a, b)))
      case _ -> (false, (-1, (-1, -1)))
    in map construct_intermediate_key (filter match_spans ns)

  let find_str_key (sp: (i64, i64)): i64 =
    let is_str_key (ik_idx: i64) : bool = i_str_keys[ik_idx].1.1 == sp
    let op (x, i) (y, j) =
      if x && y then if i < j
                   then (x, i)
                   else (y, j)
      else if y then (y, j)
      else (x, i)
    in (reduce_comm op (false, -1) (zip (map is_str_key (indices i_str_keys)) (indices i_str_keys))).1

  let find_obj_keys (parent: i64): (i64, i64) =
    let is_obj_key (x:i64) : bool = i_str_keys[x].0 && i_str_keys[x].1.0 == parent
    let op (x, (i1, i2)) (y, (j1, j2)) =
      if x && y 
      then 
        (x, (i64.min i1 j1, i64.max i2 j2))
      else 
        if y 
        then (y, (j1, j2))
        else (x, (i1, i2))
    let temp = (reduce_comm op (false, (i64.highest, -1)) (zip (map is_obj_key (indices i_str_keys)) (zip (indices i_str_keys) (indices i_str_keys)))).1
    in if temp.1 == (-1)
       then (-1, -1)
       else (temp.0, temp.1 + 1)

  let find_children (parent: i64): (i64, i64) =
    let is_child (x:i64) : bool = final_intermediate_value[x].1.1.0 == parent
    let op (x, (i1, i2)) (y, (j1, j2)) =
      if x && y 
      then 
        (x, (i64.min i1 j1, i64.max i2 j2))
      else 
        if y 
        then (y, (j1, j2))
        else (x, (i1, i2))
    let temp = (reduce_comm op (false, (i64.highest, -1)) (zip (map is_child (indices final_intermediate_value)) (zip (indices final_intermediate_value) (indices final_intermediate_value)))).1
    in if temp.1 == (-1)
       then (-1, -1)
       else (temp.0, temp.1 + 1)

  --signature: pre-sort index, depth, parent as pre-sort index, node
  let get_val (n: (i64, (i64, (i64, node)))): JSON =
    match n
    case (_, (_, (_, #terminal #string (a, b)))) -> #string (find_str_key (a, b))
    case (_, (_, (_, #terminal #number n))) -> #num (s_to_num n source)
    case (idx, (_, (_, #terminal #literal_4 _))) -> #list (find_children idx)
    case (_, (_, (_, #terminal #literal_6 _))) -> #bool false
    case (_, (_, (_, #terminal #literal_7 _))) -> #null
    case (_, (_, (_, #terminal #literal_8 _))) -> #bool true
    case (idx, (_, (_, #terminal #literal_9 _))) -> #obj (find_obj_keys idx) (find_children idx)
    case _ -> #null
  let str_keys: [](i64, i64) =
    let isolate_key (ik: (bool, (i64, (i64, i64)))): (i64, i64) = (ik.1.1.0 + 1, ik.1.1.1 - 1)
    in map isolate_key i_str_keys
  let final_json: []JSON =
    map get_val final_intermediate_value
  in (final_json, str_keys)

--def main : [](i64, (i64, (i64, node))) =
--    let json = preprocess_cst (parse testjson) in
--    if null json then [] else cst_by_depth json

def main : ([]JSON, [](i64, i64)) =
  let json = preprocess_cst (parse testjson)
  in if null json then ([], []) else sorted_cst_to_JSON testjson (cst_by_depth json)

entry test_function (j:[]u8) : ([][5]i64, [][2]i64) = 
  let data =
    let json = preprocess_cst (parse j) in
    if null json then ([], []) else sorted_cst_to_JSON testjson (cst_by_depth json)
  let JSON_to_primitive (j_data:JSON) : [5]i64 =
    match j_data
    case #null -> [0, -1, -1, -1, -1]
    case #num x -> [1, x, -1, -1, -1]
    case #bool true -> [2, 1, -1, -1, -1]
    case #bool false -> [2, 0, -1, -1, -1]
    case #string x -> [3, x, -1, -1, -1]
    case #list (a, b) -> [4, -1, -1, a, b]
    case #obj (a, b) (c, d) -> [5, a, b, c, d]
  let keys_to_primitive (a:i64, b:i64) : [2]i64 = [a, b] in
  (map JSON_to_primitive data.0, map keys_to_primitive data.1)