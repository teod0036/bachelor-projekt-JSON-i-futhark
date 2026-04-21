import "lib/github.com/diku-dk/sorts/radix_sort"
import "json"

type production = parser.production
type node = parser.node terminal production
type option 'a = #none | #some a

type JSON = #null | #num i64 | #bool bool | #string (i64, i64) | #list (i64, i64) | #obj (i64, i64) (i64, i64)

def testjson : []u8 = "[{\"foo\": 1}, {\"bar\": 2}]"
--def testjson : []u8 = "[{\"a\": 1, \"ab\": {\"abc\": true}},{\"abcd\": [3, 4, 5]}]"

--testjson cst
--#some [(0, #production (#Array_production)), (0, #production (#Array)), (1, #terminal (#literal_4) (0, 1)), 
--      (1, #production (#Array_head)), (3, #production (#Object_production)), (4, #production (#Object)), 
--      (5, #terminal (#literal_9) (1, 2)), (5, #production (#Object_head)), (7, #production (#Object_element)), 
--      (8, #terminal (#string) (2, 7)), (8, #terminal (#literal_3) (7, 8)), (8, #production (#String)), 
--      (11, #terminal (#string) (9, 15)), (7, #production (#Object_tail)), (5, #terminal (#literal_10) (15, 16)), 
--      (3, #production (#Array_tail)), (15, #terminal (#literal_2) (16, 17)), (15, #production (#Object_production)), 
--      (17, #production (#Object)), (18, #terminal (#literal_9) (18, 19)), (18, #production (#Object_head)), 
--      (20, #production (#Object_element)), (21, #terminal (#string) (19, 24)), 
--      (21, #terminal (#literal_3) (24, 25)), (21, #production (#Number)), (24, #terminal (#number) (26, 27)), 
--      (20, #production (#Object_tail)), (18, #terminal (#literal_10) (27, 28)), (15, #production (#Array_tail)), 
--      (1, #terminal (#literal_5) (28, 29))]

def preprocess_cst (js: option ([](i64, node))) : [](i64, node) =
  match js
  case #some json -> json
  case #none -> []

def cst_by_depth (ns: [](i64, node)) : [](i64, (i64, (i64, node))) =
  let (_,nodes) = unzip ns
  -- wyllie's algorithm implementation inspired by slide 24 of:
  -- https://github.com/diku-dk/dpp-e2025-pub/blob/main/slides/L5-pointer-structures.pdf
  -- And by lines 145-162 of:
  -- https://github.com/diku-dk/vtree/blob/main/lib/github.com/diku-dk/vtree/vtree.fut
  let compress [n] (N: [n](i64, node)) :  [n](i64, node) =
    let f i = if N[i].0 == 0 || N[i].1 == #production #Array || N[i].1 == #production #Object
              then N[i]
              else (N[N[i].0].0, N[N[i].0].1)
    in tabulate n f
  let rank [n] (R: [n]i64) (N: [n](i64, node)) : ([n]i64, [n](i64, node)) =
    let f i = if N[i].0 == 0
              then (R[i], N[i])
              else (R[i] + R[N[i].0], (N[N[i].0].0, N[N[i].0].1))
    in unzip (tabulate n f)

  let wyllie [n] (N: [n](i64, node)) : ([n]i64, [n]i64) =
    let R = replicate n 1 with [0] = 0
    let N' = loop N for _i < 64 - i64.clz n do 
                    compress N
    let (P,_) = unzip N' 
    let (R,_) = loop (R, N') for _i < 64 - i64.clz n do
                    rank R N'
    in (R, P)
  let (Depths, Parents) = wyllie ns
  let parented_and_depthed = zip (indices nodes) (zip Depths (zip Parents nodes))
  let get_key (n: (i64, (i64, (i64, node)))) =
    match n
    case (_, (depth, (_, _))) -> depth
  in radix_sort_by_key get_key i64.num_bits i64.get_bit parented_and_depthed

-- converts a []u8 (string) to a number
def s_to_num (a: i64, b: i64) (s: []u8) =
  let xs = reverse s[a:b]
  in loop acc = 0
     for i < (b - a) do
       acc * 10 + (i64.u8 xs[(b - a) - 1 - i] - '0')

def sorted_cst_to_JSON (source: []u8) (ns: [](i64, (i64, (i64, node)))) : ([]JSON, [](i64, i64)) =
  let match_relevant (n: (i64, (i64, (i64, node)))): bool =
    --relevant: #production #Array_production, #production #Object_production, #terminal #string, #terminal #number, #terminal #literal_6, #terminal #literal_7, #terminal #literal_8
    match n
    case (_, (_, (_, #production #Array_production))) -> true
    case (_, (_, (_, #production #Object_production))) -> true
    case (_, (_, (_, #terminal #string (_, b)))) ->
      if b >= length source
      then true
      else if source[b] == ':'
      then false
      else true
    case (_, (_, (_, #terminal #number _))) -> true
    case (_, (_, (_, #terminal #literal_6 _))) -> true
    case (_, (_, (_, #terminal #literal_7 _))) -> true
    case (_, (_, (_, #terminal #literal_8 _))) -> true
    case _ -> false
  let final_intermediate_value = filter match_relevant ns
  --signature: (pre-sort index, (depth, (parent as pre-sort index, node)))
  
  let i_keys: [](i64, (i64, i64)) =
  --signature: (parent, (span, span)))
    let match_spans (nde: (i64, (i64, (i64, node)))): bool =
      match nde.1.1
      case (_, #terminal #string (_, b)) -> 
        if b < length source 
        then 
          if source[b] == ':'
          then true
          else false
        else false
      case _ -> false
    let construct_intermediate_key (n: (i64, (i64, (i64, node)))): (i64, (i64, i64)) =
      match n.1.1
      case (p, #terminal #string (a, b)) -> (p, (a, b))
      case _ -> (-1, (-1, -1))
    in map construct_intermediate_key (filter match_spans ns)
  
  let find_obj_keys (parent: i64): (i64, i64) =
    let is_obj_key (x:i64) : bool = i_keys[x].0 == parent
    let op (x, (i1, i2)) (y, (j1, j2)) =
      if x && y 
      then 
        (x, (i64.min i1 j1, i64.max i2 j2))
      else 
        if y 
        then (y, (j1, j2))
        else (x, (i1, i2))
    let temp = (reduce_comm op (false, (i64.highest, -1)) (zip (map is_obj_key (indices i_keys)) (zip (indices i_keys) (indices i_keys)))).1
    in if temp.1 == (-1)
       then (-1, -1)
       else (temp.0, temp.1 + 1)

  let find_children (parent: i64): (i64, i64) =
    let is_child (x:i64) : bool = final_intermediate_value[x].1.1.0 == parent && parent != x
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
    case (_, (_, (_, #terminal #string (a, b)))) -> #string (a+1, b-1)
    case (_, (_, (_, #terminal #number n))) -> #num (s_to_num n source)
    case (idx, (_, (_, #production #Array_production))) -> #list (find_children idx)
    case (_, (_, (_, #terminal #literal_6 _))) -> #bool false
    case (_, (_, (_, #terminal #literal_7 _))) -> #null
    case (_, (_, (_, #terminal #literal_8 _))) -> #bool true
    case (idx, (_, (_, #production #Object_production))) -> #obj (find_obj_keys idx) (find_children idx)
    case _ -> #null
  let isolate_key (ik: (i64, (i64, i64))): (i64, i64) = (ik.1.0 + 1, ik.1.1 - 1)
  let keys: [](i64, i64) = map isolate_key i_keys
  let final_json: []JSON = map get_val final_intermediate_value
  in (final_json, keys)

def parse_JSON (s:[]u8) : ([]JSON, [](i64, i64)) =
  let json = preprocess_cst (parse s)
  in if null json then ([], []) else sorted_cst_to_JSON s (cst_by_depth json)

def main =
  parse_JSON testjson

-- test functionality of parser
-- ==
-- entry: test_fun
-- input { "1" }
-- output { [[1i64, 1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "12" }
-- output { [[1i64, 12i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "123456789" }
-- output { [[1i64, 123456789i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "true" }
-- output { [[2i64, 1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "false" }
-- output { [[2i64, 0i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "\"test\"" }
-- output { [[3i64, 1i64, 5i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "\"\"" }
-- output { [[3i64, 1i64, 1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "null" }
-- output { [[0i64, -1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[]" }
-- output { [[4i64, -1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "{}" }
-- output { [[5i64, -1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[1]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 2i64], [1i64, 1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[1, 2]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64], [1i64, 2i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "{\"foo\": 1}" }
-- output { [[5i64, 0i64, 1i64, 1i64, 2i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64]] }
-- input { "{\"foo\": 1, \"bar\": 2}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64], [1i64, 2i64, -1i64, -1i64, -1i64]] [[2i64, 5i64],[12i64, 15i64]] }
-- input { "[[]]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 2i64], [4i64, -1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[[1]]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 2i64], [4i64, -1i64, -1i64, 2i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[{}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 2i64], [5i64, -1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[{\"foo\": 1}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 2i64], [5i64, 0i64, 1i64, 2i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[3i64, 6i64]] }
-- input { "[1, []]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[1, [2]]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, 3i64, 4i64], [1i64, 2i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[[], 1]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [4i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[[1], 2]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [4i64, -1i64, -1i64, 3i64, 4i64], [1i64, 2i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[1, {}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64], [5i64, -1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[1, {\"foo\": 2}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64], [5i64, 0i64, 1i64, 3i64, 4i64], [1i64, 2i64, -1i64, -1i64, -1i64]] [[6i64, 9i64]] }
-- input { "[{}, 1]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [5i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[{\"foo\": 1}, 2]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [5i64, 0i64, 1i64, 3i64, 4i64], [1i64, 2i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[3i64, 6i64]] }
-- input { "[[], []]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [4i64, -1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[[1], []]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [4i64, -1i64, -1i64, 3i64, 4i64], [4i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[[], [1]]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [4i64, -1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, 3i64, 4i64], [1i64, 1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[[1], [2]]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [4i64, -1i64, -1i64, 3i64, 4i64], [4i64, -1i64, -1i64, 4i64, 5i64], [1i64, 1i64, -1i64, -1i64, -1i64], [1i64, 2i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[[], {}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [4i64, -1i64, -1i64, -1i64, -1i64], [5i64, -1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[[1], {}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [4i64, -1i64, -1i64, 3i64, 4i64], [5i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[[], {\"foo\": 1}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [4i64, -1i64, -1i64, -1i64, -1i64], [5i64, 0i64, 1i64, 3i64, 4i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[7i64, 10i64]] }
-- input { "[[1], {\"foo\": 2}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [4i64, -1i64, -1i64, 3i64, 4i64], [5i64, 0i64, 1i64, 4i64, 5i64], [1i64, 1i64, -1i64, -1i64, -1i64], [1i64, 2i64, -1i64, -1i64, -1i64]] [[8i64, 11i64]] }
-- input { "[{}, []]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [5i64, -1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[{\"foo\": 1}, []]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [5i64, 0i64, 1i64, 3i64, 4i64], [4i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[3i64, 6i64]] }
-- input { "[{}, [1]]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [5i64, -1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, 3i64, 4i64], [1i64, 1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[{\"foo\": 1}, [2]]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [5i64, 0i64, 1i64, 3i64, 4i64], [4i64, -1i64, -1i64, 4i64, 5i64], [1i64, 1i64, -1i64, -1i64, -1i64], [1i64, 2i64, -1i64, -1i64, -1i64]] [[3i64, 6i64]] }
-- input { "[{}, {}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [5i64, -1i64, -1i64, -1i64, -1i64], [5i64, -1i64, -1i64, -1i64, -1i64]] empty([0][2]i64) }
-- input { "[{\"foo\": 1}, {}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [5i64, 0i64, 1i64, 3i64, 4i64], [5i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[3i64, 6i64]] }
-- input { "[{}, {\"foo\": 1}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [5i64, -1i64, -1i64, -1i64, -1i64], [5i64, 0i64, 1i64, 3i64, 4i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[7i64, 10i64]] }
-- input { "[{\"foo\": 1}, {\"bar\": 2}]" }
-- output { [[4i64, -1i64, -1i64, 1i64, 3i64], [5i64, 0i64, 1i64, 3i64, 4i64], [5i64, 1i64, 2i64, 4i64, 5i64], [1i64, 1i64, -1i64, -1i64, -1i64], [1i64, 2i64, -1i64, -1i64, -1i64]] [[3i64, 6i64], [15i64, 18]] }
-- input { "{\"foo\": []}" }
-- output { [[5i64, 0i64, 1i64, 1i64, 2i64], [4i64, -1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64]] }
-- input { "{\"foo\": [1]}" }
-- output { [[5i64, 0i64, 1i64, 1i64, 2i64], [4i64, -1i64, -1i64, 2i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64]] }
-- input { "{\"foo\": {\"bar\": 1}}" }
-- output { [[5i64, 0i64, 1i64, 1i64, 2i64], [5i64, 1i64, 2i64, 2, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [10i64, 13i64]] }
-- input { "{\"foo\": 1, \"bar\": []}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [12i64, 15i64]] }
-- input { "{\"foo\": 1, \"bar\": [2]}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, 3i64, 4i64], [1i64, 2i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [12i64, 15i64]] }
-- input { "{\"foo\": [], \"bar\": 1}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [4i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [13i64, 16i64]] }
-- input { "{\"foo\": [1], \"bar\": 2}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [4i64, -1i64, -1i64, 3i64, 4i64], [1i64, 2i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [14i64, 17i64]] }
-- input { "{\"foo\": 1, \"bar\": {}}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64], [5i64, -1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [12i64, 15i64]] }
-- input { "{\"foo\": 1, \"bar\": {\"baz\": 2}}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [1i64, 1i64, -1i64, -1i64, -1i64], [5i64, 2i64, 3i64, 3i64, 4i64], [1i64, 2i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [12i64, 15i64], [20i64, 23i64]] }
-- input { "{\"foo\": {}, \"bar\": 1}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [5i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [13i64, 16i64]] }
-- input { "{\"foo\": {\"baz\": 1}, \"bar\": 2}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [5i64, 2i64, 3i64, 3i64, 4i64], [1i64, 2i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [21i64, 24i64], [10i64, 13i64]] }
-- input { "{\"foo\": [], \"bar\": []}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [4i64, -1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [13i64, 16i64]] }
-- input { "{\"foo\": [], \"bar\": [1]}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [4i64, -1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, 3i64, 4i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [13i64, 16i64]] }
-- input { "{\"foo\": [1], \"bar\": []}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [4i64, -1i64, -1i64, 3i64, 4i64], [4i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [14i64, 17i64]] }
-- input { "{\"foo\": [1], \"bar\": [2]}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [4i64, -1i64, -1i64, 3i64, 4i64], [4i64, -1i64, -1i64, 4i64, 5i64], [1i64, 1i64, -1i64, -1i64, -1i64], [1i64, 2i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [14i64, 17i64]] }
-- input { "{\"foo\": [], \"bar\": {}}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [4i64, -1i64, -1i64, -1i64, -1i64], [5i64, -1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [13i64, 16i64]] }
-- input { "{\"foo\": [], \"bar\": {\"baz\": 1}}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [4i64, -1i64, -1i64, -1i64, -1i64], [5i64, 2i64, 3i64, 3i64, 4i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [13i64, 16i64], [21i64, 24i64]] }
-- input { "{\"foo\": [1], \"bar\": {}}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [4i64, -1i64, -1i64, 3i64, 4i64], [5i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [14i64, 17i64]] }
-- input { "{\"foo\": [1], \"bar\": {\"baz\": 2}}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [4i64, -1i64, -1i64, 3i64, 4i64], [5i64, 2i64, 3i64, 4i64, 5i64], [1i64, 1i64, -1i64, -1i64, -1i64], [1i64, 2i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [14i64, 17i64], [22i64, 25i64]] }
-- input { "{\"foo\": {}, \"bar\": []}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [5i64, -1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [13i64, 16i64]] }
-- input { "{\"foo\": {\"baz\": 1}, \"bar\": []}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [5i64, 2i64, 3i64, 3i64, 4i64], [4i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [21i64, 24i64], [10i64, 13i64]] }
-- input { "{\"foo\": {}, \"bar\": [1]}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [5i64, -1i64, -1i64, -1i64, -1i64], [4i64, -1i64, -1i64, 3i64, 4i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [13i64, 16i64]] }
-- input { "{\"foo\": {\"baz\": 1}, \"bar\": [2]}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [5i64, 2i64, 3i64, 3i64, 4i64], [4i64, -1i64, -1i64, 4i64, 5i64], [1i64, 1i64, -1i64, -1i64, -1i64], [1i64, 2i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [21i64, 24i64], [10i64, 13i64]] }
-- input { "{\"foo\": {}, \"bar\": {}}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [5i64, -1i64, -1i64, -1i64, -1i64], [5i64, -1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [13i64, 16i64]] }
-- input { "{\"foo\": {}, \"bar\": {\"baz\": 1}}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [5i64, -1i64, -1i64, -1i64, -1i64], [5i64, 2i64, 3i64, 3i64, 4i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [13i64, 16i64], [21i64, 24i64]] }
-- input { "{\"foo\": {\"baz\": 1}, \"bar\": {}}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [5i64, 2i64, 3i64, 3i64, 4i64], [5i64, -1i64, -1i64, -1i64, -1i64], [1i64, 1i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [21i64, 24i64], [10i64, 13i64]] }
-- input { "{\"foo\": {\"baz\": 1}, \"bar\": {\"qux\": 2}}" }
-- output { [[5i64, 0i64, 2i64, 1i64, 3i64], [5i64, 2i64, 3i64, 3i64, 4i64], [5i64, 3i64, 4i64, 4i64, 5i64], [1i64, 1i64, -1i64, -1i64, -1i64], [1i64, 2i64, -1i64, -1i64, -1i64]] [[2i64, 5i64], [21i64, 24i64], [10i64, 13i64], [29i64, 32i64]] }

entry test_fun (j:[]u8) : ([][5]i64, [][2]i64) = 
  let data = parse_JSON j
  let JSON_to_primitive (j_data:JSON) : [5]i64 =
    match j_data
    case #null -> [0, -1, -1, -1, -1]
    case #num x -> [1, x, -1, -1, -1]
    case #bool true -> [2, 1, -1, -1, -1]
    case #bool false -> [2, 0, -1, -1, -1]
    case #string (a, b) -> [3, a, b, -1, -1]
    case #list (a, b) -> [4, -1, -1, a, b]
    case #obj (a, b) (c, d) -> [5, a, b, c, d]
  let keys_to_primitive (a:i64, b:i64) : [2]i64 = [a, b] in
  (map JSON_to_primitive data.0, map keys_to_primitive data.1)