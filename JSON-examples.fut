import "JSON-datatype"
import "JSON-utils"
import "lib/github.com/diku-dk/sorts/radix_sort"

--From https://futhark-lang.org/examples/array-equality.html
def str_equal [n] [m] (a: [n]u8) (b: [m]u8) : bool =
  n == m && (and (map2 (==) a (b :> [n]u8)))

--The root needs to point to an object, otherwise an invalid environment is returned
--If the object does not contain the key, a new environment containing only null is returned
def get_by_key (JSE:JSON_environment) (key:[]u8) : JSON_environment =
  let (r:i64, j:[]JSON, k:[](i64, i64), s:[]u8) = JSE in 
  if r == -1 
  then (-1, [], [], [])
  else
    match j[r]
      case #obj (a, b) (c, _) ->
        if a == -1 
        then (0, [#null], [], [])
        else
          let relevant_keys = k[a:b]
          let is_key (x1:i64, x2:i64): bool = str_equal key s[x1:x2]
          let op (x, i) (y, j) =
            if x && y
            then if i < j
                then (x, i)
                else (y, j)
            else if y
            then (y, j)
            else (x, i)
          let offset = (reduce_comm op (false, -1) (zip (map is_key relevant_keys) (indices relevant_keys))) in
          if offset.0
          then (c + offset.1, j, k, s)
          else (0, [#null], [], [])
      case _ -> (-1, [], [], [])

--Based on first example from https://jqlang.org/manual/#object-identifier-index
--Expected output: "42" 
def get_foo_example =
  let input = "{\"foo\": 42, \"bar\": \"less interesting data\"}"
  let foo = "foo" 
  let JSE = parse_JSON input in
  print_JSON (get_by_key JSE foo)

--The root needs to point to a list otherwise an invalid environment is returned
--If index is out of bounds, a new environment containing null is returned
def get_by_index (JSE:JSON_environment) (idx:i64) : JSON_environment =
  let (r:i64, j:[]JSON, k:[](i64, i64), s:[]u8) = JSE in 
  if r == -1 
  then (-1, [], [], [])
  else
    match j[r]
    case #list (a, b) -> 
      if idx < b
      then (a+idx, j, k, s)
      else (0, [#null], [], [])
    case _ -> (-1, [], [], [])

--Based on first example from https://jqlang.org/manual/#array-index
--Expected output: "{\"name\":\"JSON\",\"good\":true}"
def get_zero_example =
  let input = "[{\"name\":\"JSON\", \"good\":true}, {\"name\":\"XML\", \"good\":false}]"
  let index = 0
  let JSE = parse_JSON input in
  print_JSON (get_by_index JSE index)


--The root needs to point to an object, otherwise an invalid environment is returned
def get_keys (JSE:JSON_environment) : JSON_environment =
  let (r:i64, j:[]JSON, k:[](i64, i64), s:[]u8) = JSE in 
  if r == -1 
  then (-1, [], [], [])
  else
    match j[r]
    case #obj (a, b) (_, _) ->
      if a == -1
      then (0, [#list (-1, -1)], [], [])
      else
        let relevant_keys = k[a:b]
        let str_to_JSON (x:str) : JSON = #string x
        let JSON_strs : []JSON = concat [#list (1, b-a+1)] (map str_to_JSON relevant_keys) in
        (0, JSON_strs, [], s) 
    case _ -> (-1, [], [], [])

--Based on first example from https://jqlang.org/manual/#keys-keys_unsorted, except the keys aren't sorted
--Expected output: "[\"abc\",\"abcd\",\"Foo\"]"
def get_keys_example =
  let input = "{\"abc\": 1, \"abcd\": 2, \"Foo\": 3}"
  let JSE = parse_JSON input in
  print_JSON (get_keys JSE)


--The root needs to point to an object or a list, otherwise an invalid environment is returned
def map_has_key (JSE:JSON_environment) (key:[]u8) : JSON_environment =
  let (r:i64, j:[]JSON, k:[](i64, i64), s:[]u8) = JSE in 
  if r == -1 
  then (-1, [], [], [])
  else
    let has_key (o:i64) : bool =
      let temp = (get_by_key (o, j, k, s) key) in
      temp.0 != -1 && temp.1[0] != #null
    let bool_to_JSONbool (x:bool) = #bool x in
    match j[r]
      case #list (a, b) ->
        if a == -1 
        then (0, [#list (-1, -1)], [], []) 
        else
          let relevant_objects = a..<b  
          let bool_arr = map has_key relevant_objects
          let JSON_bools = concat [#list (1, b-a+1)] (map bool_to_JSONbool bool_arr) in
          (0, JSON_bools, [], [])
      case #obj (_, _) (a, b) ->
        if a == -1 
        then (0, [#list (-1, -1)], [], []) 
        else
          let relevant_objects = a..<b  
          let bool_arr = map has_key relevant_objects
          let JSON_bools = concat [#list (1, b-a+1)] (map bool_to_JSONbool bool_arr) in
          (0, JSON_bools, [], [])
      case _ -> (-1, [], [], [])

--Based on first example from https://jqlang.org/manual/#has
--Expected output: "[true,false]"
def map_has_foo_example1 =
  let input = "[{\"foo\": 42}, {}]"
  let key = "foo"
  let JSE = parse_JSON input in
  print_JSON (map_has_key JSE key)

--Expected output: "[true,false]"
def map_has_foo_example2 =
  let input = "{\"bar\":{\"foo\": 42}, \"baz\":{}}"
  let key = "foo"
  let JSE = parse_JSON input in
  print_JSON (map_has_key JSE key)



--The root needs to point to a list, otherwise an invalid environment is returned
--The function returns an environment where the root is replaced with an array of roots
--this is a way to represent a list of objects without rebuilding the environment
def select_key_val_is_string (JSE:JSON_environment) (key:[]u8) (value:[]u8) : ([]i64, []JSON, []str, []u8) =
  let (r:i64, j:[]JSON, k:[](i64, i64), s:[]u8) = JSE in 
  if r == -1 
    then ([-1], [], [], [])
    else
      match j[r]
      case #list (a, b) ->
        let relevant_objects = a..<b
        let key_val_is_string (o:i64) : i64 =
          let (r', j', _, _) = (get_by_key (o, j, k, s) key) in
          match j'[r']
          case #string (a', b') ->
            let slice = s[a':b'] in
            if str_equal value slice
            then o
            else -1
          case _ -> -1
        let is_bad (x:i64) = x != -1
        let roots = filter is_bad (map key_val_is_string relevant_objects) in
        (roots, j, k, s)
      case _ -> ([-1], [], [], [])

--Based on second example from https://jqlang.org/manual/#select
--Expected output: "{\"id\":\"second\",\"val\":2}"
def select_id_is_second_example =
  let input = "[{\"id\": \"first\", \"val\": 1}, {\"id\": \"second\", \"val\": 2}]"
  let key = "id"
  let value = "second"
  let JSE = parse_JSON input 
  let (rs, j, k, s) = select_key_val_is_string JSE key value in
  print_JSON (rs[0], j, k, s)


--The root needs to point to a list, otherwise an invalid environment is returned'
--If there are elements within the list which either are not objects or 
--do not contain the specified key, or the key does not map to a number,
--they are put at the end unsorted 
def sort_by_key_val (JSE:JSON_environment) (key:[]u8) : JSON_environment =
  let (r:i64, j:[]JSON, k:[](i64, i64), s:[]u8) = JSE in 
  if r == -1 
    then (-1, [], [], [])
    else
      match j[r] 
      case #list (a, b) ->
        let get_radix_key (x:i64) : i64 =
          if x < a 
          then 0
          else 
            if x >= b
            then i64.highest
            else 
              let (value, j', _, _) = get_by_key (x, j, k, s) key in
              if value == -1
              then i64.highest
              else 
                match j'[value]
                case #num n -> n
                case _ -> i64.highest
        let sorted_indices = radix_sort_by_key get_radix_key i64.num_bits i64.get_bit (indices j)
        let sorted_JSON = scatter (copy j) sorted_indices j in
        (r, sorted_JSON, k, s)
      case _ -> (-1, [], [], [])

--Based on second example from https://jqlang.org/manual/v1.8/#sort-sort_by
--Expected output: "[{\"foo\":2,\"bar\":1},{\"foo\":3,\"bar\":10},{\"foo\":4,\"bar\":10}]"
def sort_by_foo_example =
  let input = "[{\"foo\":4, \"bar\":10}, {\"foo\":3, \"bar\":10}, {\"foo\":2, \"bar\":1}]"
  let key = "foo"
  let JSE = parse_JSON input in
  print_JSON (sort_by_key_val JSE key)



def unique_by_key_val (JSE:JSON_environment) (key:[]u8) : ([]i64, []JSON, []str, []u8) =
  let (r:i64, j:[]JSON, k:[](i64, i64), s:[]u8) = sort_by_key_val JSE key in
  if r == -1
  then ([-1], [], [], [])
  else 
    match j[r]
    case #list (a, b) ->
      let relevant_objects = a..<b
      let border (x:i64) : i64 =
        if x == a
        then x
        else 
          let (r1, val1, _, _) = get_by_key (x, j, k, s) key
          let (r2, val2, _, _) = get_by_key (x-1, j, k, s) key in
          if val1[r1] == val2[r2]
          then -1
          else x 
      let unique_indices = filter (\i -> i != -1) (map border relevant_objects) in
      (unique_indices, j, k, s)
    case _ -> ([-1], [], [], [])

--Based on example from https://jqlang.org/manual/v1.8/#unique-unique_by
--Expected output: [1,3]
def unique_by_foo_example =
  let input = "[{\"foo\": 1, \"bar\": 2}, {\"foo\": 1, \"bar\": 3}, {\"foo\": 4, \"bar\": 5}]"
  let key = "foo"
  let JSE = parse_JSON input in
  let (rs, _, _, _) = (unique_by_key_val JSE key) in
  rs


--This functions behavior is undefined if keys of the specifed name are bound to values of #obj, #list, or #string
def group_by_key_val (JSE:JSON_environment) (key:[]u8) : JSON_environment =
  let (r:i64, j:[]JSON, k:[](i64, i64), s:[]u8) = JSE in
  if r == -1
  then (-1, [], [], [])
  else 
    match j[r]
    case #list (_, b) ->
      let (border_indices, _, _, _) = unique_by_key_val JSE key
      let to_group (idx:i64) : (i64,i64) =
        if border_indices[idx] == last border_indices
        then (border_indices[idx], b)
        else (border_indices[idx], border_indices[idx+1])
      let to_jlist (a':i64, b':i64) : JSON = #list (a', b')
      let groups = map to_jlist (map to_group (indices border_indices))
      let lenj = length j
      let new_root_value = [#list ((lenj + 1), (lenj + 1 + (length groups)))]
      let grouped_objs = concat new_root_value groups 
      let combined_json = concat j grouped_objs in
      (lenj, combined_json, k, s)
    case _ -> (-1, [], [], [])

--Based on example from https://jqlang.org/manual/v1.8/#group_by
--Expected output: "[[{\"foo\":1,\"bar\":10},{\"foo\":1,\"bar\":1}],[{\"foo\":3,\"bar\":100}]]"
def group_by_foo_example =
  let input = "[{\"foo\":1, \"bar\":10}, {\"foo\":3, \"bar\":100}, {\"foo\":1, \"bar\":1}]"
  let key = "foo"
  let JSE = parse_JSON input in
  print_JSON (group_by_key_val JSE key)



def max_by_key_val (JSE:JSON_environment) (key:[]u8) : JSON_environment =
  let (r:i64, j:[]JSON, k:[](i64, i64), s:[]u8) = JSE in 
  if r == -1 
    then (-1, [], [], [])
    else
      match j[r] 
      case #list (a, b) ->
        let max_obj_idx (x:i64) (y:i64) : i64 =
          if x == -1 
          then y
          else 
            if y == -1
            then x
            else
              let (xr, _, _, _) = get_by_key (x, j, k, s) key
              let (yr, _, _, _) = get_by_key (y, j, k, s) key in
              match (j[xr], j[yr])
              case (#num a, #num b) -> if a > b then x else y
              case (#num _, _) -> x
              case (_, #num _) -> y
              case _ -> -1
        let relevant_objects = a..<b
        let max_obj_idx = reduce max_obj_idx (-1) (relevant_objects) in
        if  max_obj_idx == -1 
        then (-1, [], [], [])
        else (max_obj_idx, j, k, s)   
      case _ -> (-1, [], [], [])

--Based on example from https://jqlang.org/manual/v1.8/#min-max-min_by-max_by
--Expected output: "{\"foo\":2,\"bar\":3}"
def max_by_foo_example =
  let input = "[{\"foo\":1, \"bar\":14}, {\"foo\":2, \"bar\":3}]"
  let key = "foo"
  let JSE = parse_JSON input in
  print_JSON (max_by_key_val JSE key)