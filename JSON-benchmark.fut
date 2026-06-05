import "json"
import "JSON-datatype"
import "JSON-module"

entry parse_entry (s:[]u8) : [](i64, node) = preprocess_cst (parse s)

entry json_mk_entry (s:[]u8) : json.env = json.mk s 

-- ==
-- entry: parse_JSON_bench
-- script input { (parse_entry ($loadbytes "benchmark_data/json1.json"), $loadbytes "benchmark_data/json1.json") }
-- script input { (parse_entry ($loadbytes "benchmark_data/json2.json"), $loadbytes "benchmark_data/json2.json") }
-- script input { (parse_entry ($loadbytes "benchmark_data/json3.json"), $loadbytes "benchmark_data/json3.json") }
-- script input { (parse_entry ($loadbytes "benchmark_data/json4.json"), $loadbytes "benchmark_data/json4.json") }
-- script input { (parse_entry ($loadbytes "benchmark_data/json5.json"), $loadbytes "benchmark_data/json5.json") }
entry parse_JSON_bench (j:[](i64, node)) (s:[]u8) : i64 =
  let AST = (sorted_cst_to_JSON s (cst_by_depth j)).0 in
  match AST[0]
  case #null -> 0
  case #bool b -> if b then 1 else 2
  case #num n -> n
  case #string (_, b) -> b
  case #list (_, b) -> b
  case #obj (_, _) (_, d) -> d

-- ==
-- entry: print_bench
-- script input { json_mk_entry ($loadbytes "benchmark_data/json1.json") }
-- script input { json_mk_entry ($loadbytes "benchmark_data/json2.json") }
-- script input { json_mk_entry ($loadbytes "benchmark_data/json3.json") }
-- script input { json_mk_entry ($loadbytes "benchmark_data/json4.json") }
-- script input { json_mk_entry ($loadbytes "benchmark_data/json5.json") }
entry print_bench (JSE:json.env) : u8 =
  let s = json.print JSE in s[0]

--There are no benchmarks for any functions requiring an object as input, 
--since i could not figure out how to generate large objects

-- ==
-- entry: get_index_bench
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json1.json"), 0i64) }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json2.json"), 9i64) }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json3.json"), 99i64) }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json4.json"), 999i64) }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json5.json"), 9999i64) } 
entry get_index_bench (JSE:json.env) (idx:i64) : i64 =
  let res = json.get_index JSE idx in (json.unpack res).0

-- ==
-- entry: get_obj_has_key_bench
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json1.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json2.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json3.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json4.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json5.json"), "age") }
entry get_obj_has_key_bench (JSE:json.env) (key:[]u8) : i64 =
  let res = json.get_obj_has_key JSE key
  let (r, j, _, _) = (json.unpack res) in
  match j[r+1]
  case #bool b -> if b then 1 else 2 
  case _ -> -1

-- ==
-- entry: get_key_has_str_bench
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json1.json"), "favoriteFruit", "banana") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json2.json"), "favoriteFruit", "banana") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json3.json"), "favoriteFruit", "banana") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json4.json"), "favoriteFruit", "banana") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json5.json"), "favoriteFruit", "banana") }
entry get_key_has_str_bench (JSE:json.env) (key:[]u8) (value:[]u8) : i64 =
  let res = json.get_key_has_str JSE key value in (json.unpack (json.get_ith_env res 0)).0

-- ==
-- entry: sort_by_key_bench
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json1.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json2.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json3.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json4.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json5.json"), "age") }
entry sort_by_key_bench (JSE:json.env) (key:[]u8) : i64 =
  let res = json.sort_by_key JSE key in (json.unpack res).0

-- ==
-- entry: get_unique_by_key_bench
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json1.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json2.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json3.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json4.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json5.json"), "age") }
entry get_unique_by_key_bench (JSE:json.env) (key:[]u8) : i64 =
  let res = json.get_unique_by_key JSE key in (json.unpack (json.get_ith_env res 0)).0

-- ==
-- entry: group_by_key_bench
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json1.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json2.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json3.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json4.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json5.json"), "age") }
entry group_by_key_bench (JSE:json.env) (key:[]u8) : i64 =
  let res = json.group_by_key JSE key  
  let (r, j, _, _) = (json.unpack res) in
  match j[r]
  case #list (_, b) -> b
  case _ -> -1

-- ==
-- entry: get_max_by_key_bench
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json1.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json2.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json3.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json4.json"), "age") }
-- script input { (json_mk_entry ($loadbytes "benchmark_data/json5.json"), "age") }
entry get_max_by_key_bench (JSE:json.env) (key:[]u8) : i64 =
  let res = json.get_max_by_key JSE key in (json.unpack res).0