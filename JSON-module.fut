import "JSON-datatype"
import "JSON-utils"
import "JSON-examples"

module type json = {
  type~ env
  type~ arr_env
  val mk [n] : [n]u8 -> env
  val print : env -> []u8
  val unpack : env -> JSON_environment
  val repack [n] [m] [o] : (root, [n]JSON, [m]str, [o]u8) -> env
  val get_key [n] : env -> [n]u8 -> env
  val get_index : env -> i64 -> env
  val get_obj_keys : env -> env
  val get_obj_has_key [n] : env -> [n]u8 -> env
  val get_key_has_str [n] [m] : env -> [n]u8 -> [m]u8 -> arr_env
  val sort_by_key [n] : env -> [n]u8 -> env
  val get_unique_by_key [n] : env -> [n]u8 -> arr_env
  val group_by_key [n] : env -> [n]u8 -> env
  val get_max_by_key [n] : env -> [n]u8 -> env
  val get_ith_env : arr_env -> i64 -> env
}

module json : json = {
  type~ env = JSON_environment
  type~ arr_env = ([]root, []JSON, []str, []u8)
  def mk [n] (s:[n]u8) = parse_JSON s
  def print (JSE:env) = print_JSON JSE
  def unpack (JSE:env) : JSON_environment = JSE
  def repack (JSE:JSON_environment) : env = JSE
  def get_key [n] (JSE:env) (key:[n]u8) = get_by_key JSE key
  def get_index (JSE:env) (idx:i64) = get_by_index JSE idx
  def get_obj_keys (JSE:env) = get_keys JSE
  def get_obj_has_key [n] (JSE:env) (key:[n]u8) = map_has_key JSE key
  def get_key_has_str [n] [m] (JSE:env) (key:[n]u8) (value:[m]u8) = select_key_val_is_string JSE key value
  def sort_by_key [n] (JSE:env) (key:[n]u8) = sort_by_key_val JSE key
  def get_unique_by_key [n] (JSE:env) (key:[n]u8) = unique_by_key_val JSE key
  def group_by_key [n] (JSE:env) (key:[n]u8) = group_by_key_val JSE key
  def get_max_by_key [n] (JSE:env) (key:[n]u8) = max_by_key_val JSE key
  def get_ith_env (JSEarr:arr_env) (idx:i64) = 
    let (rs, j, k, s) = JSEarr in
    if idx >= length rs || idx < 0
    then (-1, [], [], [])
    else (rs[idx], j, k, s)
}