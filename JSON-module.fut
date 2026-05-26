import "JSON-datatype"
import "JSON-utils"
import "JSON-examples"

module type json = {
  type~ json_env
  type~ json_arr_env
  val mk [n] : [n]u8 -> json_env
  val print : json_env -> []u8
  val get_key [n] : json_env -> [n]u8 -> json_env
  val get_index : json_env -> i64 -> json_env
  val get_obj_keys : json_env -> json_env
  val get_obj_has_key [n] : json_env -> [n]u8 -> json_env
  val get_key_has_str [n] [m] : json_env -> [n]u8 -> [m]u8 -> json_arr_env
  val sort_by_key [n] : json_env -> [n]u8 -> json_env
  val get_unique_by_key [n] : json_env -> [n]u8 -> json_arr_env
  val group_by_key [n] : json_env -> [n]u8 -> json_env
  val get_max_by_key [n] : json_env -> [n]u8 -> json_env
}

module json : json = {
  type~ json_env = JSON_environment
  type~ json_arr_env = ([]root, []JSON, []str, []u8)
  def mk [n] (s:[n]u8) = parse_JSON s
  def print (JSE:json_env) = print_JSON JSE
  def get_key [n] (JSE:json_env) (key:[n]u8) = get_by_key JSE key
  def get_index (JSE:json_env) (idx:i64) = get_by_index JSE idx
  def get_obj_keys (JSE:json_env) = get_keys JSE
  def get_obj_has_key [n] (JSE:json_env) (key:[n]u8) = map_has_key JSE key
  def get_key_has_str [n] [m] (JSE:json_env) (key:[n]u8) (value:[m]u8) = select_key_val_is_string JSE key value
  def sort_by_key [n] (JSE:json_env) (key:[n]u8) = sort_by_key_val JSE key
  def get_unique_by_key [n] (JSE:json_env) (key:[n]u8) = unique_by_key_val JSE key
  def group_by_key [n] (JSE:json_env) (key:[n]u8) = group_by_key_val JSE key
  def get_max_by_key [n] (JSE:json_env) (key:[n]u8) = max_by_key_val JSE key
}