import "lib/github.com/diku-dk/sorts/radix_sort"
import "json"

type production = parser.production
type node = parser.node terminal production 
type option 'a = #none | #some a

--advanced testjson for later: "[{\"foo\": 1, \"bar\": {\"baz\": true}},{\"qux\": [3, 4, 5]}]"

type JSON = #null | #num i64 | #bool bool | #string i64 | #list (i64, i64) | #obj (i64, i64) (i64, i64)

def testjson : []u8 = "[{\"foo\": \"test\"}, {\"bar\": 2}]"
--def testjson : []u8 = "[{\"foo\": 1, \"bar\": {\"baz\": true}},{\"qux\": [3, 4, 5]}]"

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

def preprocess_cst (js:option ([](i64, node))) : [](i64, node) =
    match js
    case #some json -> json
    case #none -> []

def cst_by_depth (ns:[](i64, node)) : [](i64, (i64, (i64, node))) =
    let track_depth (acc:(i64, (i64, node))) (n:(i64, (i64, node))) : (i64, (i64, node)) =
        match (acc, n)
        case (((depth, (_, #terminal #literal_4  (_, _))), (_, (parent, nde)))) -> 
                (depth+1, (parent, nde))
        case (((depth, (_, #terminal #literal_9 (_, _))), (_, (parent, nde)))) -> 
                (depth+1, (parent, nde))
        case (((depth, (_, #terminal #literal_5  (_, _))), (_, (parent, nde)))) -> 
                (depth-1, (parent, nde))
        case (((depth, (_, #terminal #literal_10 (_, _))), (_, (parent, nde)))) -> 
                (depth-1, (parent, nde))
        case ((depth, (_, _)), (_, (parent, nde))) -> 
                (depth, (parent, nde)) 
    in
    --let depths = scan track_depth (0, (0, #production (#Null))) (zip (indices ns) ns) in
    let depths = 
        loop (acc:((i64, (i64, node)), [](i64, (i64, node)))) = ((0, (0, #production (#Null))), []) for x in (zip (indices ns) ns) do
            let temp = track_depth acc.0 x in 
            (temp, concat acc.1 [temp])
    in
    --let isolate (dn:(i64, (i64, node))) : (i64, i64) = (dn.1.0, dn.0) in
    --let d = map isolate depths in
    let consolidate_parents (prev:(i64, (i64, (i64, node)))) (cur:(i64, (i64, (i64, node)))) : (i64, (i64, (i64, node))) =
        --If something breaks in the future, this function is probably at fault
        --the consolidation logic is kind of sketchy
        --(index, (depth, (parent, node)))
        match prev.1.1.1
        case (#terminal #literal_4 _) -> (cur.0, (cur.1.0, (prev.0, cur.1.1.1)))
        case (#terminal #literal_9 _) -> (cur.0, (cur.1.0, (prev.0, cur.1.1.1)))
        case _ ->
            if cur.1.0 == prev.1.0
            then (cur.0, (cur.1.0, (prev.1.1.0, cur.1.1.1))) 
            else (cur.0, (cur.1.0, (cur.1.1.0, cur.1.1.1))) 
    in
    --scan consolidate_parents (0, (0, (0, #production (#Null)))) (zip (indices depths.1) depths.1)
    let consolidated = 
        loop (acc:((i64, (i64, (i64, node))), [](i64, (i64, (i64, node))))) = ((0, (0, (0, #production (#Null)))), []) for x in (zip (indices depths.1) depths.1) do
            let temp = consolidate_parents acc.0 x in
            (temp, concat acc.1 [temp])
    in
    let get_key (n:(i64, (i64, (i64, node)))) = n.1.0 in
    radix_sort_by_key get_key i64.num_bits i64.get_bit consolidated.1

-- converts a []u8 (string) to a number
def s_to_num  (a:i64, b:i64) (s:[]u8) =
    let xs = reverse s[a:b] in
    loop acc = 0 for i < (b-a) do
        acc * 10 + (i64.u8 xs[(b-a) - 1 - i] - '0')


def sorted_cst_to_JSON (source:[]u8) (ns:[](i64, (i64, (i64, node)))) : ([]JSON, [](i64, i64)) =
    let i_str_keys : [](bool, (i64, (i64, i64))) =
        let match_spans (nde:(i64, (i64, (i64, node)))) : bool =
            match nde.1.1.1
            case (#terminal #string _) -> true
            case _ -> false
        in
        let construct_intermediate_key (n:(i64, (i64, (i64, node)))) : (bool, (i64, (i64, i64))) =
            match n.1.1
            case (p, #terminal #string (a, b)) ->
                if b >= length source then (false, (p, (a, b)))
                else if source[b] == 58 then (true, (p, (a, b)))
                else (false, (p, (a, b)))
            case _ -> (false, (-1, (-1, -1)))
        in
        map construct_intermediate_key (filter match_spans ns)
    in
    let match_relevant (n:(i64, (i64, (i64, node)))) : bool =
        --relevant: string, number, literal 4, literal 6, literal 7, literal 8, literal 9
        match n
        case (_, (_, (_, #production _))) -> false
        case (_, (_, (_, #terminal #string (_, b)))) -> 
            if b >= length source then true
            else if source[b] == 58 then false
            else true
        case (_, (_, (_, #terminal #number (_, _)))) -> true
        case (_, (_, (_, #terminal #literal_4 _))) -> true
        case (_, (_, (_, #terminal #literal_6 _))) -> true
        case (_, (_, (_, #terminal #literal_7 _))) -> true
        case (_, (_, (_, #terminal #literal_8 _))) -> true
        case (_, (_, (_, #terminal #literal_9 _))) -> true
        case _ -> false
    in
    let find_str_key (sp:(i64, i64)) : i64 =
        let is_str_key (x:i64) (ik_idx:i64) : i64 =
            if i_str_keys[ik_idx].1.1 == sp then ik_idx else x
        in
        reduce is_str_key (-1) (indices i_str_keys)
    in
    let find_obj_keys (parent:i64) : (i64, i64) =
        let obj_key_span (x:(i64, i64)) (ik_idxs:(i64, i64)) : (i64, i64) =
            if i_str_keys[ik_idxs.0].0 && i_str_keys[ik_idxs.0].1.0 == parent then
                (i64.min x.0 ik_idxs.0, i64.max x.1 ik_idxs.1)
            else x
        in
        let temp = reduce obj_key_span (i64.highest, -1) (zip (indices i_str_keys) (indices i_str_keys)) in
        if temp.1 == (-1) then (-1, -1)
        else (temp.0, temp.1 + 1)
    in
    let final_intermediate_value = filter match_relevant ns in
    --signature: (pre-sort index, (depth, (parent as pre-sort index, node)))
    let find_children (parent:i64) : (i64, i64) =
        let obj_child_span (x:(i64, i64)) (ik_idxs:(i64, i64)) : (i64, i64) =
            if final_intermediate_value[ik_idxs.0].1.1.0 == parent then
                (i64.min x.0 ik_idxs.0, i64.max x.1 ik_idxs.1)
            else x
        in
        let temp = reduce obj_child_span (i64.highest, -1) (zip (indices final_intermediate_value) (indices final_intermediate_value)) in
        if temp.1 == (-1) then (-1, -1)
        else (temp.0, temp.1 + 1)
    in
    --signature: pre-sort index, depth, parent as pre-sort index, node 
    let get_val (n:(i64, (i64, (i64, node)))) : JSON =
        match n
        case (_,   (_, (_, #terminal #string (a, b)))) -> #string (find_str_key (a, b))
        case (_,   (_, (_, #terminal #number n)))      -> #num (s_to_num n source) 
        case (idx, (_, (_, #terminal #literal_4 _)))   -> #list (find_children idx)
        case (_,   (_, (_, #terminal #literal_6 _)))   -> #bool false
        case (_,   (_, (_, #terminal #literal_7 _)))   -> #null
        case (_,   (_, (_, #terminal #literal_8 _)))   -> #bool true
        case (idx, (_, (_, #terminal #literal_9 _)))   -> #obj (find_obj_keys idx) (find_children idx)
        case _ -> #null
    in
    let str_keys : [](i64, i64) =
        let isolate_key (ik:(bool, (i64, (i64, i64)))) : (i64, i64) =
            (ik.1.1.0+1, ik.1.1.1-1)
        in
        map isolate_key i_str_keys
    in
    let final_json : []JSON =
        map get_val final_intermediate_value
    in
    (final_json, str_keys)


def read_val (s:[]u8) (x:node) : JSON =
    match x
    case (#terminal #number (a, b)) -> #num (s_to_num (a,b) s)
    case (#terminal #literal_6 _) -> #bool false
    case (#terminal #literal_8 _) -> #bool true
    case _ -> #null

def read_json (s:[]u8) (js:option ([](i64, node))) : []JSON =
    match js
    case #some json -> 
        let extract_val (x:(i64, node)) : JSON = 
            match x
            case (_, x) -> read_val s x 
        in  let is_not_null (y:JSON) : bool = 
                match y
                case #null -> false
                case _ -> true 
            in filter is_not_null (map extract_val json)
    case #none -> []

def main : ([]JSON, [](i64, i64)) = 
    let json = preprocess_cst (parse testjson) in
    --if null json then [] else get_intermediate_keys testjson json
    if null json then ([], []) else sorted_cst_to_JSON testjson (cst_by_depth json)